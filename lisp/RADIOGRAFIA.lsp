;;; ============================================================
;;; RADIOGRAFIA.lsp  v1
;;; RADIOGRAFIA : vuelca una caja (bloque) y su entorno en coordenadas
;;;               LOCALES del bloque -> lado (IZQUIERDA/DERECHA/ARRIBA/ABAJO)
;;; RADIOCENSO  : cuenta tipos de entidad, capas y bloques del dibujo
;;; Salida: archivo TXT junto al DWG (o en TEMP si no se puede escribir)
;;; ============================================================
(vl-load-com)

(setq rg:f nil
      rg:ins '(0.0 0.0 0.0)
      rg:ang 0.0
      rg:sx 1.0
      rg:sy 1.0
      rg:base '(0.0 0.0 0.0)
      rg:bb0 '(0.0 0.0)
      rg:bb1 '(1.0 1.0))

;;; ---------- utilidades ----------
(defun rg:w (s) (if rg:f (write-line s rg:f)) s)
(defun rg:n (x) (rtos x 2 3))
(defun rg:p (p) (strcat "(" (rg:n (car p)) ", " (rg:n (cadr p)) ")"))
(defun rg:g (c ed) (cdr (assoc c ed)))
(defun rg:gd (c ed d / v) (if (setq v (assoc c ed)) (cdr v) d))
(defun rg:deg (a) (* a (/ 180.0 pi)))
(defun rg:d2 (a b) (distance (list (car a) (cadr a)) (list (car b) (cadr b))))
(defun rg:join (pts) (apply 'strcat (mapcar '(lambda (p) (strcat (rg:p p) " ")) pts)))
(defun rg:verts (ed) (mapcar 'cdr (vl-remove-if-not '(lambda (x) (= (car x) 10)) ed)))
(defun rg:col (ed / c) (setq c (rg:gd 62 ed "PORCAPA")) (if (numberp c) (itoa c) c))

(defun rg:inc (key al / c)
  (if (setq c (assoc key al))
    (subst (cons key (1+ (cdr c))) c al)
    (cons (cons key 1) al)))

(defun rg:open (suffix / base path f)
  (setq base (strcat "RADIO" suffix "_" (vl-filename-base (getvar "DWGNAME")) ".txt")
        path (strcat (getvar "DWGPREFIX") base)
        f (open path "w"))
  (if (null f)
    (setq path (strcat (getenv "TEMP") "\\" base)
          f (open path "w")))
  (setq rg:f f)
  (if f path nil))

;;; ---------- geometria ----------
(defun rg:len (ed / pts s)
  (if (= (rg:g 0 ed) "LINE")
    (distance (rg:g 10 ed) (rg:g 11 ed))
    (progn
      (setq pts (rg:verts ed) s 0.0)
      (while (cdr pts)
        (setq s (+ s (rg:d2 (car pts) (cadr pts))) pts (cdr pts)))
      s)))

(defun rg:ends (ed / ty v)
  (setq ty (rg:g 0 ed))
  (cond
    ((= ty "LINE") (list (rg:g 10 ed) (rg:g 11 ed)))
    ((= ty "LWPOLYLINE")
     (setq v (rg:verts ed))
     (if (cdr v) (list (car v) (last v))))
    (T nil)))

;;; mundo -> coordenadas locales del bloque (independiente de rotacion/escala)
(defun rg:loc (p / dx dy ca sn)
  (setq dx (- (car p) (car rg:ins))
        dy (- (cadr p) (cadr rg:ins))
        ca (cos rg:ang)
        sn (sin rg:ang))
  (list (+ (/ (+ (* dx ca) (* dy sn)) rg:sx) (car rg:base))
        (+ (/ (+ (* (- dx) sn) (* dy ca)) rg:sy) (cadr rg:base))))

;;; lado mas cercano de la caja (negativo = fuera de la caja)
(defun rg:side (l / dl dr db dt m s)
  (setq dl (- (car l) (car rg:bb0))
        dr (- (car rg:bb1) (car l))
        db (- (cadr l) (cadr rg:bb0))
        dt (- (cadr rg:bb1) (cadr l))
        m (min dl dr db dt))
  (setq s (cond ((= m dl) "IZQUIERDA")
                ((= m dr) "DERECHA")
                ((= m db) "ABAJO")
                (T "ARRIBA")))
  (list s m))

;;; caja envolvente de la definicion del bloque
(defun rg:bbox (name / b e ed ty pts xs ys c r)
  (setq b (tblobjname "BLOCK" name))
  (if b
    (progn
      (setq e (entnext b))
      (while (and e (/= (setq ty (rg:g 0 (setq ed (entget e)))) "ENDBLK"))
        (cond
          ((member ty '("LINE" "SOLID" "TRACE" "POINT" "VERTEX"))
           (foreach x ed
             (if (member (car x) '(10 11 12 13))
               (setq pts (cons (cdr x) pts)))))
          ((= ty "LWPOLYLINE")
           (setq pts (append (rg:verts ed) pts)))
          ((member ty '("CIRCLE" "ARC"))
           (setq c (rg:g 10 ed) r (rg:g 40 ed))
           (setq pts (append (list (list (- (car c) r) (- (cadr c) r))
                                   (list (+ (car c) r) (+ (cadr c) r)))
                             pts))))
        (setq e (entnext e)))))
  (if pts
    (progn
      (setq xs (mapcar 'car pts) ys (mapcar 'cadr pts))
      (setq rg:bb0 (list (apply 'min xs) (apply 'min ys))
            rg:bb1 (list (apply 'max xs) (apply 'max ys))))
    (setq rg:bb0 '(0.0 0.0) rg:bb1 '(1.0 1.0)))
  pts)

;;; ---------- descripcion de entidades ----------
(defun rg:detail (ed / ty)
  (setq ty (rg:g 0 ed))
  (cond
    ((= ty "LINE")
     (strcat "de " (rg:p (rg:g 10 ed)) " a " (rg:p (rg:g 11 ed))
             " long=" (rg:n (distance (rg:g 10 ed) (rg:g 11 ed)))))
    ((= ty "LWPOLYLINE")
     (strcat "vertices=" (itoa (length (rg:verts ed)))
             (if (= 1 (logand 1 (rg:gd 70 ed 0))) " CERRADA " " abierta ")
             "long=" (rg:n (rg:len ed)) " : " (rg:join (rg:verts ed))))
    ((= ty "CIRCLE")
     (strcat "centro=" (rg:p (rg:g 10 ed)) " radio=" (rg:n (rg:g 40 ed))))
    ((= ty "ARC")
     (strcat "centro=" (rg:p (rg:g 10 ed)) " radio=" (rg:n (rg:g 40 ed))
             " ang=" (rg:n (rg:deg (rg:g 50 ed))) "->" (rg:n (rg:deg (rg:g 51 ed)))))
    ((= ty "TEXT")
     (strcat "texto='" (rg:gd 1 ed "") "' pos=" (rg:p (rg:g 10 ed))
             " alt=" (rg:n (rg:g 40 ed))
             " rot=" (rg:n (rg:deg (rg:gd 50 ed 0.0)))))
    ((= ty "MTEXT")
     (strcat "texto='" (rg:gd 1 ed "") "' pos=" (rg:p (rg:g 10 ed))
             " alt=" (rg:n (rg:g 40 ed))))
    ((= ty "INSERT")
     (strcat "bloque='" (rg:g 2 ed) "' pos=" (rg:p (rg:g 10 ed))
             " rot=" (rg:n (rg:deg (rg:gd 50 ed 0.0)))
             " esc=" (rg:n (rg:gd 41 ed 1.0)) "," (rg:n (rg:gd 42 ed 1.0))))
    ((member ty '("SOLID" "TRACE"))
     (strcat "puntos=" (rg:join (list (rg:g 10 ed) (rg:g 11 ed) (rg:g 12 ed) (rg:g 13 ed)))))
    ((= ty "ATTDEF")
     (strcat "tag=" (rg:g 2 ed) " pos=" (rg:p (rg:g 10 ed))))
    (T "(sin detalle)")))

(defun rg:keypts (ed / ty i)
  (setq ty (rg:g 0 ed))
  (cond
    ((= ty "LINE")
     (list (cons "ini" (rg:g 10 ed)) (cons "fin" (rg:g 11 ed))))
    ((= ty "LWPOLYLINE")
     (setq i 0)
     (mapcar '(lambda (p) (cons (strcat "v" (itoa (setq i (1+ i)))) p)) (rg:verts ed)))
    ((member ty '("TEXT" "MTEXT" "INSERT" "CIRCLE" "ARC"))
     (list (cons "pos" (rg:g 10 ed))))
    (T nil)))

(defun rg:locline (lbl p / l s w h)
  (setq l (rg:loc p)
        s (rg:side l)
        w (max (- (car rg:bb1) (car rg:bb0)) 1e-9)
        h (max (- (cadr rg:bb1) (cadr rg:bb0)) 1e-9))
  (strcat "    " lbl " mundo=" (rg:p p) " local=" (rg:p l)
          " lado=" (car s) " dist_borde=" (rg:n (cadr s))
          " u=" (rg:n (/ (- (car l) (car rg:bb0)) w))
          " v=" (rg:n (/ (- (cadr l) (cadr rg:bb0)) h))))

(defun rg:nearest (p txts / best bd d)
  (foreach tx txts
    (setq d (rg:d2 p (cadr tx)))
    (if (or (null bd) (< d bd))
      (setq bd d best tx)))
  (if best
    (strcat "'" (car best) "' d=" (rg:n bd))
    "(ninguno)"))

;;; ============================================================
;;; RADIOGRAFIA
;;; ============================================================
(defun c:RADIOGRAFIA ( / sel ref ed name w h tol ss i e items txts n ends
                         la lb sa sb pc po lc sc path r nv)
  (setq sel (entsel "\nSelecciona la CAJA (bloque de 8 salidas): "))
  (cond
    ((null sel) (princ "\nNo seleccionaste nada."))
    ((/= (rg:g 0 (setq ed (entget (car sel)))) "INSERT")
     (princ "\nEso no es un bloque (INSERT)."))
    (T
     (setq ref (car sel)
           name (rg:g 2 ed)
           rg:ins (rg:g 10 ed)
           rg:ang (rg:gd 50 ed 0.0)
           rg:sx (rg:gd 41 ed 1.0)
           rg:sy (rg:gd 42 ed 1.0)
           rg:base (cond ((cdr (assoc 10 (tblsearch "BLOCK" name))))
                         (T (list 0.0 0.0 0.0))))
     (if (equal rg:sx 0.0 1e-12) (setq rg:sx 1.0))
     (if (equal rg:sy 0.0 1e-12) (setq rg:sy 1.0))
     (rg:bbox name)
     (setq w (- (car rg:bb1) (car rg:bb0))
           h (- (cadr rg:bb1) (cadr rg:bb0))
           tol (* 0.15 (max w h)))
     (prompt "\nSelecciona las acometidas, textos F1/F2 y todo lo que rodea la caja: ")
     (setq ss (ssget))
     (if (null ss)
       (princ "\nSin seleccion.")
       (progn
         ;; separar entidades y textos
         (setq i 0 items nil txts nil)
         (repeat (sslength ss)
           (setq e (ssname ss i) i (1+ i))
           (if (not (equal e ref))
             (progn
               (setq ed (entget e) items (cons ed items))
               (if (member (rg:g 0 ed) '("TEXT" "MTEXT"))
                 (setq txts (cons (list (rg:gd 1 ed "") (rg:g 10 ed)) txts))))))
         (setq items (reverse items))
         (setq ed (entget ref))

         (setq path (rg:open "GRAFIA"))
         (if (null path)
           (princ "\nNo pude crear el archivo de salida.")
           (progn
             (rg:w "=== RADIOGRAFIA DWG ===")
             (rg:w (strcat "Dibujo: " (getvar "DWGNAME") "  Fecha: " (rtos (getvar "CDATE") 2 4)))
             (rg:w (strcat "INSUNITS=" (itoa (getvar "INSUNITS")) "  (coordenadas de mundo/WCS)"))
             (rg:w "")
             (rg:w "--- CAJA (bloque) ---")
             (rg:w (strcat "Nombre definicion: " name "  handle=" (rg:g 5 ed) "  capa=" (rg:g 8 ed)))
             (rg:w (strcat "Insercion=" (rg:p rg:ins) "  rotacion=" (rg:n (rg:deg rg:ang))
                           "  escala=" (rg:n rg:sx) "," (rg:n rg:sy)
                           (if (or (< rg:sx 0) (< rg:sy 0)) "  ESPEJADO" "")))
             (rg:w (strcat "Punto base=" (rg:p rg:base)))
             (rg:w (strcat "Caja local (min)=" (rg:p rg:bb0) " (max)=" (rg:p rg:bb1)
                           "  ancho=" (rg:n w) " alto=" (rg:n h) "  tol_contacto=" (rg:n tol)))
             ;; atributos
             (setq e (entnext ref))
             (while (and e (= (rg:g 0 (setq ed (entget e))) "ATTRIB"))
               (rg:w (strcat "  ATTRIB " (rg:g 2 ed) " = '" (rg:gd 1 ed "") "'"))
               (setq e (entnext e)))
             ;; bloque dinamico
             (setq r (vl-catch-all-apply
                       '(lambda ( / o)
                          (setq o (vlax-ename->vla-object ref))
                          (rg:w (strcat "Nombre efectivo: " (vla-get-effectivename o)))
                          (if (= (vla-get-isdynamicblock o) :vlax-true)
                            (foreach p (vlax-safearray->list
                                         (vlax-variant-value (vla-getdynamicblockproperties o)))
                              (rg:w (strcat "  DYN " (vla-get-propertyname p) " = "
                                            (vl-princ-to-string (vlax-variant-value (vla-get-value p))))))))))
             ;; definicion
             (rg:w "")
             (rg:w "--- Definicion del bloque (coordenadas locales) ---")
             (setq e (entnext (tblobjname "BLOCK" name)))
             (while (and e (/= (rg:g 0 (setq ed (entget e))) "ENDBLK"))
               (rg:w (strcat "  " (rg:g 0 ed) " capa=" (rg:g 8 ed) " color=" (rg:col ed)
                             " " (rg:detail ed)))
               (setq e (entnext e)))

             ;; entidades
             (rg:w "")
             (rg:w "--- Entidades seleccionadas (con posicion LOCAL respecto a la caja) ---")
             (setq n 0)
             (foreach ed items
               (setq n (1+ n))
               (rg:w (strcat "[" (itoa n) "] " (rg:g 0 ed) " handle=" (rg:g 5 ed)
                             " capa=" (rg:g 8 ed) " color=" (rg:col ed)
                             " tipolinea=" (rg:gd 6 ed "PORCAPA")))
               (rg:w (strcat "    " (rg:detail ed)))
               (foreach kp (rg:keypts ed)
                 (rg:w (rg:locline (car kp) (cdr kp)))))

             ;; relaciones
             (rg:w "")
             (rg:w "--- Relaciones (una linea por acometida) ---")
             (rg:w "REL|handle|tipo|capa|color|lado_contacto|dist_borde|toca|u|v|texto_cerca_contacto|texto_cerca_otro_extremo|vertices|long_total")
             (foreach ed items
               (setq ends (rg:ends ed))
               (if ends
                 (progn
                   (setq la (rg:loc (car ends)) lb (rg:loc (cadr ends))
                         sa (rg:side la) sb (rg:side lb))
                   (if (<= (abs (cadr sa)) (abs (cadr sb)))
                     (setq pc (car ends) po (cadr ends) lc la sc sa)
                     (setq pc (cadr ends) po (car ends) lc lb sc sb))
                   (setq nv (if (= (rg:g 0 ed) "LINE") 2 (length (rg:verts ed))))
                   (rg:w (strcat "REL|" (rg:g 5 ed) "|" (rg:g 0 ed) "|" (rg:g 8 ed) "|" (rg:col ed)
                                 "|" (car sc) "|" (rg:n (cadr sc))
                                 "|" (if (<= (abs (cadr sc)) tol) "SI" "NO")
                                 "|" (rg:n (/ (- (car lc) (car rg:bb0)) (max w 1e-9)))
                                 "|" (rg:n (/ (- (cadr lc) (cadr rg:bb0)) (max h 1e-9)))
                                 "|" (rg:nearest pc txts)
                                 "|" (rg:nearest po txts)
                                 "|" (itoa nv)
                                 "|" (rg:n (rg:len ed)))))))

             (close rg:f)
             (setq rg:f nil)
             (princ (strcat "\nListo. Archivo generado:\n" path
                            "\nAbrelo y pega su contenido en el chat."))))))))
  (princ))

;;; ============================================================
;;; RADIOCENSO : inventario de todo el modelo
;;; ============================================================
(defun c:RADIOCENSO ( / ss i ed ty tl ll bl path)
  (setq ss (ssget "X" (list (cons 410 "Model"))))
  (if (null ss)
    (princ "\nNo hay entidades en el modelo.")
    (progn
      (setq i 0)
      (repeat (sslength ss)
        (setq ed (entget (ssname ss i)) i (1+ i) ty (rg:g 0 ed))
        (setq tl (rg:inc ty tl)
              ll (rg:inc (strcat (rg:g 8 ed) " | " ty) ll))
        (if (= ty "INSERT")
          (setq bl (rg:inc (rg:g 2 ed) bl))))
      (setq path (rg:open "CENSO"))
      (if (null path)
        (princ "\nNo pude crear el archivo de salida.")
        (progn
          (rg:w (strcat "=== CENSO de " (getvar "DWGNAME") " (espacio modelo) ==="))
          (rg:w "--- Tipos de entidad ---")
          (foreach x (vl-sort tl '(lambda (a b) (> (cdr a) (cdr b))))
            (rg:w (strcat (itoa (cdr x)) "  " (car x))))
          (rg:w "--- Capa | Tipo ---")
          (foreach x (vl-sort ll '(lambda (a b) (> (cdr a) (cdr b))))
            (rg:w (strcat (itoa (cdr x)) "  " (car x))))
          (rg:w "--- Bloques insertados ---")
          (foreach x (vl-sort bl '(lambda (a b) (> (cdr a) (cdr b))))
            (rg:w (strcat (itoa (cdr x)) "  " (car x))))
          (close rg:f)
          (setq rg:f nil)
          (princ (strcat "\nListo. Archivo generado:\n" path))))))
  (princ))

(princ "\nRADIOGRAFIA.lsp cargado. Comandos: RADIOGRAFIA  RADIOCENSO")
(princ)
