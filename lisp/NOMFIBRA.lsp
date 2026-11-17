;;; ===========================================================================
;;; NOMFIBRA.lsp
;;; Comando     : NOMFIBRA
;;; Version     : 1.0
;;; Fecha       : 2026-10-16
;;; Descripcion : Nomenclatura automatica de la fibra de 24 hilos en un
;;;               grupo de servicio (FTTH).
;;;   - F1 = continuidad: ruta desde el divisor de INICIO que conecta MAS
;;;          divisores de 1er nivel (desempate: la mas larga), o bien solo
;;;          la ruta mas larga, segun el criterio elegido en el dialogo.
;;;   - F2, F3, F4... = derivadas: ramas que salen de los cierres/nodos que
;;;          estan sobre la F1. Se numeran desde la mas lejana hacia el
;;;          divisor de inicio (o al reves), en forma corrida en todo el
;;;          grupo o reiniciando en F2 en cada cierre.
;;;   - La etiqueta se coloca junto a cada caja divisor (bloque elegido),
;;;     sobre el tramo de salida.
;;;   Cuadro de dialogo con listas desplegables (capa, bloque, reglas) que
;;;   recuerda la ultima eleccion durante la sesion.
;;;
;;;   NO modifica la fibra ni los bloques. Solo AGREGA textos en una capa
;;;   propia (y opcionalmente una polilinea que resalta la F1). Si activas
;;;   "Borrar etiquetas previas", BORRA los textos de esa capa (y la ruta
;;;   F1 anterior) en el espacio actual. Todo queda en un solo UNDO.
;;;
;;;   Supuestos: la fibra de 24h esta en UNA capa (LINE, ARC, LWPOLYLINE,
;;;   POLYLINE 2D/3D); los tramos se unen por extremos/vertices o tocando
;;;   el contorno del bloque divisor. La red se trata como arbol (sin anillos).
;;; ===========================================================================

(vl-load-com)

;;; ------------------------- CONFIGURACION ----------------------------------
(setq NF:TOL        0.10)  ; tolerancia para unir extremos de tramos (unid. dibujo)
(setq NF:MARGEN     0.50)  ; margen alrededor del bloque divisor para "pegar" la fibra
(setq NF:CELDA      5.0)   ; celda del indice espacial (debe ser > NF:TOL)
(setq NF:DEC        2)     ; decimales en reporte y CSV
(setq NF:ALINEAR    nil)   ; T = texto alineado al tramo ; nil = texto horizontal
(setq NF:F1_CADA_DIV T)    ; T = poner "F1" en cada divisor de la continuidad
(setq NF:FN_CADA_DIV T)    ; T = repetir F2/F3.. en cada divisor dentro de su derivada
(setq NF:MAX_DER    0)     ; max. derivadas por cierre (0 = sin limite, solo avisa)
(setq NF:BLQ_EXTRA  '())   ; otros nombres de bloque que tambien cuentan como
                           ; divisor 1er nivel, ej. '("CAJA_8S_A" "CAJA_8S_B")
(setq NF:COLOR_ET   2)     ; color de la capa de etiquetas (2 = amarillo)
(setq NF:CAPA_RUTA  "NOM_F1_RUTA") ; capa de la polilinea que resalta la F1
(setq NF:COLOR_RUTA 1)     ; color de la ruta F1 (1 = rojo)
(setq NF:ANCHO_RUTA 0.30)  ; ancho global de la polilinea de ruta F1
(setq NF:SEP        ",")   ; separador del CSV

;;; Valores por defecto del dialogo (se recuerdan en la sesion)
(if (not *NF_CRIT*)   (setq *NF_CRIT*   "0"))
(if (not *NF_ORDEN*)  (setq *NF_ORDEN*  "0"))
(if (not *NF_NUMER*)  (setq *NF_NUMER*  "0"))
(if (not *NF_PREF*)   (setq *NF_PREF*   "F"))
(if (not *NF_ALT*)    (setq *NF_ALT*    "1.5"))
(if (not *NF_CAPAET*) (setq *NF_CAPAET* "NOM_FIBRA"))
(if (not *NF_RUTA*)   (setq *NF_RUTA*   "1"))
(if (not *NF_BORRAR*) (setq *NF_BORRAR* "0"))
(if (not *NF_CSV*)    (setq *NF_CSV*    "1"))
;;; ---------------------------------------------------------------------------

;;; ======================= UTILIDADES GENERALES ===============================
(defun nf:floor (x) (if (and (< x 0.0) (/= x (fix x))) (1- (fix x)) (fix x)))
(defun nf:cell (p) (list (nf:floor (/ (car p) NF:CELDA)) (nf:floor (/ (cadr p) NF:CELDA))))
(defun nf:p2 (p) (list (car p) (cadr p)))

;; agrega val a la lista asociada a key en una lista de asociacion
(defun nf:addto (key val lst / e)
  (if (setq e (assoc key lst))
    (subst (cons key (cons val (cdr e))) e lst)
    (cons (list key val) lst)))

(defun nf:pt   (id) (cdr (assoc id *nf-pts*)))
(defun nf:dist (id) (cdr (assoc id *nf-dist*)))
(defun nf:par  (id) (cdr (assoc id *nf-par*)))
(defun nf:isdiv (id) (if (assoc id *nf-box*) T nil))

(defun nf:halfbox (id / b)
  (if (setq b (cdr (assoc id *nf-box*)))
    (* 0.5 (max (- (nth 2 b) (nth 0 b)) (- (nth 3 b) (nth 1 b))))
    0.0))

(defun nf:capa (nom col)
  (if (not (tblsearch "LAYER" nom))
    (entmake (list '(0 . "LAYER") '(100 . "AcDbSymbolTableRecord")
                   '(100 . "AcDbLayerTableRecord") (cons 2 nom) '(70 . 0)
                   (cons 62 col) '(6 . "Continuous")))))

(defun nf:tabla (tb / r l n)
  (setq r (tblnext tb T))
  (while r
    (setq n (cdr (assoc 2 r)))
    (if (and (not (wcmatch n "`**,*|*"))
             (or (/= tb "BLOCK") (= 0 (logand 4 (cdr (assoc 70 r))))))
      (setq l (cons n l)))
    (setq r (tblnext tb)))
  (acad_strlsort l))

(defun nf:efname (en / r)
  (setq r (vl-catch-all-apply 'vla-get-EffectiveName (list (vlax-ename->vla-object en))))
  (if (vl-catch-all-error-p r) (cdr (assoc 2 (entget en))) r))

(defun nf:esdiv (en) (if (member (strcase (nf:efname en)) *nf-divnames*) T nil))

;;; ======================= CONSTRUCCION DEL GRAFO =============================
;; registra un bloque divisor como nodo, con su caja envolvente
(defun nf:add-div (en / o mn mx p1 p2 id x0 x1 y0 y1 ix iy)
  (setq o (vlax-ename->vla-object en))
  (vla-GetBoundingBox o 'mn 'mx)
  (setq p1 (vlax-safearray->list mn) p2 (vlax-safearray->list mx))
  (setq *nf-nid* (1+ *nf-nid*) id *nf-nid*)
  (setq *nf-pts* (cons (cons id (list (/ (+ (car p1) (car p2)) 2.0)
                                      (/ (+ (cadr p1) (cadr p2)) 2.0)))
                       *nf-pts*))
  (setq *nf-box* (cons (list id (car p1) (cadr p1) (car p2) (cadr p2)) *nf-box*))
  (setq *nf-divent* (cons (cons en id) *nf-divent*))
  (setq x0 (nf:floor (/ (- (car p1) NF:MARGEN) NF:CELDA))
        x1 (nf:floor (/ (+ (car p2) NF:MARGEN) NF:CELDA))
        y0 (nf:floor (/ (- (cadr p1) NF:MARGEN) NF:CELDA))
        y1 (nf:floor (/ (+ (cadr p2) NF:MARGEN) NF:CELDA))
        ix x0)
  (while (<= ix x1)
    (setq iy y0)
    (while (<= iy y1)
      (setq *nf-cgrid* (nf:addto (list ix iy) id *nf-cgrid*))
      (setq iy (1+ iy)))
    (setq ix (1+ ix)))
  id)

(defun nf:inbox (p id / b)
  (setq b (cdr (assoc id *nf-box*)))
  (and (>= (car p)  (- (nth 0 b) NF:MARGEN)) (<= (car p)  (+ (nth 2 b) NF:MARGEN))
       (>= (cadr p) (- (nth 1 b) NF:MARGEN)) (<= (cadr p) (+ (nth 3 b) NF:MARGEN))))

;; devuelve el nodo para el punto p: divisor que lo contiene, nodo existente
;; a menos de NF:TOL, o un nodo nuevo
(defun nf:getnode (p / c best bd d)
  (setq c (nf:cell p))
  (foreach cid (cdr (assoc c *nf-cgrid*))
    (if (and (not best) (nf:inbox p cid)) (setq best cid)))
  (if (not best)
    (progn
      (setq bd NF:TOL)
      (foreach dc '((-1 -1) (-1 0) (-1 1) (0 -1) (0 0) (0 1) (1 -1) (1 0) (1 1))
        (foreach id (cdr (assoc (list (+ (car c) (car dc)) (+ (cadr c) (cadr dc))) *nf-grid*))
          (if (<= (setq d (distance p (nf:pt id))) bd) (setq bd d best id))))
      (if (not best)
        (progn
          (setq *nf-nid* (1+ *nf-nid*) best *nf-nid*)
          (setq *nf-pts* (cons (cons best p) *nf-pts*))
          (setq *nf-grid* (nf:addto c best *nf-grid*))))))
  best)

(defun nf:add-edge (pa pb w / a b)
  (setq a (nf:getnode (nf:p2 pa)) b (nf:getnode (nf:p2 pb)))
  (if (/= a b) (setq *nf-edges* (cons (list a b w) *nf-edges*))))

(defun nf:add-curve (en / ty i ep)
  (setq ty (cdr (assoc 0 (entget en))))
  (if (member ty '("LINE" "ARC"))
    (nf:add-edge (vlax-curve-getStartPoint en) (vlax-curve-getEndPoint en)
                 (vlax-curve-getDistAtParam en (vlax-curve-getEndParam en)))
    (progn
      (setq i 0 ep (fix (+ (vlax-curve-getEndParam en) 1e-9)))
      (while (< i ep)
        (nf:add-edge (vlax-curve-getPointAtParam en i)
                     (vlax-curve-getPointAtParam en (1+ i))
                     (- (vlax-curve-getDistAtParam en (1+ i))
                        (vlax-curve-getDistAtParam en i)))
        (setq i (1+ i))))))

;; distancia de p al segmento a-b y parametro t (0..1) de la proyeccion
(defun nf:seg-dist (p a b / dx dy l2 tt q)
  (setq dx (- (car b) (car a)) dy (- (cadr b) (cadr a))
        l2 (+ (* dx dx) (* dy dy))
        tt (if (> l2 1e-12)
             (/ (+ (* (- (car p) (car a)) dx) (* (- (cadr p) (cadr a)) dy)) l2)
             0.0)
        tt (max 0.0 (min 1.0 tt))
        q  (list (+ (car a) (* tt dx)) (+ (cadr a) (* tt dy))))
  (cons (distance p q) tt))

;; divisores atravesados por un tramo sin vertice dentro: se parte el tramo
(defun nf:split-huerfanos (/ usados id c lim best bd tb r)
  (foreach e *nf-edges* (setq usados (cons (car e) (cons (cadr e) usados))))
  (foreach dv *nf-box*
    (setq id (car dv))
    (if (not (member id usados))
      (progn
        (setq c   (nf:pt id)
              lim (+ (* 0.5 (distance (list (nth 1 dv) (nth 2 dv))
                                      (list (nth 3 dv) (nth 4 dv))))
                     NF:MARGEN)
              best nil bd lim)
        (foreach e *nf-edges*
          (setq r (nf:seg-dist c (nf:pt (car e)) (nf:pt (cadr e))))
          (if (and (<= (car r) bd) (> (cdr r) 0.0) (< (cdr r) 1.0))
            (setq bd (car r) best e tb (cdr r))))
        (if best
          (setq *nf-edges*
                 (cons (list (car best) id (* (caddr best) tb))
                       (cons (list id (cadr best) (* (caddr best) (- 1.0 tb)))
                             (vl-remove best *nf-edges*)))))))))

;;; ======================= RECORRIDO DEL ARBOL ================================
(defun nf:bfs (root / cur nxt)
  (setq *nf-par*   (list (cons root -1))
        *nf-dist*  (list (cons root 0.0))
        *nf-dc*    (list (cons root (if (nf:isdiv root) 1 0)))
        *nf-order* (list root)
        cur        (list root))
  (while cur
    (setq nxt nil)
    (foreach n cur
      (foreach nb (cdr (assoc n *nf-adj*))
        (if (not (assoc (car nb) *nf-par*))
          (setq *nf-par*   (cons (cons (car nb) n) *nf-par*)
                *nf-dist*  (cons (cons (car nb) (+ (nf:dist n) (cdr nb))) *nf-dist*)
                *nf-dc*    (cons (cons (car nb) (+ (cdr (assoc n *nf-dc*))
                                                   (if (nf:isdiv (car nb)) 1 0)))
                                 *nf-dc*)
                *nf-order* (cons (car nb) *nf-order*)
                nxt        (cons (car nb) nxt)))))
    (setq cur nxt))
  ;; hijos
  (foreach pr *nf-par*
    (if (/= (cdr pr) -1) (setq *nf-ch* (nf:addto (cdr pr) (car pr) *nf-ch*))))
  ;; distancia maxima alcanzable por debajo de cada nodo (orden inverso BFS)
  (foreach n *nf-order*
    (setq f (nf:dist n))
    (foreach k (cdr (assoc n *nf-ch*))
      (setq f (max f (cdr (assoc k *nf-far*)))))
    (setq *nf-far* (cons (cons n f) *nf-far*))))

(defun nf:mainchild (n / best bf f)
  (foreach k (cdr (assoc n *nf-ch*))
    (setq f (cdr (assoc k *nf-far*)))
    (if (or (not bf) (> f bf)) (setq bf f best k)))
  best)

(defun nf:subtree (c / st n out)
  (setq st (list c))
  (while st
    (setq n (car st) st (cdr st) out (cons n out))
    (foreach k (cdr (assoc n *nf-ch*)) (setq st (cons k st))))
  out)

;;; ======================= ETIQUETAS ==========================================
(defun nf:rotleg (a)
  (cond ((not NF:ALINEAR) 0.0)
        ((and (> a (/ pi 2.0)) (<= a (* 1.5 pi))) (+ a pi))
        (T a)))

;; texto junto al nodo a, sobre el tramo a->b
(defun nf:etiqueta (a b txt / pa pb ang d p p3)
  (if (and a b (/= b -1))
    (progn
      (setq pa  (nf:pt a) pb (nf:pt b) ang (angle pa pb)
            d   (min (+ (nf:halfbox a) (* 1.5 *nf-h*)) (* 0.5 (distance pa pb)))
            p   (polar (polar pa ang d) (+ ang (/ pi 2.0)) (* 1.2 *nf-h*))
            p3  (list (car p) (cadr p) 0.0))
      (entmake (list '(0 . "TEXT") (cons 8 *nf-capet*) (cons 10 p3)
                     (cons 40 *nf-h*) (cons 1 txt) (cons 50 (nf:rotleg ang))
                     (cons 7 (getvar "TEXTSTYLE")) '(72 . 1) (cons 11 p3) '(73 . 2)))
      (setq *nf-netq* (1+ *nf-netq*)))))

(defun nf:etq-subarbol (nodos txt / nx)
  (foreach m nodos
    (if (nf:isdiv m)
      (if (setq nx (nf:mainchild m))
        (nf:etiqueta m nx txt)
        (nf:etiqueta m (nf:par m) txt)))))

;;; ======================= DIALOGO (DCL) ======================================
(defun nf:dcl-ok (/ h)
  (setq h (distof (get_tile "alt")))
  (cond
    ((or (not h) (<= h 0.0)) (set_tile "msg" "Altura de texto no valida."))
    ((= (vl-string-trim " " (get_tile "pref")) "") (set_tile "msg" "Falta el prefijo."))
    ((not (snvalid (get_tile "capaet"))) (set_tile "msg" "Nombre de capa de etiquetas no valido."))
    (T
     (setq *NF_CAPA*   (nth (atoi (get_tile "capa")) capas)
           *NF_BLQ*    (nth (atoi (get_tile "blq")) blqs)
           *NF_CRIT*   (get_tile "crit")
           *NF_ORDEN*  (get_tile "orden")
           *NF_NUMER*  (get_tile "numer")
           *NF_PREF*   (vl-string-trim " " (get_tile "pref"))
           *NF_ALT*    (get_tile "alt")
           *NF_CAPAET* (get_tile "capaet")
           *NF_RUTA*   (get_tile "ruta")
           *NF_BORRAR* (get_tile "borrar")
           *NF_CSV*    (get_tile "csv"))
     (done_dialog 1))))

(defun nf:popup (key lst sel)
  (start_list key) (mapcar 'add_list lst) (end_list)
  (set_tile key (itoa (cond ((vl-position sel lst)) (0)))))

(defun nf:dialogo (/ f lns res)
  (setq dfile (vl-filename-mktemp "nomfib.dcl")
        lns '("nomfibra : dialog { label = \"NOMFIBRA - Nomenclatura fibra 24h (F1 / F2...)\";"
              "  : boxed_column { label = \"Datos del dibujo\";"
              "    : popup_list { key = \"capa\";  label = \"Capa fibra 24h:\";          width = 60; }"
              "    : popup_list { key = \"blq\";   label = \"Bloque divisor 1er nivel:\"; width = 60; }"
              "  }"
              "  : boxed_column { label = \"Reglas\";"
              "    : popup_list { key = \"crit\";  label = \"Criterio F1:\";      width = 60; }"
              "    : popup_list { key = \"orden\"; label = \"Orden derivadas:\";  width = 60; }"
              "    : popup_list { key = \"numer\"; label = \"Numeracion:\";       width = 60; }"
              "  }"
              "  : boxed_column { label = \"Etiquetas\";"
              "    : edit_box { key = \"pref\";   label = \"Prefijo:\";           edit_width = 8; }"
              "    : edit_box { key = \"alt\";    label = \"Altura de texto:\";   edit_width = 8; }"
              "    : edit_box { key = \"capaet\"; label = \"Capa de etiquetas:\"; edit_width = 24; }"
              "    : toggle { key = \"ruta\";   label = \"Dibujar ruta F1 resaltada\"; }"
              "    : toggle { key = \"borrar\"; label = \"Borrar etiquetas previas de esa capa (espacio actual)\"; }"
              "    : toggle { key = \"csv\";    label = \"Exportar resumen CSV junto al DWG\"; }"
              "  }"
              "  ok_cancel;"
              "  : text { key = \"msg\"; }"
              "}"))
  (setq f (open dfile "w"))
  (foreach l lns (write-line l f))
  (close f)
  (setq dcl (load_dialog dfile))
  (if (not (new_dialog "nomfibra" dcl))
    (progn (unload_dialog dcl) (setq dcl nil) (vl-file-delete dfile) nil)
    (progn
      (nf:popup "capa" capas *NF_CAPA*)
      (nf:popup "blq" blqs *NF_BLQ*)
      (start_list "crit")
      (mapcar 'add_list '("Mas divisores de 1er nivel (desempate: la mas larga)" "Ruta mas larga"))
      (end_list)
      (start_list "orden")
      (mapcar 'add_list '("Desde la mas lejana hacia el divisor de inicio" "Desde el divisor de inicio hacia la mas lejana"))
      (end_list)
      (start_list "numer")
      (mapcar 'add_list '("Corrida en todo el grupo (F2, F3, F4...)" "Por cierre (reinicia en F2 en cada cierre)"))
      (end_list)
      (set_tile "crit" *NF_CRIT*)
      (set_tile "orden" *NF_ORDEN*)
      (set_tile "numer" *NF_NUMER*)
      (set_tile "pref" *NF_PREF*)
      (set_tile "alt" *NF_ALT*)
      (set_tile "capaet" *NF_CAPAET*)
      (set_tile "ruta" *NF_RUTA*)
      (set_tile "borrar" *NF_BORRAR*)
      (set_tile "csv" *NF_CSV*)
      (action_tile "accept" "(nf:dcl-ok)")
      (action_tile "cancel" "(done_dialog 0)")
      (setq res (start_dialog))
      (unload_dialog dcl) (setq dcl nil)
      (vl-file-delete dfile) (setq dfile nil)
      (= res 1))))

;;; ======================= COMANDO PRINCIPAL ==================================
(defun c:NOMFIBRA (/ *error* doc oce dcl dfile capas blqs s root-en ss i en ed ty curvas
                     *nf-divnames* *nf-pts* *nf-box* *nf-divent* *nf-grid* *nf-cgrid*
                     *nf-edges* *nf-adj* *nf-par* *nf-dist* *nf-dc* *nf-order* *nf-ch*
                     *nf-far* *nf-nid* *nf-h* *nf-capet* *nf-netq*
                     root crit lejano porcierre pref target bk bd n dc f path ders
                     k prevn cuenta nombre nodos len ndv res usados sinc fuera
                     rest nx sel csvf fh lin f1len f1dc)

  (defun *error* (m)
    (if dcl (unload_dialog dcl))
    (if (and dfile (findfile dfile)) (vl-file-delete dfile))
    (if doc (vla-EndUndoMark doc))
    (if oce (setvar "CMDECHO" oce))
    (if (and m (not (wcmatch (strcase m) "*CANCEL*,*QUIT*,*BREAK*,*EXIT*")))
      (princ (strcat "\nError: " m)))
    (princ))

  (setq oce (getvar "CMDECHO"))
  (setvar "CMDECHO" 0)

  ;; ---- listas para el dialogo
  (setq capas (nf:tabla "LAYER") blqs (nf:tabla "BLOCK"))
  (if (not blqs) (progn (princ "\nEl dibujo no tiene bloques definidos.") (exit)))
  (if (not (nf:dialogo)) (progn (princ "\nCancelado.") (exit)))

  (setq *nf-divnames* (mapcar 'strcase (cons *NF_BLQ* NF:BLQ_EXTRA))
        *nf-h*        (distof *NF_ALT*)
        *nf-capet*    *NF_CAPAET*
        *nf-netq*     0
        *nf-nid*      0
        crit          (atoi *NF_CRIT*)
        lejano        (= *NF_ORDEN* "0")
        porcierre     (= *NF_NUMER* "1")
        pref          *NF_PREF*)

  ;; ---- divisor de inicio
  (while (not root-en)
    (setq s (entsel "\nSelecciona el divisor de INICIO (donde entra la fibra de 24h): "))
    (cond
      ((not s) (princ "\nCancelado.") (exit))
      ((and (= (cdr (assoc 0 (entget (car s)))) "INSERT") (nf:esdiv (car s)))
       (setq root-en (car s)))
      (T (princ (strcat "\nNo es el bloque divisor elegido (" *NF_BLQ* ").")))))

  ;; ---- area de trabajo
  (princ "\nSelecciona el area del grupo de servicio <Enter = todo el espacio actual>: ")
  (setq ss (ssget '((0 . "LINE,ARC,LWPOLYLINE,POLYLINE,INSERT"))))
  (if (not ss)
    (setq ss (ssget "_X" (list '(0 . "LINE,ARC,LWPOLYLINE,POLYLINE,INSERT")
                               (cons 410 (getvar "CTAB"))))))
  (if (not ss) (progn (princ "\nNo hay objetos.") (exit)))

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (vla-StartUndoMark doc)

  ;; ---- leer divisores primero, luego fibra
  (princ "\nLeyendo dibujo...")
  (repeat (setq i (sslength ss))
    (setq en (ssname ss (setq i (1- i))) ed (entget en) ty (cdr (assoc 0 ed)))
    (cond
      ((= ty "INSERT") (if (nf:esdiv en) (nf:add-div en)))
      ((/= (strcase (cdr (assoc 8 ed))) (strcase *NF_CAPA*)) nil)
      ((and (= ty "POLYLINE") (/= 0 (logand 80 (cdr (assoc 70 ed))))) nil)
      (T (setq curvas (cons en curvas)))))
  (if (not (assoc root-en *nf-divent*)) (nf:add-div root-en))
  (setq root (cdr (assoc root-en *nf-divent*)))
  (if (not curvas)
    (progn (princ (strcat "\nNo hay tramos en la capa " *NF_CAPA* " dentro de la seleccion."))
           (exit)))
  (foreach c curvas (nf:add-curve c))
  (nf:split-huerfanos)

  ;; ---- adyacencia
  (foreach e *nf-edges*
    (setq *nf-adj* (nf:addto (car e)  (cons (cadr e) (caddr e)) *nf-adj*)
          *nf-adj* (nf:addto (cadr e) (cons (car e)  (caddr e)) *nf-adj*)
          usados   (cons (car e) (cons (cadr e) usados))))
  (if (not (assoc root *nf-adj*))
    (progn (princ "\nEl divisor de inicio no toca la fibra. Revisa NF:MARGEN o la capa.")
           (exit)))

  ;; ---- recorrido y eleccion de F1
  (nf:bfs root)
  (setq bk -1 bd -1.0)
  (foreach n *nf-order*
    (setq dc (cdr (assoc n *nf-dc*)) f (nf:dist n))
    (if (if (= crit 0)
          (or (> dc bk) (and (= dc bk) (> f bd)))
          (> f bd))
      (setq bk dc bd f target n)))
  (setq n target)
  (while (/= n -1) (setq path (cons n path) n (nf:par n)))
  (if (< (length path) 2) (progn (princ "\nNo se encontro continuidad desde el inicio.") (exit)))
  (setq f1len (nf:dist target) f1dc (cdr (assoc target *nf-dc*)))

  ;; ---- derivadas: hijos de nodos F1 que no pertenecen a F1
  (foreach n path
    (foreach k (cdr (assoc n *nf-ch*))
      (if (not (member k path))
        (setq ders (cons (list n k (nf:dist n) (angle (nf:pt n) (nf:pt k))) ders)))))
  (setq ders (vl-sort ders
               (function (lambda (a b)
                 (if (equal (caddr a) (caddr b) 1e-6)
                   (< (cadddr a) (cadddr b))
                   (if lejano (> (caddr a) (caddr b)) (< (caddr a) (caddr b))))))))

  ;; ---- capas y limpieza opcional
  (nf:capa *nf-capet* NF:COLOR_ET)
  (if (= *NF_BORRAR* "1")
    (foreach flt (list (list '(0 . "TEXT,MTEXT") (cons 8 *nf-capet*) (cons 410 (getvar "CTAB")))
                       (list '(0 . "LWPOLYLINE") (cons 8 NF:CAPA_RUTA) (cons 410 (getvar "CTAB"))))
      (if (setq sel (ssget "_X" flt))
        (repeat (setq i (sslength sel)) (entdel (ssname sel (setq i (1- i))))))))

  ;; ---- etiquetas F1
  (setq rest path)
  (while rest
    (setq n (car rest) nx (cadr rest))
    (if (or (= n root) (and NF:F1_CADA_DIV (nf:isdiv n)))
      (if nx
        (nf:etiqueta n nx (strcat pref "1"))
        (nf:etiqueta n (nf:par n) (strcat pref "1"))))
    (setq rest (cdr rest)))

  ;; ---- ruta F1 resaltada
  (if (= *NF_RUTA* "1")
    (progn
      (nf:capa NF:CAPA_RUTA NF:COLOR_RUTA)
      (entmakex (append (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 NF:CAPA_RUTA)
                              '(100 . "AcDbPolyline") (cons 90 (length path)) '(70 . 0)
                              (cons 43 NF:ANCHO_RUTA))
                        (mapcar '(lambda (x) (cons 10 (nf:pt x))) path)))))

  ;; ---- numeracion y etiquetas de derivadas
  (setq k 2 prevn nil)
  (foreach d ders
    (if (and porcierre (/= (car d) prevn)) (setq k 2))
    (setq nombre (strcat pref (itoa k))
          nodos  (nf:subtree (cadr d))
          len    0.0 ndv 0)
    (foreach m nodos
      (setq len (+ len (- (nf:dist m) (nf:dist (nf:par m)))))
      (if (nf:isdiv m) (setq ndv (1+ ndv))))
    (nf:etiqueta (car d) (cadr d) nombre)
    (if NF:FN_CADA_DIV (nf:etq-subarbol nodos nombre))
    (setq res    (cons (list nombre (car d) (caddr d) len ndv) res)
          cuenta (if (assoc (car d) cuenta)
                   (subst (cons (car d) (1+ (cdr (assoc (car d) cuenta)))) (assoc (car d) cuenta) cuenta)
                   (cons (cons (car d) 1) cuenta))
          k      (1+ k)
          prevn  (car d)))
  (setq res (reverse res))

  ;; ---- diagnostico
  (foreach b *nf-box*
    (cond ((not (member (car b) usados)) (setq sinc (cons (car b) sinc)))
          ((not (assoc (car b) *nf-par*)) (setq fuera (cons (car b) fuera)))))

  ;; ---- reporte en linea de comandos
  (princ "\n================ NOMFIBRA ================")
  (princ (strcat "\nNodos: " (itoa (length *nf-pts*)) "   Tramos: " (itoa (length *nf-edges*))
                 "   Divisores leidos: " (itoa (length *nf-box*))))
  (princ (strcat "\n" pref "1 (continuidad): long = " (rtos f1len 2 NF:DEC)
                 "   divisores = " (itoa f1dc)))
  (foreach r res
    (princ (strcat "\n" (car r) ": sale a " (rtos (caddr r) 2 NF:DEC) " del inicio"
                   "   long = " (rtos (cadddr r) 2 NF:DEC)
                   "   divisores = " (itoa (nth 4 r)))))
  (if (> NF:MAX_DER 0)
    (foreach c cuenta
      (if (> (cdr c) NF:MAX_DER)
        (princ (strcat "\nAVISO: un cierre tiene " (itoa (cdr c)) " derivadas (max. "
                       (itoa NF:MAX_DER) ").")))))
  (if sinc  (princ (strcat "\nAVISO: " (itoa (length sinc))  " divisor(es) no tocan la fibra.")))
  (if fuera (princ (strcat "\nAVISO: " (itoa (length fuera)) " divisor(es) en fibra no conectada al inicio.")))
  (princ (strcat "\nEtiquetas creadas: " (itoa *nf-netq*) " en capa " *nf-capet*))

  ;; ---- CSV
  (if (= *NF_CSV* "1")
    (progn
      (setq csvf (strcat (getvar "DWGPREFIX") (vl-filename-base (getvar "DWGNAME")) "_NOMFIBRA.csv"))
      (if (setq fh (open csvf "w"))
        (progn
          (write-line (strcat "Nombre" NF:SEP "Sale_a_dist_inicio" NF:SEP "Longitud" NF:SEP "Divisores") fh)
          (write-line (strcat pref "1" NF:SEP "0" NF:SEP (rtos f1len 2 NF:DEC) NF:SEP (itoa f1dc)) fh)
          (foreach r res
            (write-line (strcat (car r) NF:SEP (rtos (caddr r) 2 NF:DEC) NF:SEP
                                (rtos (cadddr r) 2 NF:DEC) NF:SEP (itoa (nth 4 r)))
                        fh))
          (close fh)
          (princ (strcat "\nCSV: " csvf)))
        (princ "\nNo se pudo escribir el CSV (archivo abierto en Excel?)."))))

  (vla-EndUndoMark doc)
  (setvar "CMDECHO" oce)
  (princ))

(princ "\nNOMFIBRA v1.0 cargado. Escribe NOMFIBRA.")
(princ)
