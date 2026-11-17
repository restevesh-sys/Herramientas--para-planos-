;;;=========================================================================
;;; NUMERA.lsp
;;; Comandos : NUMERA (numerar)  /  NUMTABLA (tabla resumen de conteo)
;;; Version  : 1.3
;;; Fecha    : 2026-10-16
;;; v1.3     : Cuadro de dialogo DCL (se genera solo, no hay .dcl aparte):
;;;            plantilla, prefijo/numero/sufijo, incremento, digitos,
;;;            consecutivo/fijo, texto o bloque (+atributo), capa existente,
;;;            altura, rotacion, figura, holgura, escala, limite, vista previa
;;;            y botones "<" para tomar altura/rotacion/bloque de la pantalla.
;;;            Se abre al iniciar NUMERA y con la opcion Config.
;;;            La opcion Linea conserva la configuracion por linea de comandos.
;;; v1.2     : Ya NO crea capas. Etiquetas en la capa actual o en una capa
;;;            existente (Config > Capa: tocar un objeto o escribir nombre).
;;;            La tabla va en la capa actual con marca XDATA "NUMTABLA" y
;;;            NUMTABLA ignora esos objetos al contar (no se cuenta a si misma).
;;;            Reemplazar/Ordenar no cambian la capa de los objetos.
;;; v1.1     : Agrega NUMTABLA y la opcion Tabla dentro de NUMERA:
;;;            cuenta bloques o textos de la misma capa que un ejemplo,
;;;            agrupa por nombre/valor y dibuja la tabla "CONTEO DE BLOQUES"
;;;            con su simbolo, fila TOTAL, total de unidades y CSV opcional.
;;; AutoCAD  : 2022 / Civil 3D 2022 (no usa comandos de linea; DCL opcional)
;;;
;;; Descripcion:
;;;   Coloca etiquetas PREFIJO + NUMERO + SUFIJO para edificios, lotes,
;;;   casas, departamentos, notas o bloques con atributo.
;;;   - Modo Consecutivo (1,2,3... / A,B,C... / descendente con incremento
;;;     negativo) o Modo Fijo (la misma nota repetida, ej. CN-20).
;;;   - Colocacion por clic, en Arreglo (N elementos en linea), Reemplazar
;;;     textos/atributos existentes uno por uno, u Ordenar (renumerar una
;;;     seleccion por filas o columnas).
;;;   - Texto con circulo o rectangulo opcional, o Bloque con atributo.
;;;   - Limite opcional de etiquetas por ejecucion.
;;;   - Recuerda la configuracion entre sesiones.
;;;   - Todo lo que hace queda en un solo UNDO (U lo deshace completo).
;;;
;;; Nota: los textos de la linea de comando van sin acentos a proposito,
;;;       para evitar problemas de codificacion del archivo.
;;;=========================================================================

(vl-load-com)

;;---------------------------- PARAMETROS ---------------------------------

;; Tolerancia para agrupar filas/columnas en ORDENAR (unidades de dibujo).
;; 0 = automatica (0.6 x altura de texto configurada).
(setq *NUM:TOL* 0.0)

;; Lo que escribes para dejar VACIO un prefijo/sufijo, o volver a la capa actual.
(setq *NUM:LIMPIA* "~")

;; T = usar el cuadro de dialogo. nil = solo linea de comandos.
(setq *NUM:USAR_DCL* T)

;; Plantillas: (nombre  prefijo  sufijo  altura  figura)
;; figura: "N" = ninguna, "C" = circulo, "R" = rectangulo
;; La capa NO la cambia la plantilla: se usa la que tengas en Config > Capa.
(setq *NUM:PLANT*
  '(("Edificio" ""       ""  2.5 "C")
    ("Lote"     "L-"     ""  1.0 "N")
    ("Casa"     "#"      ""  1.0 "N")
    ("Depto"    "DP - #" ""  0.8 "N")
    ("Nota"     ""       ""  0.8 "N")))

;; Valores por defecto (solo se usan la primera vez; luego se recuerdan).
(setq *NUM:DEF*
  '(("PRE" . "DP - #")   ; prefijo
    ("SUF" . "")         ; sufijo
    ("VAL" . "1")        ; numero o letra actual
    ("INC" . "1")        ; incremento (negativo = descendente)
    ("DIG" . "0")        ; digitos con ceros a la izquierda (0 = sin relleno)
    ("MOD" . "C")        ; C = consecutivo, F = fijo
    ("TIP" . "T")        ; T = texto, B = bloque
    ("BLK" . "")         ; nombre de bloque
    ("ATT" . "")         ; etiqueta del atributo ("" = el primero)
    ("ALT" . "0.8")      ; altura de texto
    ("CAP" . "")         ; capa existente ("" = capa actual). Nunca se crea.
    ("FIG" . "N")        ; figura alrededor del texto: N, C, R
    ("HOL" . "1.5")      ; holgura de la figura (1.0 = justa)
    ("ESC" . "1.0")      ; escala del bloque
    ("ROT" . "0")        ; rotacion en grados
    ("LIM" . "0")))      ; limite de etiquetas por ejecucion (0 = sin limite)

;;------------------------ CONFIGURACION (memoria) ------------------------

(defun _num:g (k) (cdr (assoc k *NUM*)))

(defun _num:s (k v)
  (setq *NUM*
    (if (assoc k *NUM*)
      (subst (cons k v) (assoc k *NUM*) *NUM*)
      (cons (cons k v) *NUM*)))
  v)

;; Se guarda con "|" delante para poder recordar valores vacios.
(defun _num:cargar ( / v)
  (setq *NUM* nil)
  (foreach p *NUM:DEF*
    (setq v (getenv (strcat "NUMERA_" (car p))))
    (_num:s (car p)
      (if (and v (= (substr v 1 1) "|")) (substr v 2) (cdr p)))))

(defun _num:guardar ()
  (foreach p *NUM*
    (setenv (strcat "NUMERA_" (car p)) (strcat "|" (cdr p)))))

;;--------------------------- NUMERO / ETIQUETA ---------------------------

(defun _num:esnum (v / l)
  (setq l (vl-string->list v))
  (if (and l (= (car l) 45)) (setq l (cdr l)))
  (and l (vl-every '(lambda (c) (<= 48 c 57)) l)))

(defun _num:esalfa (v / l)
  (setq l (vl-string->list (strcase v)))
  (and l (vl-every '(lambda (c) (<= 65 c 90)) l)))

;; A->B, Z->AA, AZ->BA
(defun _num:alfa+ (s / r carry)
  (setq carry t)
  (foreach c (reverse (vl-string->list (strcase s)))
    (if carry
      (if (= c 90)
        (setq r (cons 65 r))
        (setq r (cons (1+ c) r) carry nil))
      (setq r (cons c r))))
  (if carry (setq r (cons 65 r)))
  (vl-list->string r))

(defun _num:pad (v / d neg)
  (setq d (atoi (_num:g "DIG")))
  (if (_num:esnum v)
    (progn
      (setq neg (= (substr v 1 1) "-"))
      (if neg (setq v (substr v 2)))
      (while (< (strlen v) d) (setq v (strcat "0" v)))
      (if neg (strcat "-" v) v))
    v))

(defun _num:etiqueta ()
  (strcat (_num:g "PRE") (_num:pad (_num:g "VAL")) (_num:g "SUF")))

(defun _num:avanzar ( / v n)
  (if (= (_num:g "MOD") "C")
    (progn
      (setq v (_num:g "VAL")
            n (atoi (_num:g "INC")))
      (cond
        ((_num:esnum v)  (_num:s "VAL" (itoa (+ (atoi v) n))))
        ((_num:esalfa v) (repeat (abs n) (setq v (_num:alfa+ v)))
                         (_num:s "VAL" v))))))

;; Usa las variables PUESTOS / PRIMERO / ULTIMO de c:NUMERA.
(defun _num:contar ()
  (setq puestos (1+ puestos)
        ultimo  (_num:etiqueta))
  (if (not primero) (setq primero ultimo)))

;;----------------------------- AUXILIARES --------------------------------

(defun _num:2d (p) (list (car p) (cadr p)))

(defun _num:ang ()
  (+ (angle '(0.0 0.0 0.0) (getvar 'ucsxdir))
     (* pi (/ (atof (_num:g "ROT")) 180.0))))

;; Capa de numeracion: la configurada si EXISTE; si no, la capa actual.
;; No crea capas.
(defun _num:capa ( / c)
  (setq c (_num:g "CAP"))
  (cond
    ((= c "") (getvar 'clayer))
    ((tblsearch "LAYER" c) c)
    (t (princ (strcat "\nLa capa " c " no existe en este dibujo; se usa la capa actual."))
       (_num:s "CAP" "")
       (getvar 'clayer))))

(defun _num:tags (blk / e l)
  (if (setq e (tblobjname "BLOCK" blk))
    (while (setq e (entnext e))
      (if (= "ATTDEF" (cdr (assoc 0 (entget e))))
        (setq l (cons (strcase (cdr (assoc 2 (entget e)))) l)))))
  (reverse l))

;; Escribe S en el atributo configurado (o el primero). Devuelve T si pudo.
(defun _num:att (obj s / atts tag at)
  (if (= :vlax-true (vla-get-hasattributes obj))
    (progn
      (setq atts (vlax-invoke obj 'getattributes)
            tag  (strcase (_num:g "ATT"))
            at   (if (= tag "")
                   (car atts)
                   (vl-some '(lambda (x) (if (= (strcase (vla-get-tagstring x)) tag) x)) atts)))
      (if at (progn (vla-put-textstring at s) t)))))

;;------------------------------ COLOCAR ----------------------------------

(defun _num:bloque (p s cap ang / blk esc doc spc obj)
  (setq blk (_num:g "BLK"))
  (if (or (= blk "") (not (tblsearch "BLOCK" blk)))
    (progn
      (princ (strcat "\nEl bloque \"" blk "\" no existe en el dibujo. Usa Config > Bloque."))
      nil)
    (progn
      (setq esc (atof (_num:g "ESC"))
            doc (vla-get-activedocument (vlax-get-acad-object))
            spc (if (= 1 (getvar 'cvport)) (vla-get-paperspace doc) (vla-get-modelspace doc))
            obj (vla-insertblock spc (vlax-3d-point p) blk esc esc esc ang))
      (vla-put-layer obj cap)
      (if (and (= :vlax-true (vla-get-hasattributes obj))
               (not (_num:att obj s)))
        (princ (strcat "\nAviso: el bloque no tiene el atributo " (_num:g "ATT"))))
      (vlax-vla-object->ename obj))))

(defun _num:colocar (pt / p s cap ang alt e eb w hol fig a b)
  (setq p   (trans pt 1 0)
        s   (_num:etiqueta)
        cap (_num:capa)
        ang (_num:ang))
  (if (= (_num:g "TIP") "B")
    (_num:bloque p s cap ang)
    (progn
      (setq alt (atof (_num:g "ALT"))
            e   (entmakex
                  (list '(0 . "TEXT") '(100 . "AcDbEntity") (cons 8 cap)
                        '(100 . "AcDbText") (cons 10 p) (cons 40 alt) (cons 1 s)
                        (cons 50 ang) (cons 7 (getvar 'textstyle))
                        '(72 . 1) (cons 11 p) '(100 . "AcDbText") '(73 . 2))))
      (if (and e (/= (setq fig (_num:g "FIG")) "N"))
        (progn
          (setq eb  (textbox (entget e))
                w   (- (caadr eb) (caar eb))
                hol (atof (_num:g "HOL")))
          (if (= fig "C")
            (entmakex (list '(0 . "CIRCLE") (cons 8 cap) (cons 10 p)
                            (cons 40 (* hol 0.5 (max w alt)))))
            (progn
              (setq a (+ (/ w 2.0)   (* alt (- hol 1.0)))
                    b (+ (/ alt 2.0) (* alt (- hol 1.0))))
              (entmakex
                (append
                  (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 cap)
                        '(100 . "AcDbPolyline") '(90 . 4) '(70 . 1) (cons 38 (caddr p)))
                  (mapcar
                    '(lambda (d)
                       (cons 10 (_num:2d (polar (polar p ang (car d)) (+ ang (/ pi 2.0)) (cadr d)))))
                    (list (list (- a) (- b)) (list a (- b)) (list a b) (list (- a) b)))))))))
      e)))

;;------------------------------ ARREGLO ----------------------------------

(defun _num:arreglo ( / n p1 p2 v def)
  (setq def (if (> (atoi (_num:g "LIM")) 0) (atoi (_num:g "LIM")) 10))
  (initget 6)
  (setq n (cond ((getint (strcat "\nCantidad de elementos <" (itoa def) ">: "))) (def)))
  (if (setq p1 (getpoint "\nPunto del primer elemento: "))
    (progn
      (initget "Final")
      (setq p2 (getpoint p1 "\nPunto del segundo elemento (fija la separacion) o [Final]: "))
      (cond
        ((= p2 "Final")
          (if (setq p2 (getpoint p1 "\nPunto del ultimo elemento: "))
            (setq v (if (> n 1)
                      (mapcar '(lambda (a b) (/ (- b a) (float (1- n)))) p1 p2)
                      '(0.0 0.0 0.0)))))
        (p2 (setq v (mapcar '- p2 p1))))
      (if v
        (repeat n
          (if (_num:colocar p1) (progn (_num:contar) (_num:avanzar)))
          (setq p1 (mapcar '+ p1 v)))))))

;;----------------------------- REEMPLAZAR --------------------------------

(defun _num:reemplazar ( / sel e tipo obj)
  (while
    (progn
      (setvar 'errno 0)
      (setq sel (nentsel (strcat "\nTexto/atributo que sera [" (_num:etiqueta) "] <terminar>: ")))
      (or sel (= 7 (getvar 'errno))))
    (if (null sel)
      (princ "\nNo tocaste nada, intenta de nuevo.")
      (progn
        (setq e (car sel) tipo (cdr (assoc 0 (entget e))))
        (cond
          ((not (member tipo '("TEXT" "MTEXT" "ATTRIB")))
            (princ "\nSolo texto, texto multiple o atributo."))
          ((and (cddr sel) (/= tipo "ATTRIB"))
            (princ "\nEse texto esta dentro de un bloque (no es atributo); no se modifica."))
          ((not (vlax-write-enabled-p (setq obj (vlax-ename->vla-object e))))
            (princ "\nObjeto en capa bloqueada."))
          (t
            (vla-put-textstring obj (_num:etiqueta))
            (if (= tipo "ATTRIB") (entupd (cdr (assoc 330 (entget e)))))
            (_num:contar)
            (_num:avanzar)))))))

;;------------------------------ ORDENAR ----------------------------------

(defun _num:ordenar ( / ss i obj ll ur c lst tol kw n)
  (princ "\nSelecciona los textos o bloques a renumerar: ")
  (if (setq ss (ssget '((0 . "TEXT,MTEXT,INSERT"))))
    (progn
      (initget "Filas Columnas")
      (setq kw (cond ((getkword "\nOrden: Filas = arriba a abajo / Columnas = izquierda a derecha [Filas/Columnas] <Columnas>: "))
                     ("Columnas")))
      (setq tol (if (> *NUM:TOL* 0.0) *NUM:TOL* (* 0.6 (atof (_num:g "ALT"))))
            n   0)
      (repeat (setq i (sslength ss))
        (setq obj (vlax-ename->vla-object (ssname ss (setq i (1- i)))))
        (vla-getboundingbox obj 'll 'ur)
        (setq c (trans (mapcar '(lambda (a b) (/ (+ a b) 2.0))
                               (vlax-safearray->list ll) (vlax-safearray->list ur))
                       0 1)
              lst (cons (list c obj) lst)))
      (setq lst
        (vl-sort lst
          (if (= kw "Filas")
            (function (lambda (a b)
              (if (equal (cadr (car a)) (cadr (car b)) tol)
                (< (car (car a)) (car (car b)))
                (> (cadr (car a)) (cadr (car b))))))
            (function (lambda (a b)
              (if (equal (car (car a)) (car (car b)) tol)
                (> (cadr (car a)) (cadr (car b)))
                (< (car (car a)) (car (car b)))))))))
      (foreach x lst
        (setq obj (cadr x))
        (cond
          ((not (vlax-write-enabled-p obj))
            (princ "\nOmitido: capa bloqueada."))
          ((= (vla-get-objectname obj) "AcDbBlockReference")
            (if (_num:att obj (_num:etiqueta))
              (progn (_num:contar) (_num:avanzar) (setq n (1+ n)))
              (princ "\nOmitido: bloque sin el atributo configurado.")))
          (t
            (vla-put-textstring obj (_num:etiqueta))
            (_num:contar) (_num:avanzar) (setq n (1+ n)))))
      (princ (strcat "\n" (itoa n) " de " (itoa (length lst)) " objetos renumerados.")))))

;;------------------------------ CONFIG -----------------------------------

(defun _num:resumen ()
  (princ
    (strcat
      "\n----------------------- NUMERA -----------------------"
      "\nPrefijo: \"" (_num:g "PRE") "\"  Numero: " (_num:g "VAL")
      "  Incremento: " (_num:g "INC") "  Digitos: " (_num:g "DIG")
      "  Sufijo: \"" (_num:g "SUF") "\""
      "\nModo: " (if (= (_num:g "MOD") "C") "Consecutivo" "Fijo")
      "  Tipo: " (if (= (_num:g "TIP") "B")
                   (strcat "Bloque " (_num:g "BLK") " [" (_num:g "ATT") "]")
                   "Texto")
      "  Altura: " (_num:g "ALT")
      "  Capa: " (if (= (_num:g "CAP") "") (strcat "(actual: " (getvar 'clayer) ")") (_num:g "CAP"))
      "\nFigura: " (cdr (assoc (_num:g "FIG") '(("N" . "Ninguna") ("C" . "Circulo") ("R" . "Rectangulo"))))
      "  Holgura: " (_num:g "HOL") "  Rotacion: " (_num:g "ROT") " grados"
      "  Limite: " (if (= (_num:g "LIM") "0") "sin limite" (_num:g "LIM"))
      "\nProxima etiqueta: " (_num:etiqueta)
      "\n------------------------------------------------------")))

(defun _num:pidetxt (k msg / v)
  (setq v (getstring t (strcat msg " <" (_num:g k) ">: ")))
  (cond ((= v ""))
        ((= v *NUM:LIMPIA*) (_num:s k ""))
        (t (_num:s k v))))

(defun _num:pidereal (k msg / v)
  (initget 6)
  (if (setq v (getreal (strcat msg " <" (_num:g k) ">: ")))
    (_num:s k (vl-princ-to-string v))))

(defun _num:pideint (k msg flg / v)
  (initget flg)
  (if (setq v (getint (strcat msg " <" (_num:g k) ">: ")))
    (_num:s k (itoa v))))

(defun _num:valor ( / v)
  (setq v (getstring (strcat "\nNumero o letra actual (001 = con ceros) <" (_num:g "VAL") ">: ")))
  (if (/= v "")
    (progn
      (_num:s "VAL" (strcase v))
      (if (and (_num:esnum v) (> (strlen v) 1) (= (substr v 1 1) "0"))
        (_num:s "DIG" (itoa (strlen v)))))))

(defun _num:girar ( / a)
  (if (setq a (getangle (strcat "\nRotacion: grados o dos puntos <" (_num:g "ROT") ">: ")))
    (_num:s "ROT" (rtos (/ (* a 180.0) pi) 2 2))))

(defun _num:elegirbloque ( / s o lst def)
  (initget "Nombre")
  (setq s (entsel "\nSelecciona un bloque del dibujo o [Nombre]: "))
  (cond
    ((= s "Nombre")
      (setq s (getstring t (strcat "\nNombre del bloque <" (_num:g "BLK") ">: ")))
      (cond ((= s ""))
            ((tblsearch "BLOCK" s) (_num:s "BLK" s))
            (t (princ "\nEse bloque no existe en el dibujo."))))
    ((and (listp s) s (= "INSERT" (cdr (assoc 0 (entget (car s))))))
      (setq o (vlax-ename->vla-object (car s)))
      (_num:s "BLK" (vla-get-effectivename o)))
    (s (princ "\nEso no es un bloque.")))
  (if (and (/= (_num:g "BLK") "") (tblsearch "BLOCK" (_num:g "BLK")))
    (progn
      (_num:s "TIP" "B")
      (if (setq lst (_num:tags (_num:g "BLK")))
        (progn
          (setq def (if (member (strcase (_num:g "ATT")) lst) (strcase (_num:g "ATT")) (car lst)))
          (princ (strcat "\nAtributos del bloque: "
                         (apply 'strcat (mapcar '(lambda (x) (strcat x "  ")) lst))))
          (setq s (getstring (strcat "\nAtributo que lleva el numero <" def ">: ")))
          (_num:s "ATT" (strcase (if (= s "") def s))))
        (progn
          (_num:s "ATT" "")
          (princ "\nEl bloque no tiene atributos: se insertara sin numero (marca fija)."))))))

(defun _num:config ( / k v)
  (while
    (progn
      (_num:resumen)
      (initget "Prefijo Sufijo Numero Incremento Digitos Modo Tipo Bloque Altura Capa Figura Holgura Escala Rotacion Limite")
      (setq k (getkword "\nCambiar [Prefijo/Sufijo/Numero/Incremento/Digitos/Modo/Tipo/Bloque/Altura/Capa/Figura/Holgura/Escala/Rotacion/Limite] <Listo>: ")))
    (cond
      ((= k "Prefijo")    (_num:pidetxt "PRE" (strcat "\nPrefijo (" *NUM:LIMPIA* " = vacio)")))
      ((= k "Sufijo")     (_num:pidetxt "SUF" (strcat "\nSufijo (" *NUM:LIMPIA* " = vacio)")))
      ((= k "Numero")     (_num:valor))
      ((= k "Incremento") (_num:pideint "INC" "\nIncremento (negativo = descendente)" 2))
      ((= k "Digitos")    (_num:pideint "DIG" "\nDigitos con ceros (0 = sin relleno, 3 = 001)" 4))
      ((= k "Modo")
        (initget "Consecutivo Fijo")
        (if (setq v (getkword "\nModo [Consecutivo/Fijo]: ")) (_num:s "MOD" (substr v 1 1))))
      ((= k "Tipo")
        (initget "Texto Bloque")
        (if (setq v (getkword "\nTipo [Texto/Bloque]: "))
          (if (= v "Bloque") (_num:elegirbloque) (_num:s "TIP" "T"))))
      ((= k "Bloque")     (_num:elegirbloque))
      ((= k "Altura")     (_num:pidereal "ALT" "\nAltura de texto"))
      ((= k "Capa")
        (initget "Nombre Actual")
        (setq v (entsel "\nToca un objeto para usar su capa o [Nombre/Actual]: "))
        (cond
          ((= v "Actual") (_num:s "CAP" ""))
          ((= v "Nombre")
            (setq v (getstring t (strcat "\nNombre de capa existente (" *NUM:LIMPIA* " = actual): ")))
            (cond ((= v ""))
                  ((= v *NUM:LIMPIA*) (_num:s "CAP" ""))
                  ((tblsearch "LAYER" v) (_num:s "CAP" (cdr (assoc 2 (tblsearch "LAYER" v)))))
                  (t (princ "\nEsa capa no existe; no se crea. Sin cambios."))))
          ((listp v) (if v (_num:s "CAP" (cdr (assoc 8 (entget (car v)))))))))
      ((= k "Figura")
        (initget "Ninguna Circulo Rectangulo")
        (if (setq v (getkword "\nFigura alrededor del texto [Ninguna/Circulo/Rectangulo]: "))
          (_num:s "FIG" (substr v 1 1))))
      ((= k "Holgura")    (_num:pidereal "HOL" "\nHolgura de la figura (1.0 = justa, 1.5 = amplia)"))
      ((= k "Escala")     (_num:pidereal "ESC" "\nEscala del bloque"))
      ((= k "Rotacion")   (_num:girar))
      ((= k "Limite")     (_num:pideint "LIM" "\nLimite de etiquetas por ejecucion (0 = sin limite)" 4)))))

(defun _num:plantilla ( / k p v)
  (initget "Edificio Lote Casa Depto Nota")
  (if (setq k (getkword "\nPlantilla [Edificio/Lote/Casa/Depto/Nota]: "))
    (progn
      (setq p (assoc k *NUM:PLANT*))
      (_num:s "PRE" (nth 1 p))
      (_num:s "SUF" (nth 2 p))
      (_num:s "ALT" (vl-princ-to-string (nth 3 p)))
      (_num:s "FIG" (nth 4 p))
      (_num:s "TIP" "T")
      (_num:s "INC" "1")
      (_num:s "DIG" "0")
      (if (= k "Nota")
        (progn
          (_num:s "MOD" "F")
          (_num:s "VAL" "")
          (setq v (getstring t "\nTexto de la nota fija (ej. CN-20): "))
          (if (/= v "") (_num:s "PRE" v)))
        (progn
          (_num:s "MOD" "C")
          (_num:s "VAL" "1")
          (_num:valor))))))

(defun _num:final ()
  (if (> puestos 0)
    (princ
      (strcat "\nNUMERA: " (itoa puestos) " etiqueta(s)"
              (if (= primero ultimo)
                (strcat " [" primero "]")
                (strcat " de [" primero "] a [" ultimo "]"))
              ". Siguiente: [" (_num:etiqueta) "]"))))

;;--------------------------- TABLA RESUMEN -------------------------------

;; Parametros de la tabla
(setq *NUM:TAB_ALT*   2.5)              ; altura de texto por defecto de la tabla
;; La tabla se dibuja en la CAPA ACTUAL (no crea capas) y cada objeto lleva
;; la marca XDATA "NUMTABLA" para que no se cuente al volver a contar.
(setq *NUM:TAB_TIT*   "CONTEO DE BLOQUES")
(setq *NUM:TAB_DESC*  "departamentos")  ; texto que sigue al valor cuando se cuentan TEXTOS
(setq *NUM:TAB_CSV*   T)                ; T = tambien escribe <dibujo>_conteo.csv junto al DWG

(setq *NUM:XD* '((-3 ("NUMTABLA" (1000 . "TABLA")))))

;; T si la entidad pertenece a una tabla hecha por NUMTABLA
(defun _num:establa (e) (assoc -3 (entget e '("NUMTABLA"))))

;; Agrega la marca a una entidad ya creada (bloques insertados con VLA)
(defun _num:marcar (e) (entmod (append (entget e) *NUM:XD*)))

(defun _num:txt (p s h cap)
  (entmakex (append
              (list '(0 . "TEXT") '(100 . "AcDbEntity") (cons 8 cap) '(100 . "AcDbText")
                    (cons 10 p) (cons 40 h) (cons 1 s) '(50 . 0.0) (cons 7 (getvar 'textstyle))
                    '(72 . 1) (cons 11 p) '(100 . "AcDbText") '(73 . 2))
              *NUM:XD*)))

(defun _num:lin (a b cap)
  (entmakex (append (list '(0 . "LINE") (cons 8 cap) (cons 10 a) (cons 11 b)) *NUM:XD*)))

;; Numero al inicio del texto ("20 departamentos" -> 20), o nil
(defun _num:lead (s) (if (wcmatch s "#*") (atoi s)))

(defun _num:tabla ( / ej e0 ins capa kw flt ss i o k g grp desc th p0 rh n maxl
                      c1 c2 c3 w x0 y0 nr r tot unid cap doc spc cen ll ur dx dy sc bc f fn)
  (setq ej (entsel "\nSelecciona un EJEMPLO a contar (bloque o texto): "))
  (cond
    ((null ej) (princ "\nNada seleccionado."))
    ((not (member (cdr (assoc 0 (entget (setq e0 (car ej))))) '("INSERT" "TEXT" "MTEXT")))
      (princ "\nEl ejemplo debe ser un bloque o un texto."))
    (t
      (setq ins  (= "INSERT" (cdr (assoc 0 (entget e0))))
            capa (cdr (assoc 8 (entget e0)))
            flt  (list (cons 0 (if ins "INSERT" "TEXT,MTEXT")) (cons 8 capa)))
      (initget "Todo Ventana")
      (setq kw (cond ((getkword (strcat "\nContar en capa " capa " [Todo/Ventana] <Ventana>: "))) ("Ventana")))
      (setq ss (if (= kw "Todo")
                 (ssget "_X" (append flt (list (cons 410 (getvar 'ctab)))))
                 (ssget flt)))
      ;; Quitar los objetos de tablas anteriores (marca NUMTABLA)
      (if ss
        (progn
          (repeat (setq i (sslength ss))
            (if (_num:establa (setq e0 (ssname ss (setq i (1- i)))))
              (ssdel e0 ss)))
          (if (= 0 (sslength ss)) (setq ss nil))))
      (if (null ss)
        (princ "\nNo se encontro nada que contar.")
        (progn
          ;; Agrupar
          (repeat (setq i (sslength ss))
            (setq o (vlax-ename->vla-object (ssname ss (setq i (1- i))))
                  k (if ins (vla-get-effectivename o) (vla-get-textstring o)))
            (if (setq g (assoc k grp))
              (setq grp (subst (cons k (1+ (cdr g))) g grp))
              (setq grp (cons (cons k 1) grp))))
          (setq grp (vl-sort grp
                      (function (lambda (a b / na nb)
                        (setq na (_num:lead (car a)) nb (_num:lead (car b)))
                        (if (and na nb (/= na nb)) (< na nb) (< (car a) (car b)))))))
          ;; Descripcion para textos
          (if (not ins)
            (progn
              (setq desc (getstring t (strcat "\nDescripcion despues del valor (" *NUM:LIMPIA* " = ninguna) <" *NUM:TAB_DESC* ">: ")))
              (cond ((= desc "") (setq desc *NUM:TAB_DESC*))
                    ((= desc *NUM:LIMPIA*) (setq desc "")))
              (if (/= desc "") (setq *NUM:TAB_DESC* desc))))
          ;; Totales
          (setq tot  (apply '+ (mapcar 'cdr grp))
                unid (if (vl-every '(lambda (x) (_num:lead (car x))) grp)
                       (apply '+ (mapcar '(lambda (x) (* (_num:lead (car x)) (cdr x))) grp))))
          ;; Resumen en linea de comando
          (princ (strcat "\n===== " *NUM:TAB_TIT* " (capa " capa ") ====="))
          (foreach x grp
            (princ (strcat "\n  " (car x) (if (or ins (= desc "")) "" (strcat " " desc)) " : " (itoa (cdr x)))))
          (princ (strcat "\n  TOTAL: " (itoa tot)
                         (if unid (strcat "   Unidades (valor x veces): " (itoa unid)) "")))
          ;; Dibujar tabla
          (initget 6)
          (setq th (cond ((getdist (strcat "\nAltura de texto de la tabla <" (rtos *NUM:TAB_ALT* 2 2) ">: ")))
                         (*NUM:TAB_ALT*)))
          (setq *NUM:TAB_ALT* th)
          (if (setq p0 (getpoint "\nEsquina superior izquierda de la tabla <solo resumen>: "))
            (progn
              (setq p0   (trans p0 1 0)
                    x0   (car p0) y0 (cadr p0)
                    cap  (getvar 'clayer)
                    rh   (* 2.4 th)
                    n    (length grp)
                    maxl (apply 'max (cons 18 (mapcar '(lambda (x) (+ 2 (strlen (car x)) (if ins 0 (1+ (strlen desc))))) grp)))
                    c1   (* th 0.85 maxl)
                    c2   (* th 0.85 21)
                    c3   (* th 0.85 9)
                    w    (+ c1 c2 c3)
                    nr   (+ 3 n (if unid 1 0))
                    doc  (vla-get-activedocument (vlax-get-acad-object))
                    spc  (if (= 1 (getvar 'cvport)) (vla-get-paperspace doc) (vla-get-modelspace doc)))
              (if (not (tblsearch "APPID" "NUMTABLA")) (regapp "NUMTABLA"))
              ;; Marco
              (entmakex (append
                          (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 cap) '(100 . "AcDbPolyline")
                                '(90 . 4) '(70 . 1)
                                (list 10 x0 y0) (list 10 (+ x0 w) y0)
                                (list 10 (+ x0 w) (- y0 (* nr rh))) (list 10 x0 (- y0 (* nr rh))))
                          *NUM:XD*))
              ;; Lineas horizontales
              (setq r 1)
              (repeat (1- nr)
                (_num:lin (list x0 (- y0 (* r rh)) 0.0) (list (+ x0 w) (- y0 (* r rh)) 0.0) cap)
                (setq r (1+ r)))
              ;; Lineas verticales (debajo del titulo)
              (_num:lin (list (+ x0 c1) (- y0 rh) 0.0) (list (+ x0 c1) (- y0 (* nr rh)) 0.0) cap)
              (_num:lin (list (+ x0 c1 c2) (- y0 rh) 0.0) (list (+ x0 c1 c2) (- y0 (* nr rh)) 0.0) cap)
              ;; Titulo y encabezados
              (_num:txt (list (+ x0 (/ w 2.0)) (- y0 (* 0.5 rh)) 0.0) *NUM:TAB_TIT* th cap)
              (_num:txt (list (+ x0 (/ c1 2.0)) (- y0 (* 1.5 rh)) 0.0) (if ins "Nombre del Bloque" "Descripcion") th cap)
              (_num:txt (list (+ x0 c1 (/ c2 2.0)) (- y0 (* 1.5 rh)) 0.0) "Veces que se repite" th cap)
              (_num:txt (list (+ x0 c1 c2 (/ c3 2.0)) (- y0 (* 1.5 rh)) 0.0) (if ins "BLOQUE" "SIMBOLO") th cap)
              ;; Filas de datos
              (setq r 2)
              (foreach x grp
                (setq cen (list (+ x0 c1 c2 (/ c3 2.0)) (- y0 (* (+ r 0.5) rh)) 0.0))
                (_num:txt (list (+ x0 (/ c1 2.0)) (cadr cen) 0.0)
                          (strcat (car x) (if (or ins (= desc "")) "" (strcat " " desc))) th cap)
                (_num:txt (list (+ x0 c1 (/ c2 2.0)) (cadr cen) 0.0) (itoa (cdr x)) th cap)
                (if ins
                  (progn ;; bloque real ajustado a la celda
                    (setq o (vla-insertblock spc (vlax-3d-point cen) (car x) 1.0 1.0 1.0 0.0))
                    (vla-put-layer o cap)
                    (vla-getboundingbox o 'll 'ur)
                    (setq ll (vlax-safearray->list ll) ur (vlax-safearray->list ur)
                          dx (max 1e-6 (- (car ur) (car ll)))
                          dy (max 1e-6 (- (cadr ur) (cadr ll)))
                          sc (min (/ (* 0.8 rh) dy) (/ (* 0.85 c3) dx)))
                    (vla-scaleentity o (vlax-3d-point cen) sc)
                    (vla-getboundingbox o 'll 'ur)
                    (setq bc (mapcar '(lambda (a b) (/ (+ a b) 2.0)) (vlax-safearray->list ll) (vlax-safearray->list ur)))
                    (vla-move o (vlax-3d-point bc) (vlax-3d-point cen))
                    (_num:marcar (vlax-vla-object->ename o)))
                  (progn ;; circulo con el valor
                    (entmakex (append (list '(0 . "CIRCLE") (cons 8 cap) (cons 10 cen) (cons 40 (* 0.4 rh))) *NUM:XD*))
                    (_num:txt cen (car x) (* 0.42 rh) cap)))
                (setq r (1+ r)))
              ;; TOTAL y unidades
              (_num:txt (list (+ x0 (/ c1 2.0)) (- y0 (* (+ r 0.5) rh)) 0.0) "TOTAL" th cap)
              (_num:txt (list (+ x0 c1 (/ c2 2.0)) (- y0 (* (+ r 0.5) rh)) 0.0) (itoa tot) th cap)
              (if unid
                (progn
                  (setq r (1+ r))
                  (_num:txt (list (+ x0 (/ c1 2.0)) (- y0 (* (+ r 0.5) rh)) 0.0) "Unidades (valor x veces)" th cap)
                  (_num:txt (list (+ x0 c1 (/ c2 2.0)) (- y0 (* (+ r 0.5) rh)) 0.0) (itoa unid) th cap)))))
          ;; CSV para Excel
          (if (and *NUM:TAB_CSV* (= 1 (getvar 'dwgtitled)))
            (progn
              (setq fn (strcat (getvar 'dwgprefix) (vl-filename-base (getvar 'dwgname)) "_conteo.csv"))
              (if (setq f (open fn "w"))
                (progn
                  (write-line "Nombre,Veces,Valor,Unidades" f)
                  (foreach x grp
                    (write-line (strcat "\"" (car x) (if (or ins (= desc "")) "" (strcat " " desc)) "\","
                                        (itoa (cdr x)) ","
                                        (if (_num:lead (car x)) (itoa (_num:lead (car x))) "") ","
                                        (if (_num:lead (car x)) (itoa (* (_num:lead (car x)) (cdr x))) ""))
                                f))
                  (write-line (strcat "\"TOTAL\"," (itoa tot) ",," (if unid (itoa unid) "")) f)
                  (close f)
                  (princ (strcat "\nCSV: " fn)))
                (princ "\nNo se pudo escribir el CSV (carpeta protegida o archivo abierto en Excel)."))))))))
  (princ))

(defun c:NUMTABLA ( / *error* doc ocmd)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))
  (defun *error* (msg)
    (if ocmd (setvar 'cmdecho ocmd))
    (vla-endundomark doc)
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError NUMTABLA: " msg)))
    (princ))
  (setq ocmd (getvar 'cmdecho))
  (setvar 'cmdecho 0)
  (vla-startundomark doc)
  (_num:tabla)
  (*error* nil))

;;------------------------- CUADRO DE DIALOGO -----------------------------

(setq *NUM:DCL*
  '("numera : dialog { label = \"NUMERA - Numeracion consecutiva o fija\";"
    "  : row {"
    "    : column {"
    "      : boxed_column { label = \"Contenido\";"
    "        : popup_list { key = \"pla\"; label = \"Plantilla:\"; width = 34; }"
    "        : edit_box { key = \"pre\"; label = \"Prefijo / nota:\"; edit_width = 18; }"
    "        : edit_box { key = \"val\"; label = \"Numero o letra:\"; edit_width = 18; }"
    "        : edit_box { key = \"suf\"; label = \"Sufijo:\"; edit_width = 18; }"
    "        : row {"
    "          : edit_box { key = \"inc\"; label = \"Incremento:\"; edit_width = 5; }"
    "          : edit_box { key = \"dig\"; label = \"Digitos:\"; edit_width = 5; }"
    "        }"
    "        : radio_row { key = \"mod\";"
    "          : radio_button { key = \"C\"; label = \"Consecutivo\"; }"
    "          : radio_button { key = \"F\"; label = \"Fijo\"; }"
    "        }"
    "        : text { key = \"vis\"; label = \"\"; width = 34; }"
    "      }"
    "      : boxed_column { label = \"Cantidad\";"
    "        : edit_box { key = \"lim\"; label = \"Limite de etiquetas (0 = sin limite):\"; edit_width = 6; }"
    "      }"
    "    }"
    "    : column {"
    "      : boxed_column { label = \"Objeto\";"
    "        : radio_row { key = \"tip\";"
    "          : radio_button { key = \"T\"; label = \"Texto\"; }"
    "          : radio_button { key = \"B\"; label = \"Bloque\"; }"
    "        }"
    "        : row {"
    "          : popup_list { key = \"blk\"; label = \"Bloque:\"; width = 30; }"
    "          : button { key = \"pblk\"; label = \"<\"; width = 4; fixed_width = true; }"
    "        }"
    "        : popup_list { key = \"att\"; label = \"Atributo:\"; width = 30; }"
    "        : edit_box { key = \"esc\"; label = \"Escala del bloque:\"; edit_width = 8; }"
    "      }"
    "      : boxed_column { label = \"Formato\";"
    "        : popup_list { key = \"cap\"; label = \"Capa:\"; width = 30; }"
    "        : row {"
    "          : edit_box { key = \"alt\"; label = \"Altura de texto:\"; edit_width = 8; }"
    "          : button { key = \"palt\"; label = \"<\"; width = 4; fixed_width = true; }"
    "        }"
    "        : row {"
    "          : edit_box { key = \"rot\"; label = \"Rotacion (grados):\"; edit_width = 8; }"
    "          : button { key = \"prot\"; label = \"<\"; width = 4; fixed_width = true; }"
    "        }"
    "        : popup_list { key = \"fig\"; label = \"Figura:\"; width = 22; }"
    "        : edit_box { key = \"hol\"; label = \"Holgura de figura:\"; edit_width = 8; }"
    "      }"
    "    }"
    "  }"
    "  : row { alignment = centered; fixed_width = true;"
    "    : button { key = \"accept\"; label = \"Colocar\"; is_default = true; width = 12; }"
    "    : button { key = \"tabla\"; label = \"Tabla de conteo\"; width = 18; }"
    "    : button { key = \"cancel\"; label = \"Cancelar\"; is_cancel = true; width = 12; }"
    "  }"
    "  errtile;"
    "}"))

(setq *NUM:FIGS* '("N" "C" "R"))

(defun _num:pos (s / r) (and (setq r (distof s 2)) (> r 0.0)))

;; Pasa lo escrito en el cuadro a la configuracion (usa BLKS ATTS CAPS de _num:dialogo)
(defun _num:dlgleer ( / i)
  (_num:s "PRE" (get_tile "pre"))
  (_num:s "VAL" (strcase (get_tile "val")))
  (_num:s "SUF" (get_tile "suf"))
  (_num:s "INC" (get_tile "inc"))
  (_num:s "DIG" (get_tile "dig"))
  (_num:s "MOD" (get_tile "mod"))
  (_num:s "TIP" (get_tile "tip"))
  (_num:s "BLK" (if blks (nth (atoi (get_tile "blk")) blks) ""))
  (_num:s "ATT" (if atts (nth (atoi (get_tile "att")) atts) ""))
  (setq i (atoi (get_tile "cap")))
  (_num:s "CAP" (if (> i 0) (nth (1- i) caps) ""))
  (_num:s "FIG" (nth (atoi (get_tile "fig")) *NUM:FIGS*))
  (_num:s "ALT" (get_tile "alt"))
  (_num:s "ROT" (get_tile "rot"))
  (_num:s "HOL" (get_tile "hol"))
  (_num:s "ESC" (get_tile "esc"))
  (_num:s "LIM" (get_tile "lim")))

(defun _num:dlgvista ()
  (_num:dlgleer)
  (set_tile "vis" (strcat "Siguiente: " (_num:etiqueta))))

;; Activa/desactiva casillas segun Texto/Bloque y Consecutivo/Fijo
(defun _num:dlgmodos ( / b fij)
  (setq b   (= (get_tile "tip") "B")
        fij (= (get_tile "mod") "F"))
  (foreach k '("blk" "att" "esc") (mode_tile k (if (and b blks) 0 1)))
  (foreach k '("alt" "palt" "fig" "hol") (mode_tile k (if b 1 0)))
  (foreach k '("inc" "dig") (mode_tile k (if fij 1 0))))

;; Llena la lista de atributos del bloque elegido
(defun _num:dlgatts ()
  (setq atts (if blks (_num:tags (nth (atoi (get_tile "blk")) blks))))
  (start_list "att")
  (if atts (mapcar 'add_list atts) (add_list "(sin atributos)"))
  (end_list)
  (set_tile "att" (itoa (cond ((vl-position (strcase (_num:g "ATT")) atts)) (0)))))

(defun _num:dlgplant (i / p)
  (if (> i 0)
    (progn
      (setq p (nth (1- i) *NUM:PLANT*))
      (set_tile "pre" (nth 1 p))
      (set_tile "suf" (nth 2 p))
      (set_tile "alt" (vl-princ-to-string (nth 3 p)))
      (set_tile "fig" (itoa (vl-position (nth 4 p) *NUM:FIGS*)))
      (set_tile "tip" "T")
      (set_tile "inc" "1")
      (set_tile "dig" "0")
      (if (= (car p) "Nota")
        (progn (set_tile "mod" "F") (set_tile "val" "") (mode_tile "pre" 2))
        (progn (set_tile "mod" "C") (set_tile "val" "1") (mode_tile "val" 2)))
      (_num:dlgmodos)
      (_num:dlgvista))))

(defun _num:dlgvalida ()
  (cond
    ((and (= (_num:g "MOD") "C")
          (not (or (_num:esnum (_num:g "VAL")) (_num:esalfa (_num:g "VAL")))))
      "Numero o letra: escribe un numero (1, 001) o letras (A, AB).")
    ((and (= (_num:g "MOD") "C")
          (or (not (_num:esnum (_num:g "INC"))) (= 0 (atoi (_num:g "INC")))))
      "Incremento: numero entero distinto de 0 (negativo = descendente).")
    ((not (and (_num:esnum (_num:g "DIG")) (>= (atoi (_num:g "DIG")) 0)))
      "Digitos: entero 0 o mayor.")
    ((not (_num:pos (_num:g "ALT"))) "Altura: numero mayor que 0 (usa punto decimal).")
    ((not (distof (_num:g "ROT") 2)) "Rotacion: numero en grados.")
    ((not (_num:pos (_num:g "HOL"))) "Holgura: numero mayor que 0.")
    ((not (_num:pos (_num:g "ESC"))) "Escala: numero mayor que 0.")
    ((not (and (_num:esnum (_num:g "LIM")) (>= (atoi (_num:g "LIM")) 0)))
      "Limite: entero 0 o mayor.")
    ((and (= (_num:g "TIP") "B")
          (or (= (_num:g "BLK") "") (not (tblsearch "BLOCK" (_num:g "BLK")))))
      "Bloque: elige un bloque del dibujo.")
    ((= (_num:etiqueta) "") "La etiqueta queda vacia: escribe prefijo, numero o sufijo.")))

(defun _num:dlgok ( / s v)
  (_num:dlgleer)
  (if (setq s (_num:dlgvalida))
    (set_tile "error" s)
    (progn
      (setq v (_num:g "VAL"))
      (if (and (_num:esnum v) (> (strlen v) 1) (= (substr v 1 1) "0"))
        (_num:s "DIG" (itoa (max (atoi (_num:g "DIG")) (strlen v)))))
      (done_dialog 1))))

;; Pone los valores actuales en el cuadro y asigna las acciones
(defun _num:dlginit ( / i)
  (if (and (= (_num:g "TIP") "B") (null blks)) (_num:s "TIP" "T"))
  (start_list "pla")
  (mapcar 'add_list (cons "(personalizada)" (mapcar 'car *NUM:PLANT*)))
  (end_list)
  (set_tile "pla" "0")
  (foreach k '(("pre" . "PRE") ("val" . "VAL") ("suf" . "SUF") ("inc" . "INC")
               ("dig" . "DIG") ("alt" . "ALT") ("rot" . "ROT") ("hol" . "HOL")
               ("esc" . "ESC") ("lim" . "LIM") ("mod" . "MOD") ("tip" . "TIP"))
    (set_tile (car k) (_num:g (cdr k))))
  (start_list "blk")
  (if blks (mapcar 'add_list blks) (add_list "(no hay bloques en el dibujo)"))
  (end_list)
  (set_tile "blk" (itoa (cond ((vl-position (_num:g "BLK") blks)) (0))))
  (if (null blks) (mode_tile "B" 1))
  (_num:dlgatts)
  (start_list "cap")
  (add_list (strcat "(capa actual: " (getvar 'clayer) ")"))
  (mapcar 'add_list caps)
  (end_list)
  (set_tile "cap"
    (itoa (if (and (/= (_num:g "CAP") "") (setq i (vl-position (_num:g "CAP") caps))) (1+ i) 0)))
  (start_list "fig")
  (mapcar 'add_list '("Ninguna" "Circulo" "Rectangulo"))
  (end_list)
  (set_tile "fig" (itoa (cond ((vl-position (_num:g "FIG") *NUM:FIGS*)) (0))))
  (action_tile "pla"    "(_num:dlgplant (atoi $value))")
  (foreach k '("pre" "val" "suf" "dig") (action_tile k "(_num:dlgvista)"))
  (action_tile "mod"    "(_num:dlgmodos) (_num:dlgvista)")
  (action_tile "tip"    "(_num:dlgmodos)")
  (action_tile "blk"    "(_num:dlgatts)")
  (action_tile "palt"   "(_num:dlgleer) (done_dialog 2)")
  (action_tile "prot"   "(_num:dlgleer) (done_dialog 3)")
  (action_tile "pblk"   "(_num:dlgleer) (done_dialog 4)")
  (action_tile "tabla"  "(_num:dlgleer) (done_dialog 5)")
  (action_tile "accept" "(_num:dlgok)")
  (action_tile "cancel" "(done_dialog 0)")
  (_num:dlgmodos)
  (_num:dlgvista))

;; Toma un bloque de la pantalla (boton < junto a Bloque)
(defun _num:dlgbloque ( / s o lst)
  (setq s (entsel "\nSelecciona el bloque a usar: "))
  (if (and s (= "INSERT" (cdr (assoc 0 (entget (car s))))))
    (progn
      (setq o (vlax-ename->vla-object (car s)))
      (_num:s "BLK" (vla-get-effectivename o))
      (_num:s "TIP" "B")
      (setq lst (_num:tags (_num:g "BLK")))
      (if (not (member (strcase (_num:g "ATT")) lst)) (_num:s "ATT" (if lst (car lst) ""))))
    (if s (princ "\nEso no es un bloque."))))

;; Muestra el cuadro. Devuelve "OK", "CANCEL", o nil si no se pudo abrir DCL.
(defun _num:dialogo ( / fn f id res bak blks atts caps l d)
  (setq bak *NUM*)
  (while (setq l (tblnext "LAYER" (null l)))
    (setq caps (cons (cdr (assoc 2 l)) caps)))
  (setq caps (acad_strlsort caps) l nil)
  (while (setq l (tblnext "BLOCK" (null l)))
    (if (and (not (wcmatch (cdr (assoc 2 l)) "`**,*|*"))
             (= 0 (logand 4 (cdr (assoc 70 l)))))
      (setq blks (cons (cdr (assoc 2 l)) blks))))
  (if blks (setq blks (acad_strlsort blks)))
  (setq fn (vl-filename-mktemp "numera.dcl"))
  (if (setq f (open fn "w"))
    (progn
      (foreach x *NUM:DCL* (write-line x f))
      (close f)
      (setq id (load_dialog fn) res 9)
      (if (<= id 0) (setq res nil))
      (while (and res (> res 1))
        (if (new_dialog "numera" id)
          (progn
            (_num:dlginit)
            (setq res (start_dialog))
            (cond
              ((= res 2)
                (if (setq d (getdist "\nAltura del texto (valor o dos puntos): "))
                  (_num:s "ALT" (rtos d 2 3))))
              ((= res 3) (_num:girar))
              ((= res 4) (_num:dlgbloque))
              ((= res 5) (_num:tabla))))
          (setq res nil)))
      (if (> id 0) (unload_dialog id))
      (vl-file-delete fn)))
  (cond
    ((null res) (princ "\nNo se pudo abrir el cuadro de dialogo; se usa la linea de comandos.") nil)
    ((= res 1) "OK")
    (t (setq *NUM* bak) "CANCEL")))

;;---------------------------- COMANDO ------------------------------------

(defun c:NUMERA ( / *error* doc ocmd pt puestos primero ultimo lim seguir r)
  (setq doc (vla-get-activedocument (vlax-get-acad-object)))

  (defun *error* (msg)
    (if ocmd (setvar 'cmdecho ocmd))
    (_num:guardar)
    (vla-endundomark doc)
    (if (and msg (not (wcmatch (strcase msg) "*BREAK*,*CANCEL*,*EXIT*")))
      (princ (strcat "\nError NUMERA: " msg)))
    (_num:final)
    (princ))

  (setq ocmd (getvar 'cmdecho) puestos 0 seguir t)
  (setvar 'cmdecho 0)
  (vla-startundomark doc)
  (_num:cargar)
  (if *NUM:USAR_DCL* (setq r (_num:dialogo)))
  (cond
    ((= r "CANCEL") (setq seguir nil))
    ((null r) (_num:resumen)))

  (while seguir
    (setq lim (atoi (_num:g "LIM")))
    (if (and (> lim 0) (>= puestos lim))
      (progn
        (princ (strcat "\nLimite de " (itoa lim) " etiquetas alcanzado."))
        (setq seguir nil))
      (progn
        (initget "Config Linea Plantilla Arreglo Reemplazar Ordenar Valor Girar Saltar Tabla")
        (setq pt (getpoint (strcat "\n[" (_num:etiqueta)
                   "] Punto o [Config/Linea/Plantilla/Arreglo/Reemplazar/Ordenar/Valor/Girar/Saltar/Tabla] <Salir>: ")))
        (cond
          ((null pt) (setq seguir nil))
          ((listp pt)
            (if (_num:colocar pt) (progn (_num:contar) (_num:avanzar))))
          ((= pt "Config")
            (if (not (and *NUM:USAR_DCL* (_num:dialogo))) (_num:config)))
          ((= pt "Linea")      (_num:config))
          ((= pt "Plantilla")  (_num:plantilla) (_num:resumen))
          ((= pt "Arreglo")    (_num:arreglo))
          ((= pt "Reemplazar") (_num:reemplazar))
          ((= pt "Ordenar")    (_num:ordenar))
          ((= pt "Valor")      (_num:valor))
          ((= pt "Girar")      (_num:girar))
          ((= pt "Tabla")      (_num:tabla))
          ((= pt "Saltar")
            (princ (strcat "\nSe salta [" (_num:etiqueta) "]"))
            (_num:avanzar))))))

  (*error* nil))

(princ "\nNUMERA v1.3 cargado. Comandos: NUMERA (numerar) y NUMTABLA (tabla de conteo).")
(princ)
