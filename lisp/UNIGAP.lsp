;;; UNIGAP.lsp
;;; Comando: UNIGAP
;;; Regla: lo que está pegado o muy cerca (extremos a menos de la distancia de
;;; unión) se UNE en una sola polilínea; lo que está separado por más que esa
;;; distancia queda como tramo aparte (ahí va un elemento).
;;;
;;; Para cada grupo de entidades unidas:
;;;   - queda UNA polilínea con vértices simplificados (tolerancia, 0 = solo extremos)
;;;   - se coloca su longitud (0 decimales) desfasada arriba como una cota
;;; Al final informa la separación más cercana de cada tramo con el siguiente
;;; (la distancia donde va el elemento).
;;;
;;; Las distancias están en unidades del dibujo (si dibujas en metros, 30 cm = 0.3).

(vl-load-com)

;; ---------- puntos de una entidad ----------
(defun uv:pts (e / ed tp r en ed2 L n i)
  (setq ed (entget e) tp (cdr (assoc 0 ed)) r nil)
  (cond
    ((= tp "LINE")
     (setq r (list (cdr (assoc 10 ed)) (cdr (assoc 11 ed)))))
    ((= tp "LWPOLYLINE")
     (setq r (mapcar 'cdr (vl-remove-if-not
                            (function (lambda (x) (= (car x) 10))) ed))))
    ((= tp "POLYLINE")
     (setq en (entnext e))
     (while (and en (/= (cdr (assoc 0 (setq ed2 (entget en)))) "SEQEND"))
       (if (= (cdr (assoc 0 ed2)) "VERTEX")
         (setq r (append r (list (cdr (assoc 10 ed2))))))
       (setq en (entnext en))))
    (T
     (setq L (vlax-curve-getDistAtParam e (vlax-curve-getEndParam e))
           n 40 i 0)
     (repeat (1+ n)
       (setq r (append r (list (vlax-curve-getPointAtDist e (* L (/ (float i) n))))))
       (setq i (1+ i)))))
  (mapcar (function (lambda (p) (list (car p) (cadr p) 0.0))) r)
)

(defun uv:dedup (pts tol / r)
  (foreach p pts
    (if (or (null r) (> (distance p (car r)) tol))
      (setq r (cons p r))))
  (reverse r)
)

;; ---------- agrupar por cercanía de extremos ----------
;; item = (ename puntos)
(defun uv:cerca (a b tol / ea eb)
  (setq ea (list (car (cadr a)) (last (cadr a)))
        eb (list (car (cadr b)) (last (cadr b))))
  (vl-some
    (function (lambda (p)
      (vl-some (function (lambda (q) (<= (distance p q) tol))) eb)))
    ea)
)

(defun uv:clusters (items tol / rest cl queue cur m res)
  (setq rest items res nil)
  (while rest
    (setq cl (list (car rest)) rest (cdr rest) queue cl)
    (while queue
      (setq cur (car queue) queue (cdr queue))
      (setq m (vl-remove-if-not
                (function (lambda (x) (uv:cerca cur x tol))) rest))
      (if m
        (progn
          (setq rest (vl-remove-if (function (lambda (x) (member x m))) rest))
          (setq cl (append cl m) queue (append queue m)))))
    (setq res (append res (list cl))))
  res
)

;; ---------- encadenar varias partes ----------
(defun uv:cadena (parts / chain rest cs ce best bd ps pe)
  (setq chain (car parts) rest (cdr parts))
  (while rest
    (setq cs (car chain) ce (last chain) best nil bd 1e99)
    (foreach p rest
      (setq ps (car p) pe (last p))
      (foreach c (list
                   (list (distance ce ps) (append chain p) p)
                   (list (distance ce pe) (append chain (reverse p)) p)
                   (list (distance cs pe) (append p chain) p)
                   (list (distance cs ps) (append (reverse p) chain) p))
        (if (< (car c) bd) (setq bd (car c) best c))))
    (setq chain (cadr best)
          rest (vl-remove (caddr best) rest)))
  chain
)

;; ---------- simplificación Douglas-Peucker ----------
(defun uv:sub (l a b / r i)
  (setq i 0 r nil)
  (foreach p l
    (if (and (>= i a) (<= i b)) (setq r (cons p r)))
    (setq i (1+ i)))
  (reverse r)
)

(defun uv:dseg (p a b / dx dy l2 u q)
  (setq dx (- (car b) (car a))
        dy (- (cadr b) (cadr a))
        l2 (+ (* dx dx) (* dy dy)))
  (if (< l2 1e-12)
    (distance p a)
    (progn
      (setq u (/ (+ (* (- (car p) (car a)) dx) (* (- (cadr p) (cadr a)) dy)) l2))
      (setq u (max 0.0 (min 1.0 u)))
      (setq q (list (+ (car a) (* u dx)) (+ (cadr a) (* u dy)) 0.0))
      (distance p q)))
)

(defun uv:dp (pts tol / n a b dmax idx i d)
  (setq n (length pts))
  (if (< n 3)
    pts
    (progn
      (setq a (car pts) b (last pts) dmax -1.0 idx 0 i 0)
      (foreach p pts
        (if (and (> i 0) (< i (1- n)))
          (progn
            (setq d (uv:dseg p a b))
            (if (> d dmax) (setq dmax d idx i))))
        (setq i (1+ i)))
      (if (> dmax tol)
        (append (uv:dp (uv:sub pts 0 idx) tol)
                (cdr (uv:dp (uv:sub pts idx (1- n)) tol)))
        (list a b))))
)

;; ---------- longitud y texto tipo cota ----------
(defun uv:largo (ent)
  (vlax-curve-getDistAtParam ent (vlax-curve-getEndParam ent))
)

(defun uv:etiqueta (ent lay h / L pt prm der ang pt2)
  (setq L   (uv:largo ent)
        pt  (vlax-curve-getPointAtDist ent (/ L 2.0))
        prm (vlax-curve-getParamAtPoint ent pt)
        der (vlax-curve-getFirstDeriv ent prm)
        ang (angle '(0.0 0.0 0.0) der))
  (if (and (> ang (/ pi 2.0)) (< ang (* 1.5 pi))) (setq ang (+ ang pi)))
  (setq pt2 (polar pt (+ ang (/ pi 2.0)) (* h 0.3)))
  (entmake (list '(0 . "TEXT") (cons 8 lay)
                 (cons 10 pt2) (cons 11 pt2)
                 (cons 40 h) (cons 1 (strcat (rtos L 2 0) " m"))
                 (cons 50 ang) '(72 . 1) '(73 . 1)))
  L
)

;; ---------- comando ----------
(defun c:UNIGAP ( / ss i e items cls cl lay chain new jt tol resp h res k j
                    r1 r2 dmin d L ref ed blk lyr sx sy sz gmax ends cands
                    used n c a b pa pb mid rot0 vx vy dirang oa ob)
  (prompt "\nSelecciona los tramos (sin los elementos): ")
  (setq ss (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE,ARC,SPLINE"))))
  (if ss
    (progn
      (setq jt (getdist "\nDistancia máxima para UNIR extremos (más que eso = separación) <0.05>: "))
      (if (null jt) (setq jt 0.05))
      (setq tol (getreal "\nTolerancia de simplificación (0 = solo inicio y fin) <0.5>: "))
      (if (null tol) (setq tol 0.5))
      (initget "Si No")
      (setq resp (getkword "\n¿Colocar texto con la longitud en cada tramo? [Si/No] <Si>: "))
      (if (/= resp "No")
        (progn
          (setq h (getdist "\nAltura del texto <2.5>: "))
          (if (null h) (setq h 2.5))))

      ;; elemento (bloque) a colocar en cada separación
      (setq ref (entsel "\nSelecciona el elemento (bloque) a poner en cada separación <Enter = ninguno>: "))
      (if ref
        (progn
          (setq ed (entget (car ref)))
          (if (= (cdr (assoc 0 ed)) "INSERT")
            (progn
              (setq blk (cdr (assoc 2 ed)) lyr (cdr (assoc 8 ed))
                    sx (cdr (assoc 41 ed)) sy (cdr (assoc 42 ed)) sz (cdr (assoc 43 ed)))
              (setq gmax (getdist "\nSeparación máxima donde va el elemento <0.6>: "))
              (if (null gmax) (setq gmax 0.6))
              (setq rot0 (getreal "\nGiro adicional del bloque en grados (0 = como está dibujado) <0>: "))
              (if (null rot0) (setq rot0 0.0)))
            (prompt "\nEso no es un bloque; no se insertará elemento."))))

      ;; items = (ename puntos)
      (setq items nil i 0)
      (repeat (sslength ss)
        (setq e (ssname ss i) i (1+ i))
        (setq chain (uv:dedup (uv:pts e) 1e-6))
        (if (>= (length chain) 2)
          (setq items (append items (list (list e chain))))))

      (setq cls (uv:clusters items jt) res nil)
      (foreach cl cls
        (setq lay (cdr (assoc 8 (entget (car (car cl))))))
        (setq chain (uv:dedup (uv:cadena (mapcar 'cadr cl)) 1e-6))
        (if (<= tol 0.0)
          (setq chain (list (car chain) (last chain)))
          (setq chain (uv:dp chain tol)))
        (setq new
          (entmakex
            (append
              (list '(0 . "LWPOLYLINE") '(100 . "AcDbEntity") (cons 8 lay)
                    '(100 . "AcDbPolyline") (cons 90 (length chain)) '(70 . 0))
              (mapcar (function (lambda (p) (list 10 (car p) (cadr p)))) chain))))
        (if new
          (progn
            (foreach it cl (entdel (car it)))
            (if (/= resp "No")
              (setq L (uv:etiqueta new lay h))
              (setq L (uv:largo new)))
            (setq res (append res (list (list (car chain) (last chain) L (angle (cadr chain) (car chain)) (angle (nth (- (length chain) 2) chain) (last chain)))))))))

      ;; insertar el elemento en cada separación
      (if blk
        (progn
          (setq ends nil k 0)
          (foreach r1 res
            (setq ends (append ends (list (list (1+ (* 2 k)) k (car r1) (nth 3 r1))
                                          (list (+ 2 (* 2 k)) k (cadr r1) (nth 4 r1)))))
            (setq k (1+ k)))
          (setq cands nil)
          (foreach a ends
            (foreach b ends
              (if (and (< (car a) (car b)) (/= (cadr a) (cadr b)))
                (progn
                  (setq d (distance (caddr a) (caddr b)))
                  (if (and (> d jt) (<= d gmax))
                    (setq cands (append cands
                                  (list (list d (car a) (car b) (caddr a) (caddr b) (nth 3 a) (nth 3 b))))))))))
          (setq cands (vl-sort cands (function (lambda (x y) (< (car x) (car y))))))
          (setq used nil n 0)
          (foreach c cands
            (if (and (not (member (cadr c) used)) (not (member (caddr c) used)))
              (progn
                (setq used (append used (list (cadr c) (caddr c))))
                (setq pa (nth 3 c) pb (nth 4 c))
                ;; dirección de los tramos que llegan al elemento
                (setq oa (nth 5 c) ob (nth 6 c)
                      vx (+ (cos oa) (cos (+ ob pi)))
                      vy (+ (sin oa) (sin (+ ob pi)))
                      dirang (if (< (+ (abs vx) (abs vy)) 1e-6) oa (atan vy vx)))
                (setq mid (list (/ (+ (car pa) (car pb)) 2.0)
                                (/ (+ (cadr pa) (cadr pb)) 2.0) 0.0))
                (entmake (list '(0 . "INSERT") (cons 2 blk) (cons 8 lyr) (cons 10 mid)
                               (cons 41 sx) (cons 42 sy) (cons 43 sz)
                               (cons 50 (+ dirang (* rot0 (/ pi 180.0))))))
                (setq n (1+ n)))))
          (prompt (strcat "\nElementos insertados: " (itoa n)))))

      ;; informe de separaciones
      (prompt (strcat "\nListo: " (itoa (length res)) " tramo(s)."))
      (setq k 0)
      (foreach r1 res
        (setq k (1+ k) dmin nil j 0)
        (foreach r2 res
          (setq j (1+ j))
          (if (/= j k)
            (foreach p (list (car r1) (cadr r1))
              (foreach q (list (car r2) (cadr r2))
                (setq d (distance p q))
                (if (or (null dmin) (< d dmin)) (setq dmin d))))))
        (prompt (strcat "\n  Tramo " (itoa k) ": longitud " (rtos (caddr r1) 2 0)
                        (if dmin
                          (strcat " | separación más cercana: " (rtos dmin 2 2))
                          ""))))
    )
    (prompt "\nNo se seleccionó nada."))
  (princ)
)

(princ "\nUNIGAP cargado. Escribe UNIGAP para ejecutar.")
(princ)
