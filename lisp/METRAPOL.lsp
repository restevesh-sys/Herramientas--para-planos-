;;; =====================================================================
;;;  METRAPOL.LSP
;;;
;;;   METRAPOL -> Selecciona un poligono limite (LWPOLYLINE cerrada) y:
;;;                1) Suma la longitud de todo lo que es LINE, LWPOLYLINE,
;;;                   POLYLINE, ARC, CIRCLE, SPLINE o ELLIPSE que este
;;;                   dentro, agrupado por CAPA.
;;;                2) Cuenta todos los bloques (INSERT) que esten dentro,
;;;                   agrupados por NOMBRE de bloque (usa EffectiveName,
;;;                   asi que tambien funciona bien con bloques dinamicos).
;;;                3) Inserta dos tablas nativas de AutoCAD en el plano:
;;;                   - METRADO POR CAPA (con fila TOTAL)
;;;                   - CONTEO DE BLOQUES (con fila TOTAL)
;;;
;;;  La seleccion de "todo lo que esta dentro" usa el mismo criterio que
;;;  SELPOL (Crossing Polygon: toca o esta dentro del poligono).
;;;
;;;  Supone que las unidades del dibujo son metros (igual que tus cotas).
;;;
;;;  Cargar con APPLOAD o arrastrando el archivo a la ventana de AutoCAD.
;;; =====================================================================

(vl-load-com)

;; ---------------------------------------------------------------------
;; Vertices (WCS) de una LWPOLYLINE
;; ---------------------------------------------------------------------
(defun PH:VerticesLW (ent / el pts elev)
  (setq el (entget ent))
  (setq elev (cdr (assoc 38 el)))
  (if (null elev) (setq elev 0.0))
  (setq pts '())
  (foreach par el
    (if (= (car par) 10)
      (setq pts (cons (list (car (cdr par)) (cadr (cdr par)) elev) pts))
    )
  )
  (reverse pts)
)

;; ---------------------------------------------------------------------
;; Longitud total de cualquier curva (linea, polilinea abierta o
;; cerrada, arco, circulo, spline, elipse) usando el rango de parametro
;; completo -> funciona igual para curvas cerradas.
;; ---------------------------------------------------------------------
(defun PH:Longitud (obj / r)
  (setq r (vl-catch-all-apply
            (function (lambda ()
              (vlax-curve-getDistAtParam obj (vlax-curve-getEndParam obj))))
            nil))
  (if (or (null r) (vl-catch-all-error-p r)) 0.0 r)
)

;; ---------------------------------------------------------------------
;; Nombre "efectivo" de un bloque (respeta bloques dinamicos)
;; ---------------------------------------------------------------------
(defun PH:NombreBloque (obj / r)
  (setq r (vl-catch-all-apply 'vlax-get (list obj 'EffectiveName)))
  (if (or (vl-catch-all-error-p r) (null r) (= r ""))
    (vlax-get obj 'Name)
    r
  )
)

;; ---------------------------------------------------------------------
;; Acumula valor en una lista de asociacion (clave . valor)
;; ---------------------------------------------------------------------
(defun PH:Acumula (lst clave valor / par)
  (setq par (assoc clave lst))
  (if par
    (subst (cons clave (+ (cdr par) valor)) par lst)
    (cons (cons clave valor) lst)
  )
)

;; =======================================================================
;; COMANDO: METRAPOL
;; =======================================================================
(defun C:METRAPOL ( / poloent pts ss n i ent obj etype capas bloques
                       ip1 ip2 doc ms tbl r total par)

  (setq poloent (car (entsel "\nSeleccione el POLIGONO limite (LWPOLYLINE cerrada): ")))
  (if (null poloent) (progn (princ "\nNo se selecciono poligono.") (exit)))
  (if (/= (cdr (assoc 0 (entget poloent))) "LWPOLYLINE")
    (progn (princ "\nEl limite debe ser una LWPOLYLINE.") (exit))
  )

  (setq pts (PH:VerticesLW poloent))
  (if (< (length pts) 3)
    (progn (princ "\nEl poligono no tiene suficientes vertices.") (exit))
  )

  (setq ss (ssget "_CP" pts))
  (if (null ss)
    (progn (princ "\nNo se encontro nada dentro del poligono.") (exit))
  )

  (setq capas '())   ; ((nombre-capa . longitud-acumulada) ...)
  (setq bloques '()) ; ((nombre-bloque . cantidad) ...)

  (setq n (sslength ss))
  (setq i 0)
  (repeat n
    (setq ent (ssname ss i))
    (if (not (equal ent poloent))
      (progn
        (setq etype (cdr (assoc 0 (entget ent))))
        (cond
          ((member etype '("LINE" "LWPOLYLINE" "POLYLINE" "ARC" "CIRCLE" "SPLINE" "ELLIPSE"))
           (setq obj (vlax-ename->vla-object ent))
           (setq capas (PH:Acumula capas (cdr (assoc 8 (entget ent))) (PH:Longitud obj)))
          )
          ((= etype "INSERT")
           (setq obj (vlax-ename->vla-object ent))
           (setq bloques (PH:Acumula bloques (PH:NombreBloque obj) 1))
          )
        )
      )
    )
    (setq i (1+ i))
  )

  (if (and (null capas) (null bloques))
    (progn (princ "\nNo se encontraron lineas ni bloques dentro del poligono.") (exit))
  )

  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq ms (vla-get-ModelSpace doc))

  ;; ---- Tabla de metrado por capa ----
  (if capas
    (progn
      (setq capas (vl-sort capas (function (lambda (a b) (< (car a) (car b))))))
      (setq ip1 (getpoint "\nPunto de insercion para la tabla METRADO POR CAPA: "))
      (setq tbl (vla-AddTable ms (vlax-3d-point ip1) (+ (length capas) 3) 2 8.0 40.0))
      (vla-SetText tbl 0 0 "METRADO POR CAPA")
      (vla-mergecells tbl 0 0 0 1)
      (vla-SetText tbl 1 0 "Capa")
      (vla-SetText tbl 1 1 "Longitud (m)")
      (setq r 2 total 0.0)
      (foreach par capas
        (vla-SetText tbl r 0 (car par))
        (vla-SetText tbl r 1 (rtos (cdr par) 2 2))
        (setq total (+ total (cdr par)))
        (setq r (1+ r))
      )
      (vla-SetText tbl r 0 "TOTAL")
      (vla-SetText tbl r 1 (rtos total 2 2))
    )
  )

  ;; ---- Tabla de conteo de bloques ----
  (if bloques
    (progn
      (setq bloques (vl-sort bloques (function (lambda (a b) (< (car a) (car b))))))
      (setq ip2 (getpoint "\nPunto de insercion para la tabla CONTEO DE BLOQUES: "))
      (setq tbl (vla-AddTable ms (vlax-3d-point ip2) (+ (length bloques) 3) 2 8.0 40.0))
      (vla-SetText tbl 0 0 "CONTEO DE BLOQUES")
      (vla-mergecells tbl 0 0 0 1)
      (vla-SetText tbl 1 0 "Bloque")
      (vla-SetText tbl 1 1 "Cantidad")
      (setq r 2 total 0)
      (foreach par bloques
        (vla-SetText tbl r 0 (car par))
        (vla-SetText tbl r 1 (itoa (cdr par)))
        (setq total (+ total (cdr par)))
        (setq r (1+ r))
      )
      (vla-SetText tbl r 0 "TOTAL")
      (vla-SetText tbl r 1 (itoa total))
    )
  )

  (princ "\nListo: tabla(s) generada(s).")
  (princ)
)

(princ "\nComando cargado: METRAPOL")
(princ)
