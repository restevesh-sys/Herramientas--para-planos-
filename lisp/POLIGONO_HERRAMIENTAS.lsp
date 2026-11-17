;;; =====================================================================
;;;  POLIGONO_HERRAMIENTAS.LSP
;;;  Dos comandos para trabajar con poligonos (LWPOLYLINE) sobre un
;;;  plano de infraestructura de redes:
;;;
;;;   CORTAPOL  -> Corta (rompe) todo lo que cruza el limite del poligono
;;;                exactamente en el punto de cruce, sin eliminar nada,
;;;                solo separa la entidad en dos en ese punto.
;;;
;;;   SELPOL    -> Selecciona el poligono y automaticamente selecciona
;;;                todo lo que esta dentro de el (lineas, texto,
;;;                bloques, polilineas, lo que sea).
;;;
;;;  Cargar con APPLOAD o arrastrando el archivo a la ventana de AutoCAD.
;;; =====================================================================

(vl-load-com)

(if (not (boundp 'acExtendNone)) (setq acExtendNone 0))

;; ---------------------------------------------------------------------
;; Devuelve lista de puntos de interseccion (WCS) entre dos objetos VLA
;; ---------------------------------------------------------------------
(defun PH:Intersecciones (obj1 obj2 / result pts n i)
  (setq result (vl-catch-all-apply
                 'vlax-invoke
                 (list obj1 'IntersectWith obj2 acExtendNone)))
  (if (or (null result) (vl-catch-all-error-p result))
    '()
    (progn
      (setq pts '())
      (setq n (length result))
      (setq i 0)
      (while (< i n)
        (setq pts (cons (list (nth i result)
                               (nth (1+ i) result)
                               (nth (+ i 2) result))
                         pts))
        (setq i (+ i 3))
      )
      pts
    )
  )
)

;; ---------------------------------------------------------------------
;; Extrae los vertices (WCS, en el plano de la entidad) de una LWPOLYLINE
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

;; =======================================================================
;; COMANDO 1: CORTAPOL
;; Selecciona un poligono limite y luego las lineas/curvas a cortar.
;; Cada objeto se rompe exactamente en cada punto donde cruza el poligono.
;; =======================================================================
(defun C:CORTAPOL ( / poloent polobj lsel i ent obj ipts distlist dp pt)

  (setq poloent (car (entsel "\nSeleccione el POLIGONO limite (polilinea cerrada): ")))
  (if (null poloent)
    (progn (princ "\nNo se selecciono poligono.") (exit))
  )
  (setq polobj (vlax-ename->vla-object poloent))

  (princ "\nSeleccione las lineas/polilineas/arcos a cortar: ")
  (setq lsel (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE,ARC,CIRCLE,SPLINE"))))
  (if (null lsel)
    (progn (princ "\nNo se selecciono nada.") (exit))
  )

  (setq i 0)
  (repeat (sslength lsel)
    (setq ent (ssname lsel i))
    (if (not (equal ent poloent))
      (progn
        (setq obj (vlax-ename->vla-object ent))
        (setq ipts (PH:Intersecciones obj polobj))
        (if ipts
          (progn
            ;; Distancia de cada punto de interseccion desde el inicio
            ;; de la curva, para poder cortar de mas lejos a mas cerca
            ;; y que el ename original siga siendo valido en cada corte.
            (setq distlist '())
            (foreach pt ipts
              (setq distlist
                (cons (list (vlax-curve-getDistAtPoint obj pt) pt) distlist))
            )
            (setq distlist
              (vl-sort distlist (function (lambda (a b) (> (car a) (car b))))))

            (foreach dp distlist
              (setq pt (cadr dp))
              (command "_.break" ent "_f" pt pt)
            )
          )
        )
      )
    )
    (setq i (1+ i))
  )
  (princ "\nCorte terminado.")
  (princ)
)

;; =======================================================================
;; COMANDO 2: SELPOL
;; Selecciona un poligono y automaticamente selecciona todo lo que
;; esta dentro (cualquier tipo de entidad: texto, bloques, lineas...).
;; Usa seleccion "Crossing Polygon" (toca o esta dentro).
;; Si prefieres solo lo que esta 100% adentro, cambia "_CP" por "_WP".
;; =======================================================================
(defun C:SELPOL ( / poloent pts ss)

  (setq poloent (car (entsel "\nSeleccione el POLIGONO: ")))
  (if (null poloent)
    (progn (princ "\nNo se selecciono poligono.") (exit))
  )

  (if (/= (cdr (assoc 0 (entget poloent))) "LWPOLYLINE")
    (progn
      (princ "\nEl objeto seleccionado debe ser una polilinea (LWPOLYLINE).")
      (exit)
    )
  )

  (setq pts (PH:VerticesLW poloent))

  (if (< (length pts) 3)
    (progn (princ "\nEl poligono no tiene suficientes vertices.") (exit))
  )

  ;; ssget necesita una lista plana de puntos 2D/3D
  (setq ss (ssget "_CP" pts))

  (if ss
    (progn
      (sssetfirst nil ss)
      (princ (strcat "\n" (itoa (sslength ss)) " objeto(s) seleccionado(s) dentro del poligono."))
    )
    (princ "\nNo se encontraron objetos dentro del poligono.")
  )
  (princ)
)

(princ "\nComandos cargados: CORTAPOL y SELPOL")
(princ)
