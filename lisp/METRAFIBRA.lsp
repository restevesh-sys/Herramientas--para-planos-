;;; ============================================================
;;;  METRAFIBRA.LSP
;;;  Comando: METRAFIBRA
;;;  Crea una tabla nativa de AutoCAD con el metrado (longitudes)
;;;  de las redes de fibra optica seleccionadas, agrupando por
;;;  CAPA (layer): Nombre de red, Longitud total (m) y Cantidad
;;;  de elementos.
;;;
;;;  Requiere AutoCAD completo (usa ActiveX/VLA, no funciona en
;;;  AutoCAD LT).
;;; ============================================================

(vl-load-com)

(defun c:METRAFIBRA (/ *error* olderr ss n i ent obj lay len data
                        item factor pt doc ms tbl nrows r
                        totlen totcnt row)

  (setq olderr *error*)
  (defun *error* (msg)
    (if (and msg
             (/= msg "Function cancelled")
             (/= msg "quit / exit abort"))
      (princ (strcat "\n[METRAFIBRA] Error: " msg))
    )
    (setvar "CMDECHO" 1)
    (setq *error* olderr)
    (princ)
  )

  (setvar "CMDECHO" 0)
  (setq doc (vla-get-ActiveDocument (vlax-get-acad-object)))
  (setq ms  (vla-get-ModelSpace doc))

  (princ "\n>> Seleccione las redes de fibra (LINE / LWPOLYLINE / POLYLINE / ARC / SPLINE): ")
  (setq ss (ssget '((0 . "LINE,LWPOLYLINE,POLYLINE,ARC,SPLINE"))))

  (if (not ss)
    (princ "\nNo se selecciono ningun objeto. Comando cancelado.")
    (progn
      (setq factor (getreal "\nFactor de conversion a metros [ej. si el dibujo esta en mm ingrese 0.001] <1.0>: "))
      (if (or (not factor) (<= factor 0.0)) (setq factor 1.0))

      ;; --- Recorrer seleccion y acumular longitudes por capa ---
      (setq data nil)
      (setq n (sslength ss))
      (setq i 0)
      (while (< i n)
        (setq ent (ssname ss i))
        (setq obj (vlax-ename->vla-object ent))
        (setq lay (vla-get-Layer obj))
        (setq len
          (vl-catch-all-apply
            'vlax-curve-getDistAtParam
            (list obj (vlax-curve-getEndParam obj))))
        (if (vl-catch-all-error-p len)
          (setq len 0.0)
          (setq len (* factor len))
        )
        (setq item (assoc lay data))
        (if item
          (setq data (subst
                       (list lay (+ (cadr item) len) (1+ (caddr item)))
                       item data))
          (setq data (append data (list (list lay len 1))))
        )
        (setq i (1+ i))
      )

      ;; --- Ordenar alfabeticamente por capa ---
      (setq data (vl-sort data (function (lambda (a b) (< (car a) (car b))))))

      (setq pt (getpoint "\nPunto de insercion de la tabla: "))
      (if (not pt) (setq pt '(0.0 0.0 0.0)))

      ;; --- Crear tabla: encabezado + filas de datos + total ---
      (setq nrows (+ (length data) 2))
      (setq tbl (vla-AddTable ms (vlax-3d-point pt) nrows 3 8.0 60.0))
      (vla-put-TitleSuppressed tbl :vlax-true)
      (vla-put-HeaderSuppressed tbl :vlax-true)

      (vla-SetColumnWidth tbl 0 90.0)
      (vla-SetColumnWidth tbl 1 55.0)
      (vla-SetColumnWidth tbl 2 45.0)

      ;; --- Encabezado (fila 0) ---
      (vla-SetText tbl 0 0 "RED / CAPA")
      (vla-SetText tbl 0 1 "LONGITUD (m)")
      (vla-SetText tbl 0 2 "CANTIDAD")

      ;; --- Filas de datos ---
      (setq totlen 0.0 totcnt 0 r 1)
      (foreach item data
        (vla-SetText tbl r 0 (car item))
        (vla-SetText tbl r 1 (rtos (cadr item) 2 2))
        (vla-SetText tbl r 2 (itoa (caddr item)))
        (setq totlen (+ totlen (cadr item)))
        (setq totcnt (+ totcnt (caddr item)))
        (setq r (1+ r))
      )

      ;; --- Fila de totales ---
      (vla-SetText tbl r 0 "TOTAL")
      (vla-SetText tbl r 1 (rtos totlen 2 2))
      (vla-SetText tbl r 2 (itoa totcnt))

      ;; --- Alineacion centrada en todas las celdas ---
      (setq row 0)
      (while (< row nrows)
        (vla-SetAlignment tbl row 0 5)
        (vla-SetAlignment tbl row 1 5)
        (vla-SetAlignment tbl row 2 5)
        (setq row (1+ row))
      )

      (vla-Regen doc acAllViewports)

      (princ (strcat "\nTabla creada. Total metrado: " (rtos totlen 2 2)
                      " m en " (itoa totcnt) " elemento(s)."))
    )
  )

  (setvar "CMDECHO" 1)
  (setq *error* olderr)
  (princ)
)

(princ "\n[METRAFIBRA] Comando cargado. Escriba METRAFIBRA para crear la tabla de metrados.")
(princ)
