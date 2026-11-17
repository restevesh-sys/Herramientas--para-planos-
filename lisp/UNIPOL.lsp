;;; UNIPOL.lsp
;;; Comando: UNIPOL
;;; Une varias entidades (líneas, arcos, polilíneas, splines) en UNA SOLA entidad
;;; y, opcionalmente, coloca un texto con la longitud total alineado a la línea.
;;;
;;; - Si hay SPLINES en la selección usa JOIN (los extremos deben tocarse).
;;; - Si no hay splines usa PEDIT > Multiple > Join con tolerancia (fuzz),
;;;   así une aunque haya pequeños huecos entre extremos.

(vl-load-com)

(defun uni:hay-spline (ss / i r)
  (setq i 0 r nil)
  (repeat (sslength ss)
    (if (= (cdr (assoc 0 (entget (ssname ss i)))) "SPLINE") (setq r T))
    (setq i (1+ i)))
  r
)

(defun uni:todas-lwpoly (ss / i r)
  (setq i 0 r T)
  (repeat (sslength ss)
    (if (/= (cdr (assoc 0 (entget (ssname ss i)))) "LWPOLYLINE") (setq r nil))
    (setq i (1+ i)))
  r
)

(defun c:UNIPOL ( / ss fuzz oldcmd e L pt prm der ang h txt resp)
  (prompt "\nSelecciona TODAS las entidades que quieres unir en una sola: ")
  (setq ss (ssget '((0 . "LINE,ARC,LWPOLYLINE,POLYLINE,SPLINE"))))
  (if (and ss (> (sslength ss) 1))
    (progn
      (setq oldcmd (getvar "CMDECHO"))
      (setvar "CMDECHO" 0)
      (if (uni:hay-spline ss)
        ;; con splines: JOIN
        (command "_.JOIN" ss "")
        ;; sin splines: PEDIT multiple con tolerancia
        (progn
          (setq fuzz (getdist "\nTolerancia para unir extremos <0.05>: "))
          (if (null fuzz) (setq fuzz 0.05))
          (if (uni:todas-lwpoly ss)
            (command "_.PEDIT" "_M" ss "" "_J" fuzz "")
            (command "_.PEDIT" "_M" ss "" "_Y" "_J" fuzz ""))
        )
      )
      (setvar "CMDECHO" oldcmd)

      ;; Anotar longitud
      (initget "Si No")
      (setq resp (getkword "\n¿Colocar texto con la longitud total? [Si/No] <Si>: "))
      (if (/= resp "No")
        (progn
          (setq e (car (entsel "\nSelecciona la entidad resultante: ")))
          (if (and e (vlax-curve-getEndParam e))
            (progn
              (setq L (vlax-curve-getDistAtParam e (vlax-curve-getEndParam e)))
              (setq pt (vlax-curve-getPointAtDist e (/ L 2.0)))
              (setq prm (vlax-curve-getParamAtPoint e pt))
              (setq der (vlax-curve-getFirstDeriv e prm))
              (setq ang (angle '(0.0 0.0 0.0) der))
              (if (and (> ang (/ pi 2.0)) (< ang (* 1.5 pi))) (setq ang (+ ang pi)))
              (setq h (getdist "\nAltura del texto <2.5>: "))
              (if (null h) (setq h 2.5))
              (setq txt (strcat (rtos L 2 0) " m"))
              (entmake (list '(0 . "TEXT")
                             (cons 10 (polar pt (+ ang (/ pi 2.0)) (* h 0.3)))
                             (cons 11 (polar pt (+ ang (/ pi 2.0)) (* h 0.3)))
                             (cons 40 h) (cons 1 txt)
                             (cons 50 ang)
                             '(72 . 1) '(73 . 1)))
              (prompt (strcat "\nLongitud total: " (rtos L 2 0)))
            )
            (prompt "\nNo se pudo leer la longitud de esa entidad.")
          )
        )
      )
      (prompt "\nListo.")
    )
    (prompt "\nSelecciona al menos 2 entidades.")
  )
  (princ)
)

(princ "\nUNIPOL cargado. Escribe UNIPOL para ejecutar.")
(princ)
