;;; UNIARCO.lsp
;;; Comando: UNIARCO
;;; Toma muchas entidades pequeñas (cuadritos, sólidos, bloques, polilíneas...)
;;; dispuestas a lo largo de una curva y las reemplaza por UN SOLO ARCO
;;; (o una LINEA si quedan casi rectas), pasando por el inicio, el medio y el final.
;;; Después puedes acotar con DIMARC / DIMENSIÓN o consultar la longitud con LIST.

(vl-load-com)

(defun uni:centro (ent / obj mn mx)
  (setq obj (vlax-ename->vla-object ent))
  (vla-getboundingbox obj 'mn 'mx)
  (setq mn (vlax-safearray->list mn)
        mx (vlax-safearray->list mx))
  (list (/ (+ (car mn) (car mx)) 2.0)
        (/ (+ (cadr mn) (cadr mx)) 2.0)
        0.0)
)

(defun c:UNIARCO ( / ss i pts n d dmax pa pb p q dx dy L ord pm cross borrar oldos oldcmd)
  (prompt "\nSelecciona los cuadritos/entidades a unir en un solo arco: ")
  (setq ss (ssget))
  (if (and ss (> (sslength ss) 2))
    (progn
      ;; centros de cada entidad
      (setq i 0 pts nil)
      (repeat (sslength ss)
        (setq pts (cons (uni:centro (ssname ss i)) pts)
              i (1+ i)))
      (setq n (length pts))

      ;; los dos puntos más alejados = extremos del arco
      (setq dmax -1.0)
      (foreach p pts
        (foreach q pts
          (setq d (distance p q))
          (if (> d dmax) (setq dmax d pa p pb q))))

      ;; ordenar los puntos a lo largo de la cuerda y tomar el del medio
      (setq dx (- (car pb) (car pa))
            dy (- (cadr pb) (cadr pa))
            L  (sqrt (+ (* dx dx) (* dy dy))))
      (setq ord (vl-sort pts
                  (function
                    (lambda (a b)
                      (< (+ (* (- (car a) (car pa)) dx) (* (- (cadr a) (cadr pa)) dy))
                         (+ (* (- (car b) (car pa)) dx) (* (- (cadr b) (cadr pa)) dy)))))))
      (setq pm (nth (/ n 2) ord))

      ;; ¿los 3 puntos son casi colineales?
      (setq cross (abs (- (* (- (car pm) (car pa)) dy)
                          (* (- (cadr pm) (cadr pa)) dx))))

      (initget "Si No")
      (setq borrar (getkword "\n¿Borrar las entidades originales? [Si/No] <Si>: "))

      (setq oldos (getvar "OSMODE")
            oldcmd (getvar "CMDECHO"))
      (setvar "CMDECHO" 0)
      (setvar "OSMODE" 0)
      (if (< (/ cross L) 1e-6)
        (command "_.LINE" pa pb "")
        (command "_.ARC" pa pm pb))
      (setvar "OSMODE" oldos)
      (setvar "CMDECHO" oldcmd)

      (if (/= borrar "No")
        (command "_.ERASE" ss ""))
      (prompt "\nListo: se creó una sola entidad. Usa DIMARC o LIST para medirla.")
    )
    (prompt "\nSelecciona al menos 3 entidades.")
  )
  (princ)
)

(princ "\nUNIARCO cargado. Escribe UNIARCO para ejecutar.")
(princ)
