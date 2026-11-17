(vl-load-com)

;; ============================================================
;; UTILIDADES
;; ============================================================

(defun POL:Vertices (ent / datos elev pts)
  ;; Devuelve los vertices de una LWPOLYLINE en coordenadas WCS.
  (setq datos (entget ent)
        elev  (cond ((cdr (assoc 38 datos))) (0.0)))

  (foreach x datos
    (if (= 10 (car x))
      (setq pts
        (cons
          (trans
            (list (cadr x) (caddr x) elev)
            ent
            0
          )
          pts
        )
      )
    )
  )
  (reverse pts)
)

(defun POL:CerradaP (ent / datos)
  ;; Comprueba que la entidad sea una LWPOLYLINE cerrada.
  (setq datos (entget ent))
  (and
    (= "LWPOLYLINE" (cdr (assoc 0 datos)))
    (= 1 (logand 1 (cdr (assoc 70 datos))))
  )
)

(defun POL:SeleccionarPoligono (/ ent)
  (while
    (progn
      (setq ent
        (car
          (entsel
            "\nSeleccione una polilinea cerrada: "
          )
        )
      )
      (cond
        ((null ent)
         nil
        )
        ((not (POL:CerradaP ent))
         (prompt
           "\nLa entidad seleccionada no es una LWPOLYLINE cerrada."
         )
         T
        )
        (T nil)
      )
    )
  )
  ent
)

;; ============================================================
;; COMANDO 1: RECORTAR EN EL LIMITE DEL POLIGONO
;; Conserva los objetos interiores y elimina los exteriores.
;;
;; Requiere que EXTRIM este disponible en la instalacion.
;; ============================================================

(defun c:RECORTAPOL (/ *error* ent pto cmdEcho)

  (setq cmdEcho (getvar "CMDECHO"))

  (defun *error* (msg)
    (setvar "CMDECHO" cmdEcho)
    (if
      (and msg
           (not
             (member msg
               '("Function cancelled" "quit / exit abort")
             )
           )
      )
      (prompt (strcat "\nError: " msg))
    )
    (princ)
  )

  (setvar "CMDECHO" 0)

  (if (setq ent (POL:SeleccionarPoligono))
    (progn
      (setq pto
        (getpoint
          "\nIndique un punto FUERA del poligono: "
        )
      )

      (if pto
        (progn
          ;; EXTRIM recorta todos los objetos del lado indicado.
          ;; Al elegir un punto exterior, conserva el contenido interior.
          (command "_.EXTRIM" ent "" pto)
          (prompt
            "\nObjetos recortados en el limite del poligono."
          )
        )
      )
    )
  )

  (setvar "CMDECHO" cmdEcho)
  (princ)
)

;; ============================================================
;; COMANDO 2: SELECCIONAR OBJETOS BAJO EL POLIGONO
;;
;; CP = selecciona objetos interiores y objetos que cruzan.
;; WP = selecciona solamente objetos totalmente interiores.
;; ============================================================

(defun c:SELECPOL (/ ent pts modo ss filtro)

  (if (setq ent (POL:SeleccionarPoligono))
    (progn
      (setq pts (POL:Vertices ent))

      (initget "Cruce Interior")
      (setq modo
        (getkword
          "\nModo [Cruce/Interior] <Cruce>: "
        )
      )

      (if (null modo)
        (setq modo "Cruce")
      )

      ;; Excluye la polilinea limite de la seleccion inicial.
      (setq filtro
        (list
          (cons -4 "<NOT")
          (cons -1 ent)
          (cons -4 "NOT>")
        )
      )

      (setq ss
        (ssget
          (if (= modo "Interior") "_WP" "_CP")
          (mapcar
            '(lambda (p)
               (trans p 0 1)
             )
            pts
          )
          filtro
        )
      )

      (if ss
        (progn
          (sssetfirst nil ss)
          (prompt
            (strcat
              "\nSeleccionados "
              (itoa (sslength ss))
              " objetos."
            )
          )
        )
        (prompt
          "\nNo se encontraron objetos en el poligono."
        )
      )
    )
  )
  (princ)
)

;; ============================================================
;; COMANDO OPCIONAL: SELECCIONAR OBJETOS + POLIGONO
;; ============================================================

(defun c:SELECPOLTODO (/ ent pts ss)

  (if (setq ent (POL:SeleccionarPoligono))
    (progn
      (setq pts
        (mapcar
          '(lambda (p)
             (trans p 0 1)
           )
          (POL:Vertices ent)
        )
      )

      (setq ss (ssget "_CP" pts))

      (if ss
        (progn
          (ssadd ent ss)
          (sssetfirst nil ss)
          (prompt
            (strcat
              "\nSeleccionados "
              (itoa (sslength ss))
              " objetos, incluido el poligono."
            )
          )
        )
        (progn
          (setq ss (ssadd ent))
          (sssetfirst nil ss)
          (prompt "\nSolo se selecciono el poligono.")
        )
      )
    )
  )
  (princ)
)

(princ "\nComandos cargados: RECORTAPOL, SELECPOL y SELECPOLTODO.")
(princ)
