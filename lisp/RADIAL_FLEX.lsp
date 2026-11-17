(defun c:RADIAL_FLEX (/ p_poste p_dest prefijo contador tam_flecha txt_h p_texto txt_final suma_metros str_inicio parsed)
  (vl-load-com)
  (setvar "CMDECHO" 0)
  (setvar "OSMODE" 0)

  ;; 1. Configuracion de Capa Tecnica
  (if (not (tblsearch "LAYER" "ELEC_RADIALES"))
    (command "-layer" "m" "ELEC_RADIALES" "c" "4" "" "")
  )
  (setvar "CLAYER" "ELEC_RADIALES")

  ;; 2. Punto de origen (unico, fijo)
  (setq p_poste (getpoint "\n1. Selecciona el PUNTO DE ORIGEN (Poste/Registro): "))

  (setq tam_flecha 0.35)
  (setq txt_h 0.25)
  (setq suma_metros 0.0)

  ;; Numero y prefijo de inicio, en un solo texto (ej: 1Dp, 3Ed)
  (setq str_inicio (getstring T "\nIndica numero y letras de INICIO (ej: 1Dp): "))
  (setq parsed (parse_num_prefix str_inicio))
  (setq contador (car parsed))
  (setq prefijo (cadr parsed))

  (princ "\n--- Solo selecciona cada punto. El numero se autoincrementa solo. ENTER o ESC para terminar ---")

  ;; 3. Bucle continuo de enumeracion (autoincremento, sin volver a preguntar el prefijo)
  (while (setq p_dest (getpoint p_poste "\nSelecciona el punto del edificio/departamento: "))

    (setq txt_final (strcat (itoa contador) prefijo))

    (command "_.line" p_poste p_dest "")
    (setq suma_metros (+ suma_metros (distance p_poste p_dest)))
    (draw_arrow_r p_dest p_poste tam_flecha)
    (draw_label_circle p_dest p_poste txt_final txt_h)

    (princ (strcat "\n-> Etiqueta colocada: " txt_final))
    (setq contador (1+ contador))
  )

  ;; 4. Insercion de Resumen Tecnico (Escala 1)
  (if (> contador 1)
    (progn
      (setq p_texto (getpoint "\nClic para ubicar el resumen: "))
      (setq txt_final (strcat (itoa (1- contador)) " puntos, " (rtos suma_metros 2 2) " m"))

      (entmake (list
        '(0 . "TEXT")
        (cons 10 p_texto)
        (cons 40 1.0)
        (cons 1 txt_final)
        '(50 . 0.0)
        (cons 8 "ELEC_RADIALES")
      ))
      (princ (strcat "\nResumen generado: " txt_final))
    )
    (princ "\nNo se coloco ningun punto.")
  )

  (setvar "OSMODE" 16383)
  (princ)
)

;; Separa el numero inicial de las letras del prefijo (ej: "1Dp" -> (1 "Dp"))
(defun parse_num_prefix (str / i numstr prefstr)
  (setq i 1)
  (setq numstr "")
  (while (and (<= i (strlen str)) (wcmatch (substr str i 1) "#"))
    (setq numstr (strcat numstr (substr str i 1)))
    (setq i (1+ i))
  )
  (setq prefstr (substr str i))
  (if (= numstr "") (setq numstr "1"))
  (list (atoi numstr) prefstr)
)

;; Punta de flecha radial (igual que antes)
(defun draw_arrow_r (p_dest p_orig tam / ang)
  (setq ang (angle p_orig p_dest))
  (command "_.pline"
    (polar p_dest (+ ang (* pi 0.82)) tam)
    p_dest
    (polar p_dest (- ang (* pi 0.82)) tam)
    "")
)

;; Etiqueta circular con el numero+prefijo, desplazada mas alla de la punta de flecha
(defun draw_label_circle (p_dest p_orig texto txt_h / ang radio p_centro)
  (setq ang (angle p_orig p_dest))
  (setq radio (+ (* (strlen texto) txt_h 0.35) (* txt_h 0.6)))
  (setq p_centro (polar p_dest ang (* radio 1.4)))

  (entmake (list
    '(0 . "CIRCLE")
    (cons 10 p_centro)
    (cons 40 radio)
    (cons 8 "ELEC_RADIALES")
  ))

  (entmake (list
    '(0 . "TEXT")
    (cons 10 p_centro)
    (cons 11 p_centro)
    (cons 40 txt_h)
    (cons 1 texto)
    '(50 . 0.0)
    (cons 72 1)
    (cons 73 2)
    (cons 8 "ELEC_RADIALES")
  ))
)
