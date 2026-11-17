;;; ============================================================
;;; ACOM_REGISTRO.LSP
;;; Automatiza el trazo de acometidas desde un REGISTRO hacia los
;;; lotes a su izquierda y derecha, sobre una banqueta horizontal.
;;;
;;; - La punta de flecha es un BLOQUE (PUNTA_ACOM), no polilinea
;;;   suelta, para no interferir en metrados de area/longitud.
;;; - La profundidad maxima de penetracion en cada lote es de 2 m
;;;   (o el valor que definas, con tope de 2 m).
;;;
;;; USO:
;;;   Comando: ACOM_REGISTRO
;;;   1) Selecciona el punto del REGISTRO (sobre la banqueta)
;;;   2) Indica cuantos lotes alimenta a la IZQUIERDA
;;;   3) Indica cuantos lotes alimenta a la DERECHA
;;;   4) Indica el ancho de lote estandar (ej. 8.5)
;;;   5) Si el primero/ultimo lote de cada lado es de esquina,
;;;      te pregunta su ancho especial (ej. 10)
;;;   6) Indica hacia que lado esta la fachada de los lotes:
;;;      Arriba (1) o Abajo (-1) respecto a la banqueta
;;;   7) Se repite el proceso mientras quieras seguir marcando
;;;      registros (Enter para salir)
;;; ============================================================

(defun ACOM_CREA_BLOQUE ( / pt1 pt2 pt3)
  ;; Crea el bloque PUNTA_ACOM (triangulo solido) si no existe.
  ;; Base point del bloque = punta de la flecha (0,0)
  (if (not (tblsearch "BLOCK" "PUNTA_ACOM"))
    (progn
      (setq pt1 (list 0.0 0.0 0.0)          ; punta
            pt2 (list -0.35 0.12 0.0)       ; talon 1
            pt3 (list -0.35 -0.12 0.0))     ; talon 2
      (entmake
        (list
          (cons 0 "SOLID")
          (cons 8 "0")
          (cons 10 pt1)
          (cons 11 pt2)
          (cons 12 pt3)
          (cons 13 pt3)
        )
      )
      (command "._-block" "PUNTA_ACOM" "0,0,0" (entlast) "")
    )
  )
)

(defun ACOM_LAYER ( / )
  (if (not (tblsearch "LAYER" "ELEC_ACOMETIDAS"))
    (command "._-layer" "m" "ELEC_ACOMETIDAS" "c" "4" "" "")
  )
  (setvar "CLAYER" "ELEC_ACOMETIDAS")
)

(defun c:ACOM_REGISTRO ( / p_reg n_izq n_der ancho ancho_esq_i ancho_esq_d
                            lado prof old_osmode i acumulado ancho_i
                            p_frente p_punta ang blkname)

  (setq old_osmode (getvar "OSMODE"))
  (ACOM_LAYER)
  (ACOM_CREA_BLOQUE)
  (setq blkname "PUNTA_ACOM")

  ;; --- Datos generales (se piden una sola vez) ---
  (initget 7)
  (setq n_izq (getint "\nCantidad de lotes a alimentar hacia la IZQUIERDA: "))
  (initget 7)
  (setq n_der (getint "\nCantidad de lotes a alimentar hacia la DERECHA: "))
  (initget 7)
  (setq ancho (getdist "\nAncho de lote estandar (m), ej. 8.5: "))

  (initget "Si No")
  (setq ancho_esq_i (getkword "\nEl primer lote (extremo izquierdo) es de esquina? [Si/No] <No>: "))
  (if (= ancho_esq_i "Si")
    (progn (initget 7) (setq ancho_esq_i (getdist "\nAncho del lote de esquina izquierdo (m), ej. 10: ")))
    (setq ancho_esq_i ancho)
  )

  (initget "Si No")
  (setq ancho_esq_d (getkword "\nEl ultimo lote (extremo derecho) es de esquina? [Si/No] <No>: "))
  (if (= ancho_esq_d "Si")
    (progn (initget 7) (setq ancho_esq_d (getdist "\nAncho del lote de esquina derecho (m), ej. 10: ")))
    (setq ancho_esq_d ancho)
  )

  (initget "Arriba Abajo")
  (setq lado (getkword "\nLos lotes estan hacia ARRIBA o ABAJO de la banqueta? [Arriba/Abajo]: "))
  (setq lado (if (= lado "Arriba") 1.0 -1.0))

  (initget 6)
  (setq prof (getdist "\nProfundidad de penetracion en el lote (m) <2.0, maximo 2.0>: "))
  (if (or (not prof) (> prof 2.0)) (setq prof 2.0))

  ;; --- Bucle: puedes marcar varios registros sin repetir datos ---
  (setvar "OSMODE" old_osmode)
  (while (setq p_reg (getpoint "\n\n[CLICK] Punto del REGISTRO sobre la banqueta [Enter para salir]: "))

    (setvar "OSMODE" 0)

    ;; ---- Lado IZQUIERDO ----
    (setq acumulado 0.0)
    (setq i 1)
    (repeat n_izq
      (setq ancho_i (cond ((= i 1) ancho_esq_i) (T ancho)))
      (setq acumulado (+ acumulado (/ ancho_i 2.0)))
      (setq p_frente (list (- (car p_reg) acumulado) (cadr p_reg) 0.0))
      (setq acumulado (+ acumulado (/ ancho_i 2.0)))

      (setq p_punta (list (car p_frente) (+ (cadr p_frente) (* lado prof)) 0.0))
      (setq ang (angle p_reg p_punta))

      (command "._line" "_non" p_reg "_non" p_punta "")
      (command "._insert" blkname "_non" p_punta "1" "1" (angtos ang 0 4))

      (setq i (1+ i))
    )

    ;; ---- Lado DERECHO ----
    (setq acumulado 0.0)
    (setq i 1)
    (repeat n_der
      (setq ancho_i (cond ((= i 1) ancho_esq_d) (T ancho)))
      (setq acumulado (+ acumulado (/ ancho_i 2.0)))
      (setq p_frente (list (+ (car p_reg) acumulado) (cadr p_reg) 0.0))
      (setq acumulado (+ acumulado (/ ancho_i 2.0)))

      (setq p_punta (list (car p_frente) (+ (cadr p_frente) (* lado prof)) 0.0))
      (setq ang (angle p_reg p_punta))

      (command "._line" "_non" p_reg "_non" p_punta "")
      (command "._insert" blkname "_non" p_punta "1" "1" (angtos ang 0 4))

      (setq i (1+ i))
    )

    (princ (strcat "\n>>> Registro trazado: " (itoa n_izq) " lotes izq, " (itoa n_der) " lotes der."))
    (setvar "OSMODE" old_osmode)
  )

  (setvar "OSMODE" old_osmode)
  (princ "\nComando ACOM_REGISTRO finalizado.")
  (princ)
)

(princ "\nComando ACOM_REGISTRO cargado. Escribe ACOM_REGISTRO para iniciar.")
(princ)
