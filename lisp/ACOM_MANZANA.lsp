;;; ============================================================
;;; ACOM_MANZANA.LSP
;;; Automatiza, para una MANZANA COMPLETA (rectangular):
;;;   1) Selecciona los lotes de la manzana.
;;;   2) Calcula el rectangulo perimetral (bounding box).
;;;   3) Traza el DUCTO desfasado 30 cm hacia afuera del perimetro.
;;;   4) Inserta un REGISTRO en cada una de las 4 esquinas
;;;      (siempre, sin importar la longitud de los lados).
;;;   5) Inserta REGISTROS intermedios en cada lado segun su
;;;      longitud, con separacion objetivo de 45-50 m:
;;;         n_tramos = round(L / 50)
;;;         n_intermedios = max(0, n_tramos - 1)
;;;      Ejemplos: 20m->0 intermedios | 100m->1 | 150m->2 | 200m->3
;;;
;;; Capas creadas:
;;;   DUCTO      -> rectangulo de ducteria (para metrado de metros)
;;;   REGISTROS  -> bloques de registro (esquina e intermedios)
;;;
;;; NOTA: asume manzana rectangular alineada a los ejes X/Y.
;;; Si la manzana esta girada o es irregular, este metodo de
;;; bounding box no aplica tal cual (avisar para ajustar).
;;; ============================================================

(defun ACOM_LAYER_SET (nombre color / )
  (if (not (tblsearch "LAYER" nombre))
    (command "._-layer" "m" nombre "c" color "" "")
  )
  (setvar "CLAYER" nombre)
)

(defun ACOM_CREA_BLOQUE_REG ( / )
  ;; Bloque simple de registro: circulo + cruz, base en el centro (0,0)
  (if (not (tblsearch "BLOCK" "REG_MZA"))
    (progn
      (entmake (list (cons 0 "CIRCLE") (cons 8 "0") (cons 10 (list 0.0 0.0 0.0)) (cons 40 0.20)))
      (entmake (list (cons 0 "LINE") (cons 8 "0") (cons 10 (list -0.25 0.0 0.0)) (cons 11 (list 0.25 0.0 0.0))))
      (entmake (list (cons 0 "LINE") (cons 8 "0") (cons 10 (list 0.0 -0.25 0.0)) (cons 11 (list 0.0 0.25 0.0))))
      (command "._-block" "REG_MZA" "0,0,0" (entlast) (entlast) (entlast) "")
      ;; nota: -block con multiples entidades requiere seleccionarlas todas;
      ;; se resuelve mejor abajo agrupando con ssadd antes del comando.
    )
  )
)

;; Version robusta de creacion de bloque (selecciona las 3 entidades recien creadas)
(defun ACOM_CREA_BLOQUE_REG2 ( / e1 e2 e3 ss)
  (if (not (tblsearch "BLOCK" "REG_MZA"))
    (progn
      (entmake (list (cons 0 "CIRCLE") (cons 8 "0") (cons 10 (list 0.0 0.0 0.0)) (cons 40 0.20)))
      (setq e1 (entlast))
      (entmake (list (cons 0 "LINE") (cons 8 "0") (cons 10 (list -0.25 0.0 0.0)) (cons 11 (list 0.25 0.0 0.0))))
      (setq e2 (entlast))
      (entmake (list (cons 0 "LINE") (cons 8 "0") (cons 10 (list 0.0 -0.25 0.0)) (cons 11 (list 0.0 0.25 0.0))))
      (setq e3 (entlast))
      (setq ss (ssadd))
      (ssadd e1 ss) (ssadd e2 ss) (ssadd e3 ss)
      (command "._-block" "REG_MZA" "0,0,0" ss "")
    )
  )
)

;; Calcula cuantos INTERMEDIOS van en un lado de longitud L
(defun ACOM_N_INTERMEDIOS (L / n_tramos)
  (setq n_tramos (atoi (rtos (/ L 50.0) 2 0))) ; redondeo simple
  ;; correccion de redondeo (rtos con 0 decimales redondea correctamente)
  (max 0 (1- n_tramos))
)

(defun c:ACOM_MANZANA ( / ss n i ent obj minx miny maxx maxy off
                          p1 p2 p3 p4 op1 op2 op3 op4
                          lados k pA pB Lado n_int j t_frac pInt ang
                          total_ducto total_reg vx vy)

  (ACOM_CREA_BLOQUE_REG2)

  ;; --- 1) Seleccion de lotes ---
  (setq ss (ssget '((0 . "LWPOLYLINE,POLYLINE"))))
  (if (not ss)
    (progn (princ "\nNo se selecciono ningun lote. Comando cancelado.") (exit))
  )

  ;; --- 2) Bounding box de todos los lotes seleccionados ---
  (setq minx nil miny nil maxx nil maxy nil)
  (setq n (sslength ss))
  (setq i 0)
  (repeat n
    (setq ent (ssname ss i))
    (setq obj (vlax-ename->vla-object ent))
    (vla-getboundingbox obj 'pmin 'pmax)
    (setq vx (vlax-safearray->list pmin))
    (setq vy (vlax-safearray->list pmax))
    (if (not minx)
      (progn
        (setq minx (car vx) miny (cadr vx))
        (setq maxx (car vy) maxy (cadr vy))
      )
      (progn
        (if (< (car vx) minx) (setq minx (car vx)))
        (if (< (cadr vx) miny) (setq miny (cadr vx)))
        (if (> (car vy) maxx) (setq maxx (car vy)))
        (if (> (cadr vy) maxy) (setq maxy (cadr vy)))
      )
    )
    (setq i (1+ i))
  )

  ;; --- 3) Rectangulo desfasado 30 cm hacia afuera ---
  (initget 6)
  (setq off (getdist "\nDesfase del ducto hacia afuera (m) <0.30>: "))
  (if (not off) (setq off 0.30))

  (setq p1 (list minx miny 0.0)   ; esquina inferior izquierda
        p2 (list maxx miny 0.0)   ; esquina inferior derecha
        p3 (list maxx maxy 0.0)   ; esquina superior derecha
        p4 (list minx maxy 0.0))  ; esquina superior izquierda

  (setq op1 (list (- minx off) (- miny off) 0.0)
        op2 (list (+ maxx off) (- miny off) 0.0)
        op3 (list (+ maxx off) (+ maxy off) 0.0)
        op4 (list (- minx off) (+ maxy off) 0.0))

  ;; --- Dibujo del ducto en capa DUCTO ---
  (ACOM_LAYER_SET "DUCTO" 5)
  (command "._pline" "_non" op1 "_non" op2 "_non" op3 "_non" op4 "c")
  (setq total_ducto (+ (* 2 (distance op1 op2)) (* 2 (distance op2 op3))))

  ;; --- 4) y 5) Registros: 4 esquinas + intermedios por lado ---
  (ACOM_LAYER_SET "REGISTROS" 2)
  (setq total_reg 0)

  ;; Esquinas (escala 1.5 para distinguir)
  (foreach pc (list op1 op2 op3 op4)
    (command "._insert" "REG_MZA" "_non" pc "1.5" "1.5" "0")
    (setq total_reg (1+ total_reg))
  )

  ;; Lados: (desde -> hasta) en orden op1-op2, op2-op3, op3-op4, op4-op1
  (setq lados (list (list op1 op2) (list op2 op3) (list op3 op4) (list op4 op1)))
  (foreach ladoActual lados
    (setq pA (car ladoActual))
    (setq pB (cadr ladoActual))
    (setq Lado (distance pA pB))
    (setq n_int (ACOM_N_INTERMEDIOS Lado))
    (setq ang (angle pA pB))
    (setq j 1)
    (repeat n_int
      (setq t_frac (/ (float j) (1+ n_int)))
      (setq pInt (polar pA ang (* t_frac Lado)))
      (command "._insert" "REG_MZA" "_non" pInt "1" "1" "0")
      (setq total_reg (1+ total_reg))
      (setq j (1+ j))
    )
  )

  ;; --- Resumen ---
  (princ (strcat "\n\n=== RESUMEN MANZANA ==="))
  (princ (strcat "\nPerimetro de ducto trazado: " (rtos total_ducto 2 2) " m"))
  (princ (strcat "\nRegistros insertados: " (itoa total_reg) " (4 de esquina + intermedios)"))
  (princ "\nComando ACOM_MANZANA finalizado.")
  (princ)
)

(princ "\nComando ACOM_MANZANA cargado. Escribe ACOM_MANZANA para iniciar.")
(princ)
