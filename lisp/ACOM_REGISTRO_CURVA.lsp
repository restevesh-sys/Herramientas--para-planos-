;;; ============================================================
;;; ACOM_REGISTRO_CURVA.LSP
;;; Version mejorada: funciona con banquetas RECTAS o CURVAS.
;;;
;;; Mejora clave respecto a la version anterior:
;;;   En vez de calcular angulos fijos (cartesianos), este script
;;;   "camina" sobre la polilinea/linea de la banqueta usando
;;;   funciones parametricas (vlax-curve-*). Esto permite:
;;;     - Ubicar el frente de cada lote a la distancia real de
;;;       arco (no en linea recta), aunque la banqueta tenga
;;;       tramos curvos (como una vialidad con retorno).
;;;     - Calcular la TANGENTE real de la banqueta en ese punto
;;;       exacto, y sacar la perpendicular correcta -> la flecha
;;;       siempre sale perpendicular a la banqueta, sea recta o
;;;       curva, sin cambiar nada en el codigo.
;;;
;;; Se mantiene:
;;;   - Punta de flecha = BLOQUE (PUNTA_ACOM), no interfiere en
;;;     metrados de area/longitud.
;;;   - Profundidad de penetracion en el lote con tope de 2 m.
;;;   - Registro seleccionado por el usuario, N lotes a cada lado.
;;;
;;; USO:
;;;   Comando: ACOM_REGISTRO_CURVA
;;;   1) Selecciona la banqueta (LINE, LWPOLYLINE, ARC o SPLINE)
;;;   2) Da los datos generales (lotes izq/der, ancho, esquinas,
;;;      lado de los lotes, profundidad)
;;;   3) Haz click cerca de la banqueta en cada punto de registro
;;;      (se ajusta automaticamente al punto mas cercano SOBRE
;;;      la banqueta)
;;;   4) Se repite mientras quieras seguir marcando registros
;;; ============================================================

(defun ACOM_CREA_BLOQUE ( / pt1 pt2 pt3)
  (if (not (tblsearch "BLOCK" "PUNTA_ACOM"))
    (progn
      (setq pt1 (list 0.0 0.0 0.0)
            pt2 (list -0.35 0.12 0.0)
            pt3 (list -0.35 -0.12 0.0))
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

;; Devuelve el punto sobre la curva a una distancia de arco dada,
;; acotando a los limites de la curva (no se sale del objeto).
(defun ACOM_PUNTO_A_DIST (curva d_objetivo / d_max)
  (setq d_max (vlax-curve-getDistAtParam curva (vlax-curve-getEndParam curva)))
  (cond
    ((< d_objetivo 0.0) (vlax-curve-getStartPoint curva))
    ((> d_objetivo d_max) (vlax-curve-getEndPoint curva))
    (T (vlax-curve-getPointAtDist curva d_objetivo))
  )
)

;; Devuelve el angulo tangente de la curva en un punto dado
(defun ACOM_ANGULO_TANGENTE (curva punto / param deriv)
  (setq param (vlax-curve-getParamAtPoint curva punto))
  (setq deriv (vlax-curve-getFirstDeriv curva param))
  (atan (cadr deriv) (car deriv))
)

;; Traza UNA acometida: linea desde el registro hasta el frente del
;; lote (siguiendo la curva), luego el tramo perpendicular de
;; profundidad "prof", y ahi inserta el bloque de punta.
(defun ACOM_TRAZA_UNA (curva p_reg d_reg d_lote lado prof blkname / p_frente ang_tan ang_perp p_punta)
  (setq p_frente (ACOM_PUNTO_A_DIST curva (+ d_reg d_lote)))
  (setq ang_tan (ACOM_ANGULO_TANGENTE curva p_frente))
  (setq ang_perp (+ ang_tan (* lado (/ pi 2))))
  (setq p_punta (polar p_frente ang_perp prof))

  (command "._line" "_non" p_reg "_non" p_frente "")
  (if (> prof 0.0001)
    (command "._line" "_non" p_frente "_non" p_punta "")
  )
  (command "._insert" blkname "_non" p_punta "1" "1" (angtos ang_perp 0 4))
)

(defun c:ACOM_REGISTRO_CURVA ( / ent curva n_izq n_der ancho ancho_esq_i ancho_esq_d
                                 lado prof old_osmode p_click p_reg d_reg
                                 acumulado ancho_i i blkname)

  (setq old_osmode (getvar "OSMODE"))
  (ACOM_LAYER)
  (ACOM_CREA_BLOQUE)
  (setq blkname "PUNTA_ACOM")

  ;; --- Seleccion de la banqueta (una sola vez) ---
  (setvar "OSMODE" old_osmode)
  (setq ent (car (entsel "\nSelecciona la banqueta (linea, polilinea o curva): ")))
  (if (not ent)
    (progn (princ "\nNo se selecciono ningun objeto. Comando cancelado.") (exit))
  )
  (setq curva (vlax-ename->vla-object ent))

  ;; --- Datos generales ---
  (initget 7)
  (setq n_izq (getint "\nCantidad de lotes a alimentar hacia ATRAS (izquierda/inicio de curva): "))
  (initget 7)
  (setq n_der (getint "\nCantidad de lotes a alimentar hacia ADELANTE (derecha/fin de curva): "))
  (initget 7)
  (setq ancho (getdist "\nAncho de lote estandar (m), ej. 8.5: "))

  (initget "Si No")
  (setq ancho_esq_i (getkword "\nEl primer lote (extremo inicial) es de esquina? [Si/No] <No>: "))
  (if (= ancho_esq_i "Si")
    (progn (initget 7) (setq ancho_esq_i (getdist "\nAncho del lote de esquina inicial (m), ej. 10: ")))
    (setq ancho_esq_i ancho)
  )

  (initget "Si No")
  (setq ancho_esq_d (getkword "\nEl ultimo lote (extremo final) es de esquina? [Si/No] <No>: "))
  (if (= ancho_esq_d "Si")
    (progn (initget 7) (setq ancho_esq_d (getdist "\nAncho del lote de esquina final (m), ej. 10: ")))
    (setq ancho_esq_d ancho)
  )

  (initget "Izquierda Derecha")
  (setq lado (getkword "\nLos lotes quedan a mano IZQUIERDA o DERECHA del sentido de la banqueta? [Izquierda/Derecha]: "))
  (setq lado (if (= lado "Izquierda") 1.0 -1.0))

  (initget 6)
  (setq prof (getdist "\nProfundidad de penetracion en el lote (m) <2.0, maximo 2.0>: "))
  (if (or (not prof) (> prof 2.0)) (setq prof 2.0))

  ;; --- Bucle de registros ---
  (setvar "OSMODE" 0)
  (while (setq p_click (getpoint "\n\n[CLICK] cerca de la banqueta = punto de REGISTRO [Enter para salir]: "))

    (setq p_reg (vlax-curve-getClosestPointTo curva p_click))
    (setq d_reg (vlax-curve-getDistAtPoint curva p_reg))

    ;; Lado hacia atras (izquierda / hacia el inicio de la curva)
    (setq acumulado 0.0)
    (setq i 1)
    (repeat n_izq
      (setq ancho_i (cond ((= i 1) ancho_esq_i) (T ancho)))
      (setq acumulado (+ acumulado (/ ancho_i 2.0)))
      (ACOM_TRAZA_UNA curva p_reg d_reg (- acumulado) lado prof blkname)
      (setq acumulado (+ acumulado (/ ancho_i 2.0)))
      (setq i (1+ i))
    )

    ;; Lado hacia adelante (derecha / hacia el fin de la curva)
    (setq acumulado 0.0)
    (setq i 1)
    (repeat n_der
      (setq ancho_i (cond ((= i 1) ancho_esq_d) (T ancho)))
      (setq acumulado (+ acumulado (/ ancho_i 2.0)))
      (ACOM_TRAZA_UNA curva p_reg d_reg acumulado lado prof blkname)
      (setq acumulado (+ acumulado (/ ancho_i 2.0)))
      (setq i (1+ i))
    )

    (princ (strcat "\n>>> Registro trazado: " (itoa n_izq) " lotes atras, " (itoa n_der) " lotes adelante."))
  )

  (setvar "OSMODE" old_osmode)
  (princ "\nComando ACOM_REGISTRO_CURVA finalizado.")
  (princ)
)

(princ "\nComando ACOM_REGISTRO_CURVA cargado. Escribe ACOM_REGISTRO_CURVA para iniciar.")
(princ)
