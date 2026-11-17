;;; =========================================================================
;;;  RENOMBRA_NOMENCLATURA.LSP
;;;  Busca textos (TEXT/MTEXT) que coincidan con un patron (ej: H04*)
;;;  y los renombra secuencialmente con un nuevo prefijo + numero correlativo
;;;  Funciona de forma similar al comando TCOUNT de Express Tools
;;;
;;;  Comandos disponibles:
;;;     BUSCAPAT   -> Solo busca y resalta los textos que cumplen el patron
;;;     RENSEQ     -> Busca, filtra y renombra secuencialmente
;;; =========================================================================

;; ---------------------------------------------------------------------
;; Funcion auxiliar: obtiene el string real de un TEXT o MTEXT
;; (en MTEXT limpia los codigos de formato basicos para comparar el patron)
;; ---------------------------------------------------------------------
(defun LimpiaMTexto (str / out i c)
  (setq out "" i 1)
  (while (<= i (strlen str))
    (setq c (substr str i 1))
    (cond
      ;; Omite codigos de formato tipo \P \A1; \fArial|...; etc. (simplificado)
      ((and (= c "\\") (< i (strlen str)))
       (setq i (1+ i)))
      (t (setq out (strcat out c)))
    )
    (setq i (1+ i))
  )
  out
)

(defun ObtenTextoEntidad (ent / edata tipo str)
  (setq edata (entget ent))
  (setq tipo (cdr (assoc 0 edata)))
  (cond
    ((= tipo "TEXT") (cdr (assoc 1 edata)))
    ((= tipo "MTEXT") (LimpiaMTexto (cdr (assoc 1 edata))))
    (t nil)
  )
)

;; ---------------------------------------------------------------------
;; Funcion auxiliar: asigna un nuevo string a un TEXT o MTEXT
;; ---------------------------------------------------------------------
(defun AsignaTextoEntidad (ent nuevo / edata tipo)
  (setq edata (entget ent))
  (setq tipo (cdr (assoc 0 edata)))
  (cond
    ((= tipo "TEXT")
     (entmod (subst (cons 1 nuevo) (assoc 1 edata) edata))
    )
    ((= tipo "MTEXT")
     (entmod (subst (cons 1 nuevo) (assoc 1 edata) edata))
    )
  )
  (entupd ent)
)

;; ---------------------------------------------------------------------
;; Funcion auxiliar: construye la lista de entidades TEXT/MTEXT
;; cuyo contenido coincide con el patron (wildcard tipo *H04* , H04* , etc)
;; ---------------------------------------------------------------------
(defun FiltraPorPatron (patron / ss n ent txt lista i)
  (setq lista '())
  (setq ss (ssget "X" (list (cons 0 "TEXT,MTEXT"))))
  (if ss
    (progn
      (setq n (sslength ss) i 0)
      (while (< i n)
        (setq ent (ssname ss i))
        (setq txt (ObtenTextoEntidad ent))
        (if (and txt (wcmatch (strcase txt) (strcase patron)))
          (setq lista (cons ent lista))
        )
        (setq i (1+ i))
      )
    )
  )
  (reverse lista)
)

;; ---------------------------------------------------------------------
;; Funcion auxiliar: ordena la lista de entidades de arriba-abajo,
;; izquierda-derecha (orden de lectura de un plano)
;; ---------------------------------------------------------------------
(defun PtoEntidad (ent / edata)
  (setq edata (entget ent))
  (cdr (assoc 10 edata))
)

(defun OrdenaLista (lista / arr n i j tmp)
  ;; ordenamiento simple por burbuja: primero Y descendente, luego X ascendente
  (setq arr lista)
  (setq n (length arr))
  (setq arr (vl-sort arr
              (function
                (lambda (a b / pa pb)
                  (setq pa (PtoEntidad a) pb (PtoEntidad b))
                  (cond
                    ((> (- (cadr pa) (cadr pb)) 0.5) T)   ;; a mas arriba que b
                    ((> (- (cadr pb) (cadr pa)) 0.5) nil) ;; b mas arriba que a
                    (t (< (car pa) (car pb)))             ;; misma fila -> izq a der
                  )
                )
              )
            )
  )
  arr
)

;; ---------------------------------------------------------------------
;; Resalta temporalmente una entidad (efecto visual simple con grip color)
;; ---------------------------------------------------------------------
(defun ResaltaEntidad (ent)
  (redraw ent 3)
)

;; =========================================================================
;;  COMANDO: BUSCAPAT
;;  Solo busca y resalta, muestra el listado en pantalla (no modifica nada)
;; =========================================================================
(defun c:BUSCAPAT ( / patron lista n i ent txt)
  (setvar "CMDECHO" 0)
  (setq patron (getstring T "\nPatron a buscar (use * como comodin, ej: H04*): "))
  (if (= patron "") (setq patron "*"))
  (setq lista (FiltraPorPatron patron))
  (setq n (length lista))
  (if (= n 0)
    (princ (strcat "\nNo se encontraron textos que coincidan con: " patron))
    (progn
      (princ (strcat "\nSe encontraron " (itoa n) " texto(s) con el patron '" patron "':\n"))
      (setq i 0)
      (foreach ent lista
        (setq txt (ObtenTextoEntidad ent))
        (princ (strcat "  " (itoa (1+ i)) ". " txt "\n"))
        (ResaltaEntidad ent)
        (setq i (1+ i))
      )
    )
  )
  (princ)
)

;; =========================================================================
;;  COMANDO: RENSEQ
;;  Busca por patron, pide nuevo prefijo + numero inicial, y renombra
;;  en orden automatico o manual (clic por clic, como TCOUNT)
;; =========================================================================
(defun c:RENSEQ ( / patron lista n prefijo numIni modo orden ent txt nuevo
                    contador i esc pickado idx encontrado nombreViejo log)

  (setvar "CMDECHO" 0)
  (princ "\n=== RENOMBRAR NOMENCLATURAS EN SECUENCIA ===")

  ;; 1. Patron de busqueda
  (setq patron (getstring T "\nPatron de busqueda (use * como comodin, ej: H04*): "))
  (if (= patron "") (setq patron "*"))

  (setq lista (FiltraPorPatron patron))
  (setq n (length lista))

  (if (= n 0)
    (progn
      (princ (strcat "\nNo se encontraron textos que coincidan con: " patron))
      (princ)
    )
    (progn
      (princ (strcat "\nSe encontraron " (itoa n) " texto(s) que coinciden con '" patron "'."))
      (foreach ent lista (ResaltaEntidad ent))

      ;; 2. Nuevo prefijo
      (setq prefijo (getstring T "\nNuevo prefijo (ej: H4R13T6-D): "))
      (if (= prefijo "") (setq prefijo "TXT-"))

      ;; 3. Numero inicial
      (setq numIni (getint "\nNumero inicial (ej: 1): "))
      (if (not numIni) (setq numIni 1))

      ;; 4. Modo de orden
      (initget "Automatico Manual")
      (setq modo (getkword "\nOrden de renombrado [Automatico/Manual] <Automatico>: "))
      (if (not modo) (setq modo "Automatico"))

      (setq log '())

      (cond
        ;; ---------------- MODO AUTOMATICO ----------------
        ((= modo "Automatico")
         (setq orden (OrdenaLista lista))
         (setq contador numIni)
         (foreach ent orden
           (setq nombreViejo (ObtenTextoEntidad ent))
           (setq nuevo (strcat prefijo (itoa contador)))
           (AsignaTextoEntidad ent nuevo)
           (setq log (cons (list nombreViejo nuevo) log))
           (setq contador (1+ contador))
         )
        )

        ;; ---------------- MODO MANUAL (tipo TCOUNT) ----------------
        ((= modo "Manual")
         (princ "\nHaga clic en cada texto EN EL ORDEN deseado. ENTER o ESC para terminar.")
         (setq contador numIni)
         (setq esc nil)
         (while (not esc)
           (setq pickado (entsel (strcat "\nSeleccione texto #" (itoa contador) " (ENTER para terminar): ")))
           (if (not pickado)
             (setq esc T)
             (progn
               (setq ent (car pickado))
               (setq idx (vl-position ent lista))
               (if (not idx)
                 (princ "\n  >> Ese texto no pertenece a los resultados del patron. Intente de nuevo.")
                 (progn
                   (setq nombreViejo (ObtenTextoEntidad ent))
                   (setq nuevo (strcat prefijo (itoa contador)))
                   (AsignaTextoEntidad ent nuevo)
                   (setq log (cons (list nombreViejo nuevo) log))
                   (princ (strcat "\n  " nombreViejo "  ->  " nuevo))
                   (setq contador (1+ contador))
                 )
               )
             )
           )
         )
        )
      )

      ;; 5. Resumen final
      (setq log (reverse log))
      (princ "\n\n=== RESUMEN DE CAMBIOS ===")
      (foreach par log
        (princ (strcat "\n  " (car par) "   ->   " (cadr par)))
      )
      (princ (strcat "\n\nTotal renombrados: " (itoa (length log))))
      (princ)
    )
  )
  (princ)
)

(princ "\n== RENOMBRA_NOMENCLATURA.LSP cargado ==")
(princ "\nComandos disponibles:  BUSCAPAT  (solo buscar/resaltar)   RENSEQ  (buscar y renombrar)")
(princ)
