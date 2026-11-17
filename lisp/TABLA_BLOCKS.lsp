(defun C:TABLA_BLOCKS (/ CM RYS ZBIOR TEMP ENT NAME CAPA ITEM LISTA PT TABLA ROW)
  (setq CM (getvar "cmdecho"))
  (setvar "cmdecho" 0)
  (setq LISTA NIL)
  (prompt "\nSelecciona los bloques para generar la tabla de metrados...")
  (if (setq ZBIOR (ssget '((0 . "INSERT"))))
    (progn
      ;; 1. PROCESAR CONTEO POR CAPA
      (repeat (setq TEMP (sslength ZBIOR))
        (setq TEMP (1- TEMP))
        (setq ENT (ssname ZBIOR TEMP))
        (setq NAME (cdr (assoc 2 (entget ENT))))
        (setq CAPA (cdr (assoc 8 (entget ENT))))
        (setq ITEM (assoc CAPA LISTA))
        (if (not ITEM)
          (setq LISTA (cons (list CAPA 1 NAME) LISTA))
          (setq LISTA (subst (list CAPA (1+ (cadr ITEM)) (caddr ITEM)) ITEM LISTA))
        )
      )
      ;; 2. PEDIR PUNTO DE INSERCIÓN PARA LA TABLA
      (setq PT (getpoint "\nSelecciona el punto donde quieres colocar la tabla: "))
      
      (if (and PT LISTA)
        (progn
          ;; Crear el objeto Tabla
          (setq TABLA (vla-addtable 
                        (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object))) 
                        (vlax-3d-point PT) 
                        (+ (length LISTA) 2) ; Filas (encabezado + datos)
                        3                   ; Columnas (Imagen, Capa, Cantidad)
                        10.0                ; Altura de fila inicial
                        60.0                ; Ancho de columna inicial
                      ))
          (vla-put-regeneratetablesuppressed TABLA :vlax-true)
          ;; Configurar encabezados
          (vla-settext TABLA 0 0 "TABLA DE BLOQUES Y METRADOS POR CAPA")
          (vla-settext TABLA 1 0 "VISTA")
          (vla-settext TABLA 1 1 "CAPA")
          (vla-settext TABLA 1 2 "CANTIDAD")
          ;; 3. LLENAR LA TABLA CON DATOS
          (setq ROW 2)
          (foreach X LISTA
            (vla-settext TABLA ROW 1 (car X))        ; Capa
            (vla-settext TABLA ROW 2 (itoa (cadr X))) ; Cantidad
            
            ;; Insertar la imagen de un bloque representativo de esa capa
            (vla-setblocktablerecordid TABLA ROW 0 (vla-get-objectid (vla-item (vla-get-blocks (vla-get-activedocument (vlax-get-acad-object))) (caddr X))) :vlax-true)
            (vla-setcellalignment TABLA ROW 0 acMiddleCenter)
            
            (setq ROW (1+ ROW))
          )
          (vla-put-regeneratetablesuppressed TABLA :vlax-false)
          (princ "\nTabla generada exitosamente.")
        )
      )
    )
    (prompt "\nNo se seleccionaron bloques.")
  )
  (setvar "cmdecho" CM)
  (vl-load-com) ; Cargar funciones visual lisp
  (princ)
)
(prompt "\nComando cargado. Escribe TABLA_BLOCKS para generar la tabla en el dibujo.")
(princ)