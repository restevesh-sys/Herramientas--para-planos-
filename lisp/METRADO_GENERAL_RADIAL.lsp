(defun c:METRADO_GENERAL_RADIAL (/ sset i ent txt_str lista_datos total_rad total_mts p_tabla obj_tabla fila rad_val mts_val)
>>   (vl-load-com)
>>   (setvar "CMDECHO" 0)
>>
>>   ;; 1. Selección de todos los textos de resumen (Filtro por contenido)
>>   (princ "\nSeleccione todos los textos 'RESUMEN: X RADIALES | TOTAL: Y.YY m': ")
>>   (setq sset (ssget '((0 . "TEXT") (1 . "*RADIALES*TOTAL*"))))   
>>
>>   (if (not sset)
>>     (progn (princ "\nError: No se seleccionaron textos con el formato de resumen.") (exit))        
>>   )
>>
>>   (setq total_rad 0 total_mts 0.0 lista_datos nil)
>>
>>   ;; 2. Procesamiento con extracción de números robusta
>>   (setq i 0)
>>   (repeat (sslength sset)      
>>     (setq ent (ssname sset i)) 
>>     (setq txt_str (strcase (cdr (assoc 1 (entget ent))))) ; Convertimos a mayúsculas para comparar 
>>
>>     ;; Extraer el valor de RADIALES (busca el número antes de la palabra RADIALES)
>>     (setq rad_val (atof (limpiar_num (substr txt_str 1 (vl-string-search "RADIALES" txt_str)))))   
>>
>>     ;; Extraer el valor de METROS (busca el número después de TOTAL: y antes de la M)
>>     (setq pos_total (vl-string-search "TOTAL" txt_str))
>>     (setq pos_m (vl-string-search " M" txt_str))
>>     (setq mts_val (atof (limpiar_num (substr txt_str (+ pos_total 6) (- pos_m pos_total 5)))))     
>>
>>     (setq total_rad (+ total_rad rad_val))
>>     (setq total_mts (+ total_mts mts_val))
>>
>>     ;; Guardamos para la tabla (Radiales y Metros)
>>     (setq lista_datos (cons (list rad_val mts_val) lista_datos)) 
>>     (setq i (1+ i))
>>   )
>>
>>   ;; 3. Creación de la Tabla de Metrados Final
>>   (setq p_tabla (getpoint "\nClic para ubicar la TABLA DE GRAN TOTAL: "))
>>
>>   (setq obj_tabla (vla-addtable

>>                     (vla-get-modelspace (vla-get-activedocument (vlax-get-acad-object)))
>>                     (vlax-3d-point p_tabla)
>>                     (+ (sslength sset) 3) ; Filas
>>                     3 ; Columnas
>>                     1.2 ; Altura fila
>>                     10.0 ; Ancho columna
>>                   ))
>>
>>   ;; Configuración de la Tabla 
>>   (vla-settext obj_tabla 0 0 "GRAN TOTAL DE INFRAESTRUCTURA RADIAL")
>>   (vla-settext obj_tabla 1 0 "ITEM")
>>   (vla-settext obj_tabla 1 1 "CANT. RADIALES")
>>   (vla-settext obj_tabla 1 2 "LONGITUD TOTAL (m)")
>>
>>   (setq fila 2)
>>   (setq i 0)
>>   (foreach item (reverse lista_datos)
>>   )
>>
>>   ;; Fila de Sumatoria Final
>>   (vla-settext obj_tabla fila 0 "SUMA TOTAL")
>>   (vla-settext obj_tabla fila 1 (rtos total_rad 2 0))
>>   (vla-settext obj_tabla fila 2 (strcat (rtos total_mts 2 2) " m"))
>>   (vla-setrowheight obj_tabla fila 1.8)
>>
>>   (princ (strcat "\nCompletado: " (itoa (sslength sset)) " resúmenes sumados."))
>>   (princ)
>> )
>>
>> ;; Función para limpiar basura de los textos y dejar solo números/puntos
>> (defun limpiar_num (str / res c)
>>   (setq res "")
>>   (setq i 1)
>>   (repeat (strlen str)
>>     (setq c (substr str i 1))
>>     (if (or (and (>= c "0") (<= c "9")) (= c "."))
>>       (setq res (strcat res c))
>>     )
>>     (setq i (1+ i))
>>   )
>>   res
>> )







