;Moreno Zárate Víctor Gibrán
(defparameter *tablero* (loop for i from 1 to 24 collect NIL))
(defparameter *etapa* 1)

(defparameter *mano_fichas_IA* 9)
(defparameter *mano_fichas_humano* 9)
(defparameter *cementerio_fichas_IA* 0)
(defparameter *cementerio_fichas_humano* 0)

(defparameter *estado* (list *tablero* *etapa* *mano_fichas_IA* *mano_fichas_humano* *cementerio_fichas_IA* *cementerio_fichas_humano*))

(defparameter *símbolo_IA* 'A)
(defparameter *símbolo_humano* 'R)

(defparameter *profundidad-máxima* 5) 
; Mapeo Casilla -> (Adjacencias)
(defparameter *adjacencias* (make-hash-table))
(setf (gethash 1 *adjacencias*) '(2 10))
(setf (gethash 2 *adjacencias*) '(1 3 5 ))
(setf (gethash 3 *adjacencias*) '(2 15))
(setf (gethash 4 *adjacencias*) '(5 11))
(setf (gethash 5 *adjacencias*) '(4 6 2 8))
(setf (gethash 6 *adjacencias*) '(5 14))
(setf (gethash 7 *adjacencias*) '(12 8))
(setf (gethash 8 *adjacencias*) '(5 7 9))
(setf (gethash 9 *adjacencias*) '(8 13))
(setf (gethash 10 *adjacencias*) '(1 22 11))
(setf (gethash 11 *adjacencias*) '(4 19 10 12))
(setf (gethash 12 *adjacencias*) '(7 16 11))
(setf (gethash 13 *adjacencias*) '(9 18 14))
(setf (gethash 14 *adjacencias*) '(6 21 13 15))
(setf (gethash 15 *adjacencias*) '(3 24 14))
(setf (gethash 16 *adjacencias*) '(12 17))
(setf (gethash 17 *adjacencias*) '(16 18 20))
(setf (gethash 18 *adjacencias*) '(13 17))
(setf (gethash 19 *adjacencias*) '(11 20))
(setf (gethash 20 *adjacencias*) '(17 23 19 21))
(setf (gethash 21 *adjacencias*) '(14 20))
(setf (gethash 22 *adjacencias*) '(10 23))
(setf (gethash 23 *adjacencias*) '(20 22 24))
(setf (gethash 24 *adjacencias*) '(15 23))
; Mapeo número hilera->(casillas)
(defparameter *config_3_linea* (make-hash-table))
(setf (gethash 1 *config_3_linea*) '(1 2 3))
(setf (gethash 2 *config_3_linea*) '(4 5 6))
(setf (gethash 3 *config_3_linea*) '(7 8 9))
(setf (gethash 4 *config_3_linea*) '(10 11 12))
(setf (gethash 5 *config_3_linea*) '(13 14 15))
(setf (gethash 6 *config_3_linea*) '(16 17 18))
(setf (gethash 7 *config_3_linea*) '(19 20 21))
(setf (gethash 8 *config_3_linea*) '(22 23 24))

(setf (gethash 9 *config_3_linea*) '(1 10 22))
(setf (gethash 10 *config_3_linea*) '(4 11 19))
(setf (gethash 11 *config_3_linea*) '(7 12 16))
(setf (gethash 12 *config_3_linea*) '(2 5 8))
(setf (gethash 13 *config_3_linea*) '(17 20 23))
(setf (gethash 14 *config_3_linea*) '(9 13 18))
(setf (gethash 15 *config_3_linea*) '(6 14 21))
(setf (gethash 16 *config_3_linea*) '(3 15 24))
; Mapeo número casilla-> (hileras)
(defparameter *mapeo_casilla_hilera* (make-hash-table))
(setf (gethash 1 *mapeo_casilla_hilera*) '(1 9))
(setf (gethash 2 *mapeo_casilla_hilera*) '(1 12))
(setf (gethash 3 *mapeo_casilla_hilera*) '(1 16))
(setf (gethash 4 *mapeo_casilla_hilera*) '(2 10))
(setf (gethash 5 *mapeo_casilla_hilera*) '(2 12))
(setf (gethash 6 *mapeo_casilla_hilera*) '(2 15))
(setf (gethash 7 *mapeo_casilla_hilera*) '(3 11))
(setf (gethash 8 *mapeo_casilla_hilera*) '(3 12))
(setf (gethash 9 *mapeo_casilla_hilera*) '(3 14))
(setf (gethash 10 *mapeo_casilla_hilera*) '(4 9))
(setf (gethash 11 *mapeo_casilla_hilera*) '(4 10))
(setf (gethash 12 *mapeo_casilla_hilera*) '(4 11))
(setf (gethash 13 *mapeo_casilla_hilera*) '(5 14))
(setf (gethash 14 *mapeo_casilla_hilera*) '(5 15))
(setf (gethash 15 *mapeo_casilla_hilera*) '(5 16))
(setf (gethash 16 *mapeo_casilla_hilera*) '(6 11))
(setf (gethash 17 *mapeo_casilla_hilera*) '(6 13))
(setf (gethash 18 *mapeo_casilla_hilera*) '(6 14))
(setf (gethash 19 *mapeo_casilla_hilera*) '(7 10))
(setf (gethash 20 *mapeo_casilla_hilera*) '(7 13))
(setf (gethash 21 *mapeo_casilla_hilera*) '(7 15))
(setf (gethash 22 *mapeo_casilla_hilera*) '(8 9))
(setf (gethash 23 *mapeo_casilla_hilera*) '(8 13))
(setf (gethash 24 *mapeo_casilla_hilera*) '(8 16))

;Negamax parametrizado
(defun Negamax-alphabeta (estado profundidad max-prof α β fun_evaluacion fun_descendientes fun_fin_de_juego? fun_ordenamovimientos fun_aplicatirada)
    (when (or (= profundidad max-prof) (APPLY fun_fin_de_juego? (list estado)))
        (let (
                (símbolo (if (= (mod profundidad 2 ) 0) *símbolo_IA* *símbolo_humano*))
            )
            (return-from Negamax-alphabeta (list NIL (APPLY fun_evaluacion (list estado SÍMBOLO))))
        )        
    )
    (let* (
            (valor 0)
            (mejor-valor MOST-NEGATIVE-FIXNUM)
            (mejor-mov NIL)
            (símbolo (if (= (mod profundidad 2 ) 0) *símbolo_IA* *símbolo_humano*))
            (operadores (APPLY fun_descendientes (list símbolo estado )))
            (descendiente NIL)
        )
        (setq operadores (APPLY fun_ordenamovimientos (list operadores estado)))
        (dolist (operador operadores)
            (setq descendiente (APPLY fun_aplicatirada (list operador estado)))
            (setq valor (- (second (Negamax-alphabeta descendiente (+ profundidad 1) max-prof (- β) (- (MAX α mejor-valor ))  fun_evaluacion fun_descendientes fun_fin_de_juego? fun_ordenamovimientos fun_aplicatirada))) )
            (when (> valor mejor-valor)
                (setq mejor-valor valor mejor-mov operador)
                (when (>= mejor-valor β )
                    (return)
                )
            )
        )
        (list mejor-mov mejor-valor)
    )
)

(defun obtiene_casilla (índice tablero)
    (nth (- índice 1) tablero)
)

(defun obtiene_hilera (índice tablero)
    (mapcar #'(lambda (índice_casilla) (obtiene_casilla índice_casilla tablero)) 
        (gethash índice *config_3_linea*)
    )
)

(defun obtiene_adjacencias (índice tablero)
    (let (
            (hilera (gethash índice *adjacencias*))
        )
        (mapcar #'(lambda (índice) (obtiene_casilla índice tablero))  hilera)
    )
)

(defun obtiene_símbolo_enemigo (símbolo)
    (cond
        ((string= símbolo *símbolo_IA*) *símbolo_humano*)
        ((string= símbolo *símbolo_humano*) *símbolo_IA*)
    )
)

(defun actualiza_estadoglobal (estado)    
    (setq *estado* estado)
    (setq *tablero* (first estado))
    (setq *etapa* (second estado))
    (setq *mano_fichas_IA* (third estado))
    (setq *mano_fichas_humano* (fourth estado))
    (setq *cementerio_fichas_IA* (fifth estado))
    (setq *cementerio_fichas_humano* (sixth estado))
)

(defun obtiene_casilla_dibujo (índice tablero)
    (let ((símbolo (obtiene_casilla índice tablero)))
        (cond 
            ((null símbolo) "○")
            ((string= símbolo *símbolo_IA*) (format nil "~c[94m⬤~c[0m" #\ESC #\ESC))
            ((string= símbolo *símbolo_humano*) (format nil "~c[31m⬤~c[0m" #\ESC #\ESC))
        )
    )
)

(defun dibuja_tablero (estado)
    (let (
            (tablero (first estado))
            (etapa (second estado))
            (mano_fichas_IA (third estado))
            (mano_fichas_humano (fourth estado))
            (cementerio_fichas_IA (fifth estado))
            (cementerio_fichas_humano (sixth estado))
        )
        (format t "~%Notación -> Casilla disponible: ○ Casilla ocupada: ⬤~%")
        (format t "~%Etapa: ~a" etapa)
        (format t "~%Fichas ~c[31m⬤~c[0m  disponibles: ~a  Fichas ~c[94m⬤~c[0m  disponibles: ~a~%" #\ESC #\ESC mano_fichas_humano #\ESC #\ESC mano_fichas_IA)
        (format t "~%Estado tablero:     Numeración:~%")
        (format t " ~a——————~a——————~a   01———————02——————03~%"     (obtiene_casilla_dibujo 1 tablero)(obtiene_casilla_dibujo 2 tablero) (obtiene_casilla_dibujo 3 tablero))
        (format t " |  ~a — ~a — ~a  |    | 04  — 05 — 06  |~%"     (obtiene_casilla_dibujo 4 tablero)(obtiene_casilla_dibujo 5 tablero) (obtiene_casilla_dibujo 6 tablero))
        (format t " |  | ~a ~a ~a |  |    |  | 07 08 09 |  |~%"     (obtiene_casilla_dibujo 7 tablero)(obtiene_casilla_dibujo 8 tablero) (obtiene_casilla_dibujo 9 tablero))
        (format t " ~a  ~a ~a + ~a ~a  ~a   10 11 12 ＋ 13 14 15~%" (obtiene_casilla_dibujo 10 tablero)(obtiene_casilla_dibujo 11 tablero) (obtiene_casilla_dibujo 12 tablero) (obtiene_casilla_dibujo 13 tablero)(obtiene_casilla_dibujo 14 tablero) (obtiene_casilla_dibujo 15 tablero))
        (format t " |  | ~a ~a ~a |  |    |  | 16 17 18 |  |~%"     (obtiene_casilla_dibujo 16 tablero)(obtiene_casilla_dibujo 17 tablero) (obtiene_casilla_dibujo 18 tablero))
        (format t " |  ~a — ~a — ~a  |    | 19  — 20 — 21  |~%"     (obtiene_casilla_dibujo 19 tablero)(obtiene_casilla_dibujo 20 tablero) (obtiene_casilla_dibujo 21 tablero))
        (format t " ~a——————~a——————~a   22———————23———————24~%"    (obtiene_casilla_dibujo 22 tablero)(obtiene_casilla_dibujo 23 tablero) (obtiene_casilla_dibujo 24 tablero))
        (format t "~%Cementerio ~c[31m⬤~c[0m : ~a  Cementerio ~c[94m⬤~c[0m : ~a~%"  #\ESC #\ESC cementerio_fichas_humano #\ESC #\ESC cementerio_fichas_IA)
    )
)

; Encuentra todas las ocurrencias de needle en haystack
(defun get-positions (needle haystack)
  (let ((result nil))
    (dotimes (i (length haystack))
      (if (eq (nth i haystack) needle)
          (push i result)))
    (nreverse result)))

;Función adaptada (índices empiezan en 1 )
(defun obtiene_posiciones (símbolo lista)
    (mapcar #'1+ (get-positions símbolo lista)) ; +1 
)

(defun cuenta_ocurrencias (símbolo lista)
    (length (obtiene_posiciones símbolo lista))
)

(defun es_3_en_línea? (hilera)
    (OR (EVERY #'(lambda (casilla) (string= *símbolo_IA* casilla)) hilera)
        (EVERY #'(lambda (casilla) (string= *símbolo_humano* casilla)) hilera)
    )
) 

(defun pertenece_3_en_línea? (índice tablero)
    (let* (
            (índices_hileras (gethash índice *mapeo_casilla_hilera*))
            (hileras (mapcar #'(lambda (índice) (obtiene_hilera índice tablero)) índices_hileras ))
        )
        (SOME #'identity (mapcar #'es_3_en_línea? hileras))
    )
)

;Poner ficha (# de casilla destino)…
;Función usada en la primera etapa
;SIN VALIDACIONES 
(defun poner_ficha (símbolo índice estado)
    (let* (
            (tablero (first estado))
            (etapa (second estado))
            (mano_fichas_IA (third estado))
            (mano_fichas_humano (fourth estado))
            (cementerio_fichas_IA (fifth estado))
            (cementerio_fichas_humano (sixth estado))

            (nuevo_tablero (copy-tree tablero))
            (nueva_etapa etapa)
            (nueva_mano_fichas_IA mano_fichas_IA)
            (nueva_mano_fichas_humano mano_fichas_humano)
            (nuevo_cementerio_fichas_IA cementerio_fichas_IA)
            (nuevo_cementerio_fichas_humano cementerio_fichas_humano)
        )
        (REPLACE nuevo_tablero (list símbolo) :start1 (- índice 1)  ) ; Pone la ficha en el índice
        (if (string= símbolo *símbolo_IA*) (decf nueva_mano_fichas_IA) (decf nueva_mano_fichas_humano)) ; Quita la ficha de la mano correspondiente
        (if (AND (= nueva_mano_fichas_IA 0) (= nueva_mano_fichas_humano 0)) (setq nueva_etapa 2) ) ; Cambio de etapa
        (list nuevo_tablero nueva_etapa nueva_mano_fichas_IA nueva_mano_fichas_humano nuevo_cementerio_fichas_IA nuevo_cementerio_fichas_humano) ; Nuevo estado
    )
)

;Mover ficha (# casilla origen # casilla destino)…
;Función usada en la segunda etapa
;SIN VALIDACIONES 
(defun mover_ficha (índice_origen índice_destino estado)
    (let* (
            (tablero (first estado))
            (etapa (second estado))
            (mano_fichas_IA (third estado))
            (mano_fichas_humano (fourth estado))
            (cementerio_fichas_IA (fifth estado))
            (cementerio_fichas_humano (sixth estado))

            (nuevo_tablero (copy-tree tablero))
            (nueva_etapa etapa)
            (nueva_mano_fichas_IA mano_fichas_IA)
            (nueva_mano_fichas_humano mano_fichas_humano)
            (nuevo_cementerio_fichas_IA cementerio_fichas_IA)
            (nuevo_cementerio_fichas_humano cementerio_fichas_humano)

            (símbolo (obtiene_casilla índice_origen tablero))
        )
        (REPLACE nuevo_tablero (list NIL) :start1 (- índice_origen 1)  ) ; Quita la ficha en el origen
        (REPLACE nuevo_tablero (list símbolo) :start1 (- índice_destino 1)  ) ; Pone la ficha en el destino 
        (list nuevo_tablero nueva_etapa nueva_mano_fichas_IA nueva_mano_fichas_humano nuevo_cementerio_fichas_IA nuevo_cementerio_fichas_humano) ; Nuevo estado
    )
)

;Remover ficha (# casilla)…
;Función usada en la primer y segunda etapa
;SIN VALIDACIONES
(defun remover_ficha (índice estado)
    (when (null índice) (return-from remover_ficha (copy-tree estado))) ;No se altera el estado

    (let* (
            (tablero (first estado))
            (etapa (second estado))
            (mano_fichas_IA (third estado))
            (mano_fichas_humano (fourth estado))
            (cementerio_fichas_IA (fifth estado))
            (cementerio_fichas_humano (sixth estado))

            (nuevo_tablero (copy-tree tablero))
            (nueva_etapa etapa)
            (nueva_mano_fichas_IA mano_fichas_IA)
            (nueva_mano_fichas_humano mano_fichas_humano)
            (nuevo_cementerio_fichas_IA cementerio_fichas_IA)
            (nuevo_cementerio_fichas_humano cementerio_fichas_humano)

            (símbolo (obtiene_casilla índice tablero))
        )
        (REPLACE nuevo_tablero (list NIL) :start1 (- índice 1)  ) ; Quita la ficha en el origen
        (if (string= símbolo *símbolo_IA*) (incf nuevo_cementerio_fichas_IA) (incf nuevo_cementerio_fichas_humano)) ; Pone la ficha en el cementerio correspondiente
        (list nuevo_tablero nueva_etapa nueva_mano_fichas_IA nueva_mano_fichas_humano nuevo_cementerio_fichas_IA nuevo_cementerio_fichas_humano) ; Nuevo estado
    )
)

(defun fin_de_juego? (estado)
    (let* (
            (tablero (first estado))
            (etapa  (second estado))
            (num_fichas_humano_tablero (cuenta_ocurrencias *símbolo_humano* tablero))
            (num_fichas_IA_tablero (cuenta_ocurrencias *símbolo_IA* tablero))
            (posibles_movimientos_IA (length (obtiene_descendientes_etapa2 *símbolo_IA* estado) ))
            (posibles_movimientos_humano (length (obtiene_descendientes_etapa2 *símbolo_humano* estado) ) )
        )
        (AND (= etapa 2) 
            (OR (< num_fichas_humano_tablero 3) (< num_fichas_IA_tablero 3) (= posibles_movimientos_IA 0) 
            (= posibles_movimientos_humano 0) ) )
    )
)

(defun es_poner_válido? (índice estado) 
    (let*((tablero (first estado)))
        (when (NOT (integerp índice)) (return-from es_poner_válido? NIL)) ;No es numero entero
        (when (NOT (AND (<= índice 24) (> índice 0)  )) (return-from es_poner_válido? NIL)) ;out of bounds
        (null (obtiene_casilla índice tablero))
    )
)

(defun es_quitar_válido_etapa1? (índice símbolo_jugador estado)
    (let*(
            (tablero (first estado))
            (símbolo_casilla NIL)
            (símbolo_enemigo NIL)
        )
        (when (NOT (integerp índice)) (return-from es_quitar_válido_etapa1? NIL)) ;No es numero entero
        (when (NOT (AND (<= índice 24) (> índice 0)  )) (return-from es_quitar_válido_etapa1? NIL)) ;out of bounds
        (setq símbolo_casilla (obtiene_casilla índice tablero))
        (setq símbolo_enemigo (obtiene_símbolo_enemigo símbolo_jugador))
        (AND (string=  símbolo_casilla símbolo_enemigo) (NOT (pertenece_3_en_línea? índice tablero)))
    )
)

(defun es_mover_valido? (símbolo origen destino estado)
    (let*(
        (tablero (first estado))
        (símbolo_origen NIL)
        (símbolo_destino NIL)
    )
        (when (OR (NOT (integerp origen)) (NOT (integerp destino))) (return-from es_mover_valido? NIL)) ;No es numero entero
        (when (OR (NOT (AND (<= origen 24) (> origen 0)  )) (NOT (AND (<= destino 24) (> destino 0)  )) ) (return-from es_mover_valido? NIL)) ;out of bounds
        (setq símbolo_origen  (obtiene_casilla origen tablero))
        (setq símbolo_destino (obtiene_casilla destino tablero))
        (AND (string= símbolo_origen símbolo) (null símbolo_destino) (numberp (position destino (gethash origen *adjacencias*))))
    )
)

(defun al_poner_se_va_a_generar_3enlínea? (índice símbolo estado)
    (let* (
            (nuevo_estado (poner_ficha símbolo índice estado))
            (nuevo_tablero (first nuevo_estado))
        )
        (pertenece_3_en_línea? índice nuevo_tablero)
    )    
)

(defun al_mover_se_va_a_generar_3enlínea? (índice_origen índice_destino estado)
    (let* (
            (nuevo_estado (copy-tree (mover_ficha índice_origen índice_destino estado)))
            (nuevo_tablero (first nuevo_estado))
        )
        (pertenece_3_en_línea? índice_destino nuevo_tablero)
    )    
)

; Operador en la primera etapa
; (símbolo_jugador poner_índice quitar_índice)
; Operador en la segunda etapa
; (símbolo_jugador (índice_origen índice_destino) quitar_índice)
(defun aplica_tirada_etapa_uno (tirada estado) 
    (let* ( (símbolo (first tirada))
            (índice_poner_ficha  (second tirada))
            (índice_quitar_ficha (third tirada))            
        )
        (remover_ficha índice_quitar_ficha (poner_ficha símbolo índice_poner_ficha estado))        
    )  
) 

(defun aplica_tirada_etapa_dos (tirada estado)
    (let* ( ;(símbolo (first tirada))
            (índices_origen_destino  (second tirada))
            (índice_origen  (first  índices_origen_destino))
            (índice_destino (second índices_origen_destino))
            (índice_quitar_ficha (third tirada))
        )
        (remover_ficha índice_quitar_ficha (mover_ficha índice_origen índice_destino estado))        
    )
)


(defun gano-hilera? (hilera símbolo)
    ;Se gana una hilera cuando todos (EVERY) los símbolos de la hilera son del jugador
    (EVERY #'(lambda (casilla) (string= símbolo casilla)) hilera)    
)

(defun evaluación-viabilidad-hilera_etapa1 (hilera símbolo)
    (let* (
            (numero_fichas_jugador (count símbolo hilera))
            (numero_fichas_enemigo (count (obtiene_símbolo_enemigo símbolo) hilera))
            (espacios_vacios (count NIL hilera))
        )
        (cond 
            ((gano-hilera? hilera símbolo) 40)
            ((gano-hilera? hilera (obtiene_símbolo_enemigo símbolo)) -30)
            ((AND (= numero_fichas_jugador 2) (= numero_fichas_enemigo 1)) 20)
            ((AND (= numero_fichas_enemigo 1) (= numero_fichas_jugador 2)) -20)
            ((AND (= numero_fichas_jugador 2) (= espacios_vacios 1)) 10)
            ((AND (= numero_fichas_enemigo 2) (= espacios_vacios 1)) -10)
            ((AND (= numero_fichas_jugador 1) (= espacios_vacios 2)) 5)
            ((AND (= numero_fichas_enemigo 1) (= espacios_vacios 2)) -5)
            (T 1)
        )
    )
    
)

(defun evaluación_etapa1 (estado símbolo)
    (let* (
            (tablero (first estado))
            ;(etapa (second estado))
            ;(mano_fichas_IA (third estado))
            ;(mano_fichas_humano (fourth estado))
            (cementerio_fichas_IA (fifth estado))
            (cementerio_fichas_humano (sixth estado))
            (cementerio_jugador (if (string= símbolo *símbolo_IA*) cementerio_fichas_IA cementerio_fichas_humano))
            (cementerio_enemigo (if (string= símbolo *símbolo_IA*) cementerio_fichas_humano cementerio_fichas_IA))
            (evaluación_hileras NIL)
            (evaluación_total 0)
        )

        (maphash (lambda (key value)
            (declare (ignore value))
            (let (
                    (hilera (obtiene_hilera key tablero))
                )
                (push (evaluación-viabilidad-hilera_etapa1 hilera símbolo) evaluación_hileras)
            )
        ) *config_3_linea*)
        (setq evaluación_total (+ (apply '+ evaluación_hileras) (* 5 cementerio_enemigo) (* -4.5 cementerio_jugador) ))        
        evaluación_total
    )
)


(defun evaluación-viabilidad-hilera_etapa2 (hilera símbolo)
    (let* (
            (numero_fichas_jugador (count símbolo hilera))
            (numero_fichas_enemigo (count (obtiene_símbolo_enemigo símbolo) hilera))
            (espacios_vacios (count NIL hilera))
        )
        (cond 
            ((gano-hilera? hilera símbolo) 50)
            ((gano-hilera? hilera (obtiene_símbolo_enemigo símbolo)) -40)
            ((AND (= numero_fichas_jugador 2) (= numero_fichas_enemigo 1)) 30)
            ((AND (= numero_fichas_enemigo 1) (= numero_fichas_jugador 2)) -10)
            ((AND (= numero_fichas_jugador 2) (= espacios_vacios 1)) 30)
            ((AND (= numero_fichas_enemigo 2) (= espacios_vacios 1)) -30)
            ((AND (= numero_fichas_jugador 1) (= espacios_vacios 2)) 10)
            ((AND (= numero_fichas_enemigo 1) (= espacios_vacios 2)) -5)
            (T 1)
        )
    )    
)

(defun evaluación_etapa2 (estado símbolo)
     (let* (
            (tablero (first estado))
            (cementerio_fichas_IA (fifth estado))
            (cementerio_fichas_humano (sixth estado))
            (cementerio_jugador (if (string= símbolo *símbolo_IA*) cementerio_fichas_IA cementerio_fichas_humano))
            (cementerio_enemigo (if (string= símbolo *símbolo_IA*) cementerio_fichas_humano cementerio_fichas_IA))
            (evaluación_hileras NIL)
            (evaluación_total 0)
        )
        (maphash (lambda (key value)
            (declare (ignore value))
            (let (
                    (hilera (obtiene_hilera key tablero))
                )
                (push (evaluación-viabilidad-hilera_etapa2 hilera símbolo) evaluación_hileras)
            )
        ) *config_3_linea*)
        (setq evaluación_total (+ (apply '+ evaluación_hileras) (* 6 cementerio_enemigo) (* -4.5 cementerio_jugador) ))
        (if (OR (> cementerio_enemigo 6) (= 0 (length (obtiene_descendientes_etapa2 (obtiene_símbolo_enemigo símbolo) estado) ) )) 
            (setq evaluación_total 1000))        
        evaluación_total
    )
)

(defun obtiene_descendientes_etapa1 (símbolo estado)
    (let* (
            (tablero (first estado))
            ;(etapa (second estado))
            (mano_fichas_IA (third estado))
            (mano_fichas_humano (fourth estado))
            ;(cementerio_fichas_IA (fifth estado))
            ;(cementerio_fichas_humano (sixth estado))
            (casillas_disponibles (obtiene_posiciones NIL tablero))
            (casillas_fichas_enemigas (obtiene_posiciones (obtiene_símbolo_enemigo símbolo) tablero))
            (casillas_donde_nosegenera_3enlínea NIL)            
            (casillas_donde_segenera_3enlínea NIL)
            (poner_donde_nosegenera_3enlínea NIL)
            (poner_donde_segenera_3enlínea NIL)
            (mano_jugador (if (string= símbolo *símbolo_IA*) mano_fichas_IA mano_fichas_humano) )            
            (descendientes NIL)
        )
        ;No hay descendientes, pues ya no tiene fichas que poner
        (when (OR (= mano_jugador 0)) (return-from obtiene_descendientes_etapa1) ) 
        ;Se obtienen las casillas donde no se generan config_3_linea
        (setq casillas_donde_nosegenera_3enlínea (REMOVE-IF #'(lambda (índice) (al_poner_se_va_a_generar_3enlínea? índice símbolo estado)) casillas_disponibles)) 
        ;Se obtienen las casillas donde SI se forman 3 en linea
        (setq casillas_donde_segenera_3enlínea (SET-DIFFERENCE casillas_disponibles casillas_donde_nosegenera_3enlínea) )
        ;Se filtran aquellas casillas enemigas que ya pertenezcan a un 3 en linea 
        (setq casillas_fichas_enemigas (REMOVE-IF #'(lambda (índice) (pertenece_3_en_línea? índice tablero)) casillas_fichas_enemigas)  )
        ;(format t "casillas con fichas enemigas: ~a" casillas_fichas_enemigas)
        ;Se obtienen (poner_que_genera_3enlinea,casilla a quitar)
        (loop for casilla_3enlínea in casillas_donde_segenera_3enlínea do
            (loop for casilla_fichas_enemigas in casillas_fichas_enemigas do
                ;(format t "~%~a ~a ~%" casilla_3enlínea casilla_fichas_enemigas)
                (push (list símbolo casilla_3enlínea casilla_fichas_enemigas) poner_donde_segenera_3enlínea )
            )
        )
        ;Se obtienen (poner, NIL)
        (setq poner_donde_nosegenera_3enlínea (mapcar #'(lambda (índice) (list símbolo índice NIL)) casillas_donde_nosegenera_3enlínea))
        (setq descendientes (append poner_donde_nosegenera_3enlínea poner_donde_segenera_3enlínea))
        descendientes
    )
)


(defun obtiene_adjacencias_vacías (índice estado)
    (remove-if-not #'(lambda (adjacencia) (null (obtiene_casilla adjacencia (first estado)))) 
        (gethash índice *adjacencias*))
)

(defun obtiene_descendientes_etapa2 (símbolo estado)
    (let* (
            (tablero (first estado))
            (casillas_fichas_jugador (obtiene_posiciones símbolo tablero))
            (casillas_fichas_enemigas (obtiene_posiciones (obtiene_símbolo_enemigo símbolo) tablero))
            (adjacencias NIL)
            (descendientes NIL)
        )
        ; Filtra las casillas enemigas que ya pertenecen a un 3 en línea
        (setq casillas_fichas_enemigas (REMOVE-IF #'(lambda (índice) (pertenece_3_en_línea? índice tablero)) casillas_fichas_enemigas)  )
        ;3loop
        (loop for casilla_jugador_origen in casillas_fichas_jugador do
            (setq adjacencias (OBTIENE_ADJACENCIAS_VACÍAS casilla_jugador_origen estado ))
            (loop for adjacencia_destino in adjacencias do
                (if (al_mover_se_va_a_generar_3enlínea? casilla_jugador_origen adjacencia_destino estado) 
                    (progn 
                        (if (= 0 (length casillas_fichas_enemigas )) (push (list símbolo (list casilla_jugador_origen adjacencia_destino) NIL) descendientes)  )
                        (loop for casilla_fichas_enemigas in casillas_fichas_enemigas do 
                            (push (list símbolo (list casilla_jugador_origen adjacencia_destino) casilla_fichas_enemigas) descendientes)
                        )                    
                    )                    
                    (push (list símbolo (list casilla_jugador_origen adjacencia_destino) NIL) descendientes)
                )
            )
        )
        descendientes
    )
)

(defun hay_casillas_enemigas_para_mover? (símbolo estado)
    (let* (
            (tablero (first estado))
            (casillas_fichas_enemigas (obtiene_posiciones (obtiene_símbolo_enemigo símbolo) tablero))
        )
        (setq casillas_fichas_enemigas (REMOVE-IF #'(lambda (índice) (pertenece_3_en_línea? índice tablero)) casillas_fichas_enemigas)  )
        (> (length casillas_fichas_enemigas) 0)
    )
)

(defun ordena_movimientos_etapa1 (movimientos estado)
    (declare (ignore estado))
    (sort (copy-alist movimientos) 
        (lambda (a b) 
            (> (length (gethash (second a) *adjacencias*)) (length (gethash (second b) *adjacencias*)))            
        )
    )
)

(defun ordena_movimientos_etapa2 (movimientos estado)
    (declare (ignore estado))
    (sort (copy-alist movimientos) 
        (lambda (a b) 
            (> (length (gethash (second (second a)) *adjacencias*)) (length (gethash (second (second b)) *adjacencias*)))            
        )
    )
)


(defun fin_de_juego_etapa1? (estado) 
    (let* (
            (mano_fichas_IA (third estado))
            (mano_fichas_humano (fourth estado))
        )
        (AND (= mano_fichas_IA 0) (= mano_fichas_humano 0))
    )
)

(defun fin_de_juego_etapa2? (estado) 
    (let* (
            (numero_fichas_IA     (cuenta_ocurrencias *símbolo_IA*     (first estado)))
            (numero_fichas_humano (cuenta_ocurrencias *símbolo_humano* (first estado)))
            (posibles_movimientos_IA (length (obtiene_descendientes_etapa2 *símbolo_IA* estado) ))
            (posibles_movimientos_humano (length (obtiene_descendientes_etapa2 *símbolo_humano* estado) ) )

        )
        (OR (< numero_fichas_IA 3) 
            (< numero_fichas_humano 3) 
            (= posibles_movimientos_IA 0) 
            (= posibles_movimientos_humano 0) 
        )
    )
)

(defun juego_etapa_1 ()
    (let* ( (poner_humano 0)
            (quitar_humano NIL)
            (tirada_humano NIL)
            (mejor_movimientovalor NIL)
            (mejor_movimiento_IA NIL)
        )
        (dibuja_tablero *estado*)
        (format t "~%Turno humano, elige la casilla a tirar: ")
        (setq poner_humano (read))
        (terpri)
        (if (es_poner_válido? poner_humano *estado* )
            (progn ; bloque poner correcto
                ;se genero 3 en linea?
                    ;Preguntar que pieza quitar
                    ;Ver si es válida la pieza a quitar
                ;ejecutar la TIRADA
                (if (al_poner_se_va_a_generar_3enlínea? poner_humano *símbolo_humano* *estado*)
                    (progn
                        (format t "~%Se generará un 3 en línea, indique el índice de la ficha enemiga a quitar del tablero: ")
                        (setq quitar_humano (read))
                        (terpri)
                        (if (NOT (es_quitar_válido_etapa1? quitar_humano *símbolo_humano* *estado*)); tirada incorrecta
                            (progn
                                (format t "~c[31mTirada incorrecta, intente de nuevo ~c[0m~%" #\ESC #\ESC) 
                                (return-from juego_etapa_1)
                            )
                        )
                    )
                )
                (setq tirada_humano (list *símbolo_humano* poner_humano quitar_humano))                    
                (actualiza_estadoglobal (aplica_tirada_etapa_uno tirada_humano *estado*))
                (dibuja_tablero *estado*)
                ; Tirada-IA
                (format t "~%~c[94mLa IA está decidiendo su tirada...~c[0m~%" #\ESC #\ESC)
                (setq mejor_movimientovalor 
                    (Negamax-alphabeta *estado* 0 *profundidad-máxima* MOST-NEGATIVE-FIXNUM MOST-POSITIVE-FIXNUM 
                    #'evaluación_etapa1 #'obtiene_descendientes_etapa1 #'fin_de_juego_etapa1? #'ordena_movimientos_etapa1 #'aplica_tirada_etapa_uno                
                )) ;(negamax)
                (setq mejor_movimiento_IA (first mejor_movimientovalor))
                (actualiza_estadoglobal (aplica_tirada_etapa_uno mejor_movimiento_IA *estado*))
                (format t "~c[94mLa IA decidió tirar en la casilla: ~a~c[0m~%" #\ESC (second mejor_movimiento_IA) #\ESC)
                (if (not(null (third mejor_movimiento_IA))) (format t "~%~c[94mLa IA decidió quitar la ficha: ~a~c[0m~%" #\ESC (third mejor_movimiento_IA) #\ESC))

            )
            ;tirada incorrecta
            (format t "~c[31mTirada incorrecta, intente de nuevo ~c[0m~%" #\ESC #\ESC) 
        )
    )
)

(defun juego_etapa_2 ()
    (let* ( (mover_origen_humano 0)
            (mover_destino_humano 0)
            (quitar_humano NIL)
            (tirada_humano NIL)
            (mejor_movimientovalor NIL)
            (mejor_movimiento_IA NIL)
        )
        (dibuja_tablero *estado*)
        (format t "~%Turno humano, elige el número de la casilla a mover: ")
        (setq mover_origen_humano (read))
        (terpri)
        (format t "~%Elige el número de la casilla destino: ")
        (setq mover_destino_humano (read))
        (terpri)
        (if (es_mover_valido? *símbolo_humano* mover_origen_humano mover_destino_humano *estado*) 
            (progn ; bloque poner correcto
                (if (AND (al_mover_se_va_a_generar_3enlínea? mover_origen_humano mover_destino_humano *estado*)
                         (HAY_CASILLAS_ENEMIGAS_PARA_MOVER? *símbolo_humano* *estado*)
                    )
                    (progn
                        (format t "~%Se generará un 3 en línea, indique el índice de la ficha enemiga a quitar del tablero: ")
                        (setq quitar_humano (read))
                        (terpri)
                        (if (NOT (es_quitar_válido_etapa1? quitar_humano *símbolo_humano* *estado*)); tirada incorrecta
                            (progn
                                (format t "~c[31mTirada incorrecta, intente de nuevo ~c[0m~%" #\ESC #\ESC) 
                                (return-from juego_etapa_2)
                            )
                        )
                    )
                )
                (setq tirada_humano (list *símbolo_humano* (list mover_origen_humano mover_destino_humano) quitar_humano))                    
                (actualiza_estadoglobal (aplica_tirada_etapa_dos tirada_humano *estado*))
                (format t "~%===================Juego del molino==================")
                (dibuja_tablero *estado*)
                (if (fin_de_juego? *estado*) (return-from juego_etapa_2) )
     
                (format t "~%~c[94mLa IA está decidiendo su tirada...~c[0m~%" #\ESC #\ESC)
                (setq mejor_movimientovalor 
                    (Negamax-alphabeta *estado* 0 *profundidad-máxima* MOST-NEGATIVE-FIXNUM MOST-POSITIVE-FIXNUM 
                    #'evaluación_etapa2 #'obtiene_descendientes_etapa2 #'fin_de_juego_etapa2? #'ordena_movimientos_etapa2 #'aplica_tirada_etapa_dos                
                )) ;(negamax)
                (setq mejor_movimiento_IA (first mejor_movimientovalor))
                (actualiza_estadoglobal (aplica_tirada_etapa_dos mejor_movimiento_IA *estado*))
                (format t "~c[94mLa IA decidió mover la casilla: ~a a la posición: ~a ~c[0m~%" #\ESC (first (second mejor_movimiento_IA)) (second (second mejor_movimiento_IA)) #\ESC)
                (if (not(null (third mejor_movimiento_IA))) (format t "~%~c[94mLa IA decidió quitar la ficha: ~a~c[0m~%" #\ESC (third mejor_movimiento_IA) #\ESC))
            )
            ;tirada incorrecta
            (format t "~c[31mTirada incorrecta, intente de nuevo ~c[0m~%" #\ESC #\ESC) 
        )
    )
)

(defun anuncia_ganador (estado)
    (let* (
            (numero_fichas_IA     (cuenta_ocurrencias *símbolo_IA*     (first estado)))
            (numero_fichas_humano (cuenta_ocurrencias *símbolo_humano* (first estado)))
            (posibles_movimientos_IA (length (obtiene_descendientes_etapa2 *símbolo_IA* estado) ))
            (posibles_movimientos_humano (length (obtiene_descendientes_etapa2 *símbolo_humano* estado) ) )

        )
        (cond ((OR (< numero_fichas_IA 3)     (= posibles_movimientos_IA 0)  ) (format t "~%Ganó el jugador humano~%"))
              ((OR (< numero_fichas_humano 3) (= posibles_movimientos_humano 0)  ) (format t "~%Ganó la IA~%"))
        )
    )

)

(defun reset-all()
    (setq *tablero* (loop for i from 1 to 24 collect NIL))
    (setq *etapa* 1)
    (setq *mano_fichas_IA* 9)
    (setq *mano_fichas_humano* 9)
    (setq *cementerio_fichas_IA* 0)
    (setq *cementerio_fichas_humano* 0)
    (setq *estado* (list *tablero* *etapa* *mano_fichas_IA* *mano_fichas_humano* *cementerio_fichas_IA* *cementerio_fichas_humano*))

)

(defun juego ()
    (reset-all)
    (loop named game while (NOT (fin_de_juego? *estado*)) do 
        (format t "~%===================Juego del molino==================")
        (cond 
            ((= *etapa* 1)  (juego_etapa_1))
            ((= *etapa* 2)  (juego_etapa_2))
        )
    )
    ;(dibuja-tablero *estado* )
    (format t "~%===================Juego del molino==================")
    (dibuja_tablero *estado*)
    (format t "~%Fin de juego~%")    
    (anuncia_ganador *estado*)
)

(format t "Para iniciar el juego, invoque la función (juego) sin argumentos")