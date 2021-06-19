; Moreno Zárate Víctor Gibrán
; Función principal: (juego)
; Variante en la cual si a un jugador se le manda a un tablero ganado, puede tirar donde quiera
(defparameter *tablero* (loop for i from 1 to 81 collect NIL))
(defparameter *tablero-mayor* '((NIL NIL NIL) (NIL NIL NIL) (NIL NIL NIL) ))
(defparameter *locación-última-jugada* NIL)
(defparameter *ultima-jugada* NIL)
(defparameter *estado* (list *tablero* *tablero-mayor* *locación-última-jugada* *ultima-jugada*) )

(defparameter *símbolo-IA* 'O)
(defparameter *símbolo-humano* 'X)
(defparameter *símbolo-indeterminado* '?)
(defparameter *operadores* (loop for i from 1 to 81 collect i))

(defparameter *profundidad-máxima* 5) ;4 o 5? 

(defparameter *mapeo-tableromayor-tableromenor* (make-hash-table))
(setf (gethash 1 *mapeo-tableromayor-tableromenor*) '(1 2 3 10 11 12 19 20 21))
(setf (gethash 2 *mapeo-tableromayor-tableromenor*) '(4 5 6 13 14 15 22 23 24 ))
(setf (gethash 3 *mapeo-tableromayor-tableromenor*) '(7 8 9 16 17 18 25 26 27 ))
(setf (gethash 4 *mapeo-tableromayor-tableromenor*) '(28 29 30 37 38 39 46 47 48 ))
(setf (gethash 5 *mapeo-tableromayor-tableromenor*) '(31 32 33 40 41 42 49 50 51 ))
(setf (gethash 6 *mapeo-tableromayor-tableromenor*) '(34 35 36 43 44 45 52 53 54 ))
(setf (gethash 7 *mapeo-tableromayor-tableromenor*) '(55 56 57 64 65 66 73 74 75 ))
(setf (gethash 8 *mapeo-tableromayor-tableromenor*) '(58 59 60 67 68 69 76 77 78 ))
(setf (gethash 9 *mapeo-tableromayor-tableromenor*) '(61 62 63 70 71 72 79 80 81 ))

(defun obtiene-númerotablero (índice)
    (let (
            (númerotablero NIL)
        )
        (maphash (lambda (key value) (if (NOT (NULL (FIND índice value))) (SETQ númerotablero key))) *mapeo-tableromayor-tableromenor*)
        númerotablero
    )
)

(defun esta-índice-en-tableromenor? (índice número-tablero) 
    (when (NULL número-tablero) 

        (return-from esta-índice-en-tableromenor? T)    
    ); en el primer turno, la locación es NIL
    (NOT (NULL (FIND índice (gethash número-tablero *mapeo-tableromayor-tableromenor*))))
)

(defparameter *mapeo-locación-casilla* (make-hash-table))
(setf (gethash 1 *mapeo-locación-casilla*) '( 1  4  7 28 31 34 55 58 61)); NO
(setf (gethash 2 *mapeo-locación-casilla*) '( 2  5  8 29 32 35 56 59 62)); N 
(setf (gethash 3 *mapeo-locación-casilla*) '( 3  6  9 30 33 36 57 60 63)); NE
(setf (gethash 4 *mapeo-locación-casilla*) '(10 13 16 37 40 43 64 67 70)); O
(setf (gethash 5 *mapeo-locación-casilla*) '(11 14 17 38 41 44 65 68 71)); CENTRO
(setf (gethash 6 *mapeo-locación-casilla*) '(12 15 18 39 42 45 66 69 72)); E
(setf (gethash 7 *mapeo-locación-casilla*) '(19 22 25 46 49 52 73 76 79)); SO
(setf (gethash 8 *mapeo-locación-casilla*) '(20 23 26 47 50 53 74 77 80)); S
(setf (gethash 9 *mapeo-locación-casilla*) '(21 24 27 48 51 54 75 78 81)); SE

(defun esta-índice-en-locación? (índice locación)
    (when (NULL locación) (return-from esta-índice-en-locación? T)); en el primer turno, la locación es NIL
    (NOT (NULL (FIND índice (gethash locación *mapeo-locación-casilla*))))
)

(defun obtiene-locación (índice)
    (let (
            (locación NIL)
        )
        (maphash (lambda (key value) (if (NOT (NULL (FIND índice value))) (SETQ locación key))) *mapeo-locación-casilla*)
        locación
    )
)

(defun agrupa-en-3 (lst)
    (if lst (cons (list (car lst) (cadr lst) (caddr lst)) (agrupa-en-3 (cdddr lst))))
)

(defun obtener-índices-tableromenor (número-tableromayor)
    (nth-value 0 (gethash número-tableromayor *mapeo-tableromayor-tableromenor*))
)

(defun obtener-tableromenor (número-tableromayor tablero)
    (let (
            (índices-tableromenor (obtener-índices-tableromenor número-tableromayor))
        )
        (agrupa-en-3 (mapcar #'(lambda (índice) (nth (- índice 1) tablero))  índices-tableromenor))
    )
)

(defun obtiene-casilla (índice tablero)
    (nth (- índice 1) tablero)
)

(defun obtiene-dibujo (símbolo)
    (if (null símbolo) "_" símbolo)
)

(defun obtiene-casilla-dibujo (índice tablero)
    (let ((símbolo (obtiene-casilla índice tablero)))
        (obtiene-dibujo símbolo)
    )
)


(defun dibuja-numeración()
    (format t "~%Numeración del tablero: ~%")
    (format t " 1  2  3 |  4  5  6 |  7  8  9 ~%" )
    (format t "10 11 12 | 13 14 15 | 16 17 18 ~%" )
    (format t "19 20 21 | 22 23 24 | 25 26 27 ~%" )
    (format t "—————————+——————————+——————————~%")
    (format t "28 29 30 | 31 32 33 | 34 35 36 ~%")
    (format t "37 38 39 | 40 41 42 | 43 44 45 ~%")
    (format t "46 47 48 | 49 50 51 | 52 53 54 ~%")
    (format t "—————————+——————————+——————————~%")
    (format t "55 56 57 | 58 59 60 | 61 62 63 ~%" )
    (format t "64 65 66 | 67 68 69 | 70 71 72 ~%" )
    (format t "73 74 75 | 76 77 78 | 79 80 81 ~%" )
)

;;;; ==============FUNCIONES DE GATO 4x4 ADAPTADAS A 3X3 (y que inician en índice 1) =============

(defun casilla (renglón columna tableromenor)
    (nth (- columna 1) (nth (- renglón 1) tableromenor))
)

(defun renglón (índice tableromenor)
    (nth (- índice 1) tableromenor)
)

(defun columna (índice tableromenor)
    (mapcar #'(lambda (renglón) (nth (- índice 1) renglón)) tableromenor)
)

(defun diagonal (índice tableromenor)
    (cond ((= índice 1) ;solo hay dos diagonales
            (list 
                (casilla 1 1 tableromenor)
                (casilla 2 2 tableromenor)
                (casilla 3 3 tableromenor)
            )
        )
        ((= índice 2) 
            (list 
                (casilla 1 3 tableromenor)
                (casilla 2 2 tableromenor)
                (casilla 3 1 tableromenor)
            )
        )
    )
)

(defun renglones (tableromenor)
    tableromenor
)

(defun columnas (tableromenor)
    (loop :for n :from 1 :to 3 :collect (columna n tableromenor))    
)

(defun diagonales (tableromenor)
    (loop :for n :from 1 :to 2 :collect (diagonal n tableromenor))    
)

(defun hileras-movimientos (tableromenor)
    (concatenate 'list (renglones tableromenor) (columnas tableromenor) 
            (diagonales tableromenor) )
)

(defun es-hilera-viable? (hilera símbolo-enemigo)
    ;Es viable si no hay (notany) algún símbolo enemigo 
    (notany  #'(lambda (casilla) (string= casilla símbolo-enemigo)) hilera)
)

(defun tableromenor-lleno? (tableromenor)
    ;El tableromenor esta lleno cuando no hay ningún (NOTANY) NIL en los renglones
    (NOTANY #'(lambda (renglón) (numberp (position NIL renglón))) tableromenor)
)

(defun gano-hilera? (hilera símbolo)
    ;Se gana una hilera cuando todos (EVERY) los símbolos de la hilera son del jugador
    (EVERY #'(lambda (casilla) (string= símbolo casilla)) hilera)    
)

(defun ganó-jugador? (símbolo tableromenor)
    ;Un jugador gana el juego cuando gana alguna (SOME) de todas las hileras
    (SOME #'(lambda (hilera) (gano-hilera? hilera símbolo)) (hileras-movimientos tableromenor))
)

(defun alguien-ganó? (tableromenor)    
    (OR (ganó-jugador? *símbolo-IA* tableromenor) (ganó-jugador?  *símbolo-humano* tableromenor))
) 

(defun fin-de-minijuego? (tableromenor)
    ;El juego finaliza cuando alguien gana o se llena el tableromenor
    (OR (alguien-ganó? tableromenor) (tableromenor-lleno? tableromenor))    
)

(defun símbolo-enemigo (símbolo)
    (if (string= símbolo *símbolo-IA*) *símbolo-humano* *símbolo-IA*)
)

; El código del gato 4x4 y el metagato utilizan diferente notación para los índices
(defun cambio-de-notación (índice)
    (cond 
        ((= índice 1) (list  1 1))
        ((= índice 2) (list  1 2))
        ((= índice 3) (list  1 3))
        ((= índice 4) (list  2 1))
        ((= índice 5) (list  2 2))
        ((= índice 6) (list  2 3))
        ((= índice 7) (list  3 1))
        ((= índice 8) (list  3 2))
        ((= índice 9) (list  3 3))
    )
)

(defun casilla-notaciónalt (índice tableromenor)
    (let* (
            (lista-índices (cambio-de-notación índice))
            (renglón (first lista-índices))
            (columna (second lista-índices))
        )
        (casilla renglón columna tableromenor)
    )
)

(defun convierte-viabilidad-a-numero (viabilidad)
    (if viabilidad 1 0)
)

(defun evaluación-viabilidad-hilera (hilera símbolo exponente)
    ; (viabilidad de la hilera) * (exponente^(número de símbolos en la hilera))
    (* (convierte-viabilidad-a-numero (es-hilera-viable? hilera (símbolo-enemigo símbolo))) (expt exponente (count símbolo hilera))   )
)

(defun evaluación-tableromenor (tablero símbolo exponente)
    (let (
            (opciones-ganadoras 0)
            (opciones-perdedoras 0)
            (posibles-posiciones (hileras-movimientos tablero))
        )
        (setq opciones-ganadoras  (reduce #'+ (mapcar #'(lambda (hilera) (evaluación-viabilidad-hilera hilera símbolo exponente)) posibles-posiciones)))
        (setq opciones-perdedoras (reduce #'+ (mapcar #'(lambda (hilera) (evaluación-viabilidad-hilera hilera (símbolo-enemigo símbolo) exponente ) ) posibles-posiciones)))
        (- opciones-ganadoras opciones-perdedoras)
    )
)


;; ===========================================================================

(defun dibuja-tablero (estado)
    (let (
            (tablero (first estado)) ;tablero general
            (tablero-mayor (second estado)); estado del tablero grande
        )        
        (format t "~%Estado del tablero:    Num. casillas:                 Num. y estado tablero mayor:~%")
        (format t "~a ~a ~a | ~a ~a ~a | ~a ~a ~a   1  2  3 |  4  5  6 |  7  8  9~%" (obtiene-casilla-dibujo 1 tablero)(obtiene-casilla-dibujo 2 tablero) (obtiene-casilla-dibujo 3 tablero) (obtiene-casilla-dibujo 4 tablero) (obtiene-casilla-dibujo 5 tablero) (obtiene-casilla-dibujo 6 tablero) (obtiene-casilla-dibujo 7 tablero)(obtiene-casilla-dibujo 8 tablero) (obtiene-casilla-dibujo 9 tablero))
        (format t "~a ~a ~a | ~a ~a ~a | ~a ~a ~a  10 11 12 | 13 14 15 | 16 17 18 ~%" (obtiene-casilla-dibujo 10 tablero)(obtiene-casilla-dibujo 11 tablero) (obtiene-casilla-dibujo 12 tablero) (obtiene-casilla-dibujo 13 tablero) (obtiene-casilla-dibujo 14 tablero) (obtiene-casilla-dibujo 15 tablero) (obtiene-casilla-dibujo 16 tablero)(obtiene-casilla-dibujo 17 tablero) (obtiene-casilla-dibujo 18 tablero))
        (format t "~a ~a ~a | ~a ~a ~a | ~a ~a ~a  19 20 21 | 22 23 24 | 25 26 27    1 | 2 | 3   ~A | ~A | ~A  ~%" (obtiene-casilla-dibujo 19 tablero)(obtiene-casilla-dibujo 20 tablero) (obtiene-casilla-dibujo 21 tablero) (obtiene-casilla-dibujo 22 tablero) (obtiene-casilla-dibujo 23 tablero) (obtiene-casilla-dibujo 24 tablero) (obtiene-casilla-dibujo 25 tablero)(obtiene-casilla-dibujo 26 tablero) (obtiene-casilla-dibujo 27 tablero) (obtiene-dibujo (casilla-notaciónalt 1 tablero-mayor)) (obtiene-dibujo (casilla-notaciónalt 2 tablero-mayor))  (obtiene-dibujo (casilla-notaciónalt 3 tablero-mayor)) )
        (format t "——————+———————+——————  —————————+——————————+——————————  ———+———+——— ———+———+———~%")
        (format t "~a ~a ~a | ~a ~a ~a | ~a ~a ~a  28 29 30 | 31 32 33 | 34 35 36    4 | 5 | 6   ~A | ~A | ~A  ~%" (obtiene-casilla-dibujo 28 tablero)(obtiene-casilla-dibujo 29 tablero) (obtiene-casilla-dibujo 30 tablero) (obtiene-casilla-dibujo 31 tablero) (obtiene-casilla-dibujo 32 tablero) (obtiene-casilla-dibujo 33 tablero) (obtiene-casilla-dibujo 34 tablero)(obtiene-casilla-dibujo 35 tablero) (obtiene-casilla-dibujo 36 tablero) (obtiene-dibujo (casilla-notaciónalt 4 tablero-mayor)) (obtiene-dibujo (casilla-notaciónalt 5 tablero-mayor))  (obtiene-dibujo (casilla-notaciónalt 6 tablero-mayor)))
        (format t "~a ~a ~a | ~a ~a ~a | ~a ~a ~a  37 38 39 | 40 41 42 | 43 44 45   ———+———+——— ———+———+———~%" (obtiene-casilla-dibujo 37 tablero)(obtiene-casilla-dibujo 38 tablero) (obtiene-casilla-dibujo 39 tablero) (obtiene-casilla-dibujo 40 tablero) (obtiene-casilla-dibujo 41 tablero) (obtiene-casilla-dibujo 42 tablero) (obtiene-casilla-dibujo 43 tablero)(obtiene-casilla-dibujo 44 tablero) (obtiene-casilla-dibujo 45 tablero))
        (format t "~a ~a ~a | ~a ~a ~a | ~a ~a ~a  46 47 48 | 49 50 51 | 52 53 54    7 | 8 | 9   ~A | ~A | ~A  ~%" (obtiene-casilla-dibujo 46 tablero)(obtiene-casilla-dibujo 47 tablero) (obtiene-casilla-dibujo 48 tablero) (obtiene-casilla-dibujo 49 tablero) (obtiene-casilla-dibujo 50 tablero) (obtiene-casilla-dibujo 51 tablero) (obtiene-casilla-dibujo 52 tablero)(obtiene-casilla-dibujo 53 tablero) (obtiene-casilla-dibujo 54 tablero) (obtiene-dibujo (casilla-notaciónalt 7 tablero-mayor)) (obtiene-dibujo (casilla-notaciónalt 8 tablero-mayor))  (obtiene-dibujo (casilla-notaciónalt 9 tablero-mayor)))
        (format t "——————+———————+——————  —————————+——————————+——————————~%")
        (format t "~a ~a ~a | ~a ~a ~a | ~a ~a ~a  55 56 57 | 58 59 60 | 61 62 63   ~%" (obtiene-casilla-dibujo 55 tablero)(obtiene-casilla-dibujo 56 tablero) (obtiene-casilla-dibujo 57 tablero) (obtiene-casilla-dibujo 58 tablero) (obtiene-casilla-dibujo 59 tablero) (obtiene-casilla-dibujo 60 tablero) (obtiene-casilla-dibujo 61 tablero)(obtiene-casilla-dibujo 62 tablero) (obtiene-casilla-dibujo 63 tablero))
        (format t "~a ~a ~a | ~a ~a ~a | ~a ~a ~a  64 65 66 | 67 68 69 | 70 71 72 ~%" (obtiene-casilla-dibujo 64 tablero)(obtiene-casilla-dibujo 65 tablero) (obtiene-casilla-dibujo 66 tablero) (obtiene-casilla-dibujo 67 tablero) (obtiene-casilla-dibujo 68 tablero) (obtiene-casilla-dibujo 69 tablero) (obtiene-casilla-dibujo 70 tablero)(obtiene-casilla-dibujo 71 tablero) (obtiene-casilla-dibujo 72 tablero))
        (format t "~a ~a ~a | ~a ~a ~a | ~a ~a ~a  73 74 75 | 76 77 78 | 79 80 81 ~%" (obtiene-casilla-dibujo 73 tablero)(obtiene-casilla-dibujo 74 tablero) (obtiene-casilla-dibujo 75 tablero) (obtiene-casilla-dibujo 76 tablero) (obtiene-casilla-dibujo 77 tablero) (obtiene-casilla-dibujo 78 tablero) (obtiene-casilla-dibujo 79 tablero)(obtiene-casilla-dibujo 80 tablero) (obtiene-casilla-dibujo 81 tablero))
    )

)

(defun fin-de-juego? (estado)
    ; El juego se termina cuando alguien gana una hilera del tablero mayor o cuando se llenan todas las casillas
    ; de los tableros menores
    (let* (
            (tablero (first estado))
            (tablero-mayor (second estado))
            (tablerojuego-lleno? (NOT (numberp (position NIL tablero)))) 
            (fin-de-juego-tableromayor? (fin-de-minijuego? tablero-mayor))
        )
        (OR tablerojuego-lleno? fin-de-juego-tableromayor?)
    )
)

(defun es-tirada-válida? (índice estado)
    (let* (
            (tablero (first estado))
            ;(tablero-mayor (second estado))
            (locación-jugada-anterior (third estado))
            (casilla-vacía? NIL)
            (tableromayor-correcto? NIL)
            (tableromenor-no-ganado? NIL)
            (númerotablero-tirada NIL)
        )
        (when (NOT (integerp índice)) (return-from es-tirada-válida? NIL)) ;No es numero entero
        (when (NOT (AND (<= índice 81) (> índice 0)  )) (return-from es-tirada-válida? NIL)) ;out of bounds

        (setq casilla-vacía? (null (obtiene-casilla índice tablero )) )   
        (setq tableromayor-correcto? (esta-índice-en-tableromenor? índice locación-jugada-anterior))
        (setq númerotablero-tirada (obtiene-númerotablero índice) )

        (setq tableromenor-no-ganado? (NOT (fin-de-minijuego?  (obtener-tableromenor númerotablero-tirada tablero))  ))        

        (AND casilla-vacía? tableromayor-correcto? tableromenor-no-ganado?)
    )
)

(defun actualiza-tableromayor (tablero)
    (let (
            (tableromayor NIL)
        )
        (maphash (lambda (key value)
            (declare (ignore value))
            (let (
                    (tableromenor (obtener-tableromenor key tablero))
                )
                (cond 
                    ((ganó-jugador? *símbolo-humano* tableromenor) (push *símbolo-humano* tableromayor) )
                    ((ganó-jugador? *símbolo-IA* tableromenor) (push *símbolo-IA* tableromayor) ) 
                    ((tableromenor-lleno? tableromenor) (push *símbolo-indeterminado* tableromayor))
                    (T (push NIL tableromayor))
                )
            )
        ) *mapeo-tableromayor-tableromenor*)
        (agrupa-en-3 (reverse tableromayor))
    )
)

(defun aplica-tirada (símbolo índice estado) ;Devuelve un nuevo estado
    (let* (
            (tablero (first estado))
            (tablero-mayor (second estado))
            (nuevo-tablero (copy-tree tablero))
            (nuevo-tablero-mayor (copy-tree tablero-mayor))
            (nuevo-última-locación NIL)
        )
        (REPLACE nuevo-tablero (list símbolo) :start1 (- índice 1)  )
        (setq nuevo-tablero-mayor (actualiza-tableromayor nuevo-tablero))
        (setq nuevo-última-locación (obtiene-locación índice) )
        ; Si se manda al jugador a un tablero que ya se ganó, puede tirar en cualquier parte
        (if (fin-de-minijuego?  (obtener-tableromenor nuevo-última-locación nuevo-tablero)  )
            (setq nuevo-última-locación NIL)
        )
        (list nuevo-tablero nuevo-tablero-mayor nuevo-última-locación índice)
    )
)

(defun actualiza-estadoglobal (estado)
    (let* 
        (
            (tablero (copy-tree (first estado))) ; lista
            (tablero-mayor (copy-tree (second estado))) ;lista
            (locación-última-jugada (third estado) ) ;átomo
            (ultima-jugada (fourth estado)) ;átomo
        )
        (setq *tablero* tablero)
        (setq *tablero-mayor* tablero-mayor)
        (setq *locación-última-jugada* locación-última-jugada)
        (setq *ultima-jugada* ultima-jugada)
        (setq *estado* (list tablero tablero-mayor locación-última-jugada ultima-jugada))
    )
)

(defun operadores-válidos-metagato (operadores estado ) 
    (remove-if-not #'(lambda (operador) (es-tirada-válida? operador estado)) operadores)
)

;Devuelve que tan cerca esta de terminar un (mini)juego en un minitablero destino dado un movimiento
(defun heurísticaaux-tablerodestino-movimiento (movimiento estado)
    (let* (
            (tablero (first estado))
            ;(tablero-mayor (second estado))
            (númerotablero-tirada (obtiene-locación movimiento))
            (tablerodestino (obtener-tableromenor númerotablero-tirada tablero) )
        )
        (cond 
            ((fin-de-minijuego?  tablerodestino) 100)
            (T (- 9 (+ (count NIL (first tablerodestino)) (count NIL (second tablerodestino)) (count NIL (third tablerodestino)) )))
        )
    )
)

;Ordena los movimientos de acuerdo a que tan cerca esta de finalizar el tablero al que se le envia a un jugador
(defun ordena-movimientos (movimientos estado)
    (sort (copy-alist movimientos) 
        (lambda (a b) 
            (> (HEURÍSTICAAUX-TABLERODESTINO-MOVIMIENTO a estado) (HEURÍSTICAAUX-TABLERODESTINO-MOVIMIENTO b estado))            
        )
    )
)

(defun evaluación-tablerodestino (tablero símbolo exponente) 
    (when (fin-de-minijuego? tablero) (return-from evaluación-tablerodestino -65))
    (let (
            (posibles-posiciones (hileras-movimientos tablero))
        )
        (- (reduce #'+ (mapcar #'(lambda (hilera) (evaluación-viabilidad-hilera hilera símbolo exponente)) posibles-posiciones)))
    )
)

(defun evaluación (estado símbolo)
    (let* (
            (tablero (first estado)) ;tablero general
            (tablero-mayor (second estado)); estado del tablero grande
            ;(locación-jugada-anterior (third estado)); NIL si es tiro libre
            (ultima-jugada (fourth estado));indice de la ultima jugada
            (tablero-última-jugada (obtener-tableromenor (obtiene-númerotablero ultima-jugada) tablero));tablero donde se tiro la ultima jugada            
            (tablero-sigturno (obtener-tableromenor (obtiene-locación ultima-jugada) tablero) );tablero a donde se lleva al enemigo
            (evaluación-tablero-actual 0)
            (evaluación-tablero-sigjugada 0)
            (evaluación-tableroglobal 0)
            (tradeoff 0)
        )
        (cond 
            ((ganó-jugador? símbolo tablero-última-jugada) (setq tradeoff 1000)); Con esta jugada, el agente aseguro un tablero pequeño
            (T  (setq evaluación-tablero-actual    (evaluación-tableromenor tablero-última-jugada símbolo 3.4))
                (setq evaluación-tablero-sigjugada (- (evaluación-tableromenor tablero-sigturno (símbolo-enemigo símbolo) 3.2)))
                (setq evaluación-tableroglobal     (evaluación-tableromenor tablero-mayor símbolo 3.5))
                (setq tradeoff (+ evaluación-tablero-actual  evaluación-tablero-sigjugada evaluación-tableroglobal)) ;tradeoff entre la tirada del tablero actual, el destino de esta jugada y el tablero general
            )
        )
        ;(dibuja-tablero estado)
        ;(format t "~%ultima-jugada: ~A~%" ultima-jugada)
        ;(format t "~%evaluación-tablero-actual: ~A~%" evaluación-tablero-actual)
        ;(format t "~%evaluación-tablero-sigjugada: ~A~%" evaluación-tablero-sigjugada)
        ;(format t "~%evaluación-tableroglobal: ~A~%" evaluación-tableroglobal)
        ;(format t "~%Tradeoff: ~A~%" tradeoff)
        ;(read)
        tradeoff
    )
)

(defun Negamax-alphabeta (estado profundidad max-prof α β)
    (when (or (= profundidad max-prof) (fin-de-juego? estado))
        (let (
                (símbolo (if (= (mod profundidad 2 ) 0) *símbolo-IA* *símbolo-humano*))
            )
            (return-from Negamax-alphabeta (list NIL (evaluación estado SÍMBOLO))) ;https://www.chessprogramming.org/Negamax#How_to_Use_NegaMax
        )        
    )
    (let (
            (valor 0)
            (mejor-valor MOST-NEGATIVE-FIXNUM)
            (mejor-mov NIL)
            (operadores (operadores-válidos-metagato *operadores* estado))
            (símbolo (if (= (mod profundidad 2 ) 0) *símbolo-IA* *símbolo-humano*))
            (descendiente NIL)
        )
        ;(format t "No de descendientes:~A ~%" (length operadores))
        (setq operadores (ordena-movimientos operadores estado))
        (dolist (operador operadores)
            (setq descendiente (aplica-tirada símbolo operador estado))
            (setq valor (- (second (Negamax-alphabeta descendiente (+ profundidad 1) max-prof (- β) (- (MAX α mejor-valor )) ))) )
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

(defun anuncia-ganador (estado)
    (let* 
        (
            (tablero-mayor (second estado))
        )
        (cond 
            ((ganó-jugador? *símbolo-IA* tablero-mayor) (format t "Ganó la IA"))
            ((ganó-jugador? *símbolo-humano* tablero-mayor) (format t "Ganó el jugador humano"))
            (T (format t "Empate"))
        )
    )
)

(defun reset-all ()
    (setq *tablero* (loop for i from 1 to 81 collect NIL))
    (setq *tablero-mayor* '((NIL NIL NIL) (NIL NIL NIL) (NIL NIL NIL) ))
    (setq *locación-última-jugada* NIL)
    (setq *estado* (list *tablero* *tablero-mayor* *locación-última-jugada*) )
)

(defun juego ()
    (reset-all)
    (let (
            (tirada-humano 0)
            (mejor-movimientovalor NIL)
            (mejor-movimiento-IA NIL)
        )
        (dibuja-tablero *estado*)
        (loop named game while (NOT (fin-de-juego? *estado*)) do 
            (if (null *locación-última-jugada*) (format t "~c[32mPuedes tirar en cualquier casilla ~c[0m~%" #\ESC #\ESC) (format t "~c[32mDebes tirar en el tablero ~A ~c[0m~%" #\ESC *locación-última-jugada* #\ESC)   )
            (format t "~%Turno humano, elige la casilla a tirar: ")
            (setq tirada-humano (read))
            (if (es-tirada-válida? tirada-humano *estado* )
                (progn ; bloque tirada correcta
                    (actualiza-estadoglobal (aplica-tirada *símbolo-humano* tirada-humano *estado*))
                    (dibuja-tablero *estado*)
                    (if (fin-de-juego? *estado*) (return-from game) )
                    (format t "~%~c[94mLa IA esta decidiendo su tirada...~c[0m~%" #\ESC #\ESC)
                    (setq mejor-movimientovalor (Negamax-alphabeta *estado* 0 *profundidad-máxima* MOST-NEGATIVE-FIXNUM MOST-POSITIVE-FIXNUM )) ;(negamax)
                    (setq mejor-movimiento-IA (first mejor-movimientovalor))
                    (format t "~c[94mLa IA decidió tirar en la casilla: ~A, ubicada en el tablero: ~A ~c[0m~%" #\ESC mejor-movimiento-IA (obtiene-númerotablero mejor-movimiento-IA) #\ESC) ;(despliega info tirada)
                    (actualiza-estadoglobal (aplica-tirada *símbolo-IA* mejor-movimiento-IA *estado*)) ;(realizar mejor tirada) y actualizar globales
                    (dibuja-tablero *estado*)
                )
                ;tirada incorrecta
                (format t "~c[31mTirada incorrecta, intente de nuevo ~c[0m~%" #\ESC #\ESC) 
            )
        )
    )
    (dibuja-tablero *estado* )
    (format t "Fin de juego~%")
    (anuncia-ganador *estado*)
)
(format t "Para iniciar el juego, invoque la función (juego) sin argumentos")