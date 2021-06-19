; Víctor Gibrán Moreno Zárate

(defparameter *tablero-test* '((X O NIL O)( O O O NIL )( NIL X X NIL )( X NIL NIL X )))

(defparameter *símbolo-IA* 'O)
(defparameter *símbolo-humano* 'X)
(defparameter *profundidad-máxima* 7)

(defparameter  *ops*  '(
        ;(:Etiqueta-humana renglón-columna-del-tablero número-casilla)
        (:Tira-posición-1  (0 0 1)) 
        (:Tira-posición-2  (0 1 2)) 
        (:Tira-posición-3  (0 2 3)) 
        (:Tira-posición-4  (0 3 4))
        (:Tira-posición-5  (1 0 5))  
        (:Tira-posición-6  (1 1 6))
        (:Tira-posición-7  (1 2 7))
        (:Tira-posición-8  (1 3 8)) 
        (:Tira-posición-9  (2 0 9))
        (:Tira-posición-10 (2 1 10))
        (:Tira-posición-11 (2 2 11))
        (:Tira-posición-12 (2 3 12))
        (:Tira-posición-13 (3 0 13))
        (:Tira-posición-14 (3 1 14))
        (:Tira-posición-15 (3 2 15))
        (:Tira-posición-16 (3 3 16))
    ) 
)

(defun casilla (renglón columna tablero)
    (nth columna (nth renglón tablero))
)

(defun renglón (índice tablero)
    (nth índice tablero)
)

(defun columna (índice tablero)
    (mapcar #'(lambda (renglón) (nth índice renglón)) tablero)
)

(defun diagonal (índice tablero)
    (cond ((= índice 0) ;solo hay dos diagonales
            (list 
                (casilla 0 0 tablero)
                (casilla 1 1 tablero)
                (casilla 2 2 tablero)
                (casilla 3 3 tablero)
            )
        )
        ((= índice 1) 
            (list 
                (casilla 0 3 tablero)
                (casilla 1 2 tablero)
                (casilla 2 1 tablero)
                (casilla 3 0 tablero)
            )
        )
    )
)

(defun renglones (tablero)
    tablero
)

(defun columnas (tablero)
    (loop :for n :below 4 :collect (columna n tablero))    
)

(defun diagonales (tablero)
    (loop :for n :below 2 :collect (diagonal n tablero))    
)

(defun hileras-movimientos (tablero)
    (concatenate 'list (renglones tablero) (columnas tablero) 
            (diagonales tablero) )
)

(defun es-hilera-viable? (hilera símbolo-enemigo)
    ;Es viable si no hay (notany) algún símbolo enemigo 
    (notany  #'(lambda (casilla) (string= casilla símbolo-enemigo)) hilera)
)

(defun operador-válido? (operador tablero)
    (let ((renglón (first (second operador)))
          (columna (second (second operador))))
        ; Regresa si esta vacía la casilla donde quiere tirar
        (NULL (casilla renglón columna tablero))
    )
)
;(operador-válido? '(:Tira-posición-7  (1 2)) *tablero* )


(defun tablero-lleno? (tablero)
    ;El tablero esta lleno cuando no hay ningún (NOTANY) NIL en los renglones
    (NOTANY #'(lambda (renglón) (numberp (position NIL renglón))) tablero)
)

(defun gano-hilera? (hilera símbolo)
    ;Se gana una hilera cuando todos (EVERY) los símbolos de la hilera son del jugador
    (EVERY #'(lambda (casilla) (string= símbolo casilla)) hilera)    
)

(defun ganó-jugador? (símbolo tablero)
    ;Un jugador gana el juego cuando gana alguna (SOME) de todas las hileras
    (SOME #'(lambda (hilera) (gano-hilera? hilera símbolo)) (hileras-movimientos tablero))
)

(defun alguien-ganó? (tablero)    
    (OR (ganó-jugador? *símbolo-IA* tablero) (ganó-jugador?  *símbolo-humano* tablero))
) 

(defun fin-de-juego? (tablero)
    ;El juego finaliza cuando alguien gana o se llena el tablero
    (OR (alguien-ganó? tablero) (tablero-lleno? tablero))    
)

; Deprecated
(defun evaluación (tablero)
    (let ((opciones-ganadoras 0)
            (opciones-perdedoras 0)
            (posibles-posiciones (hileras-movimientos tablero))
        )
        (cond ((ganó-jugador? *símbolo-IA* tablero) 100) 
              ((ganó-jugador? *símbolo-humano* tablero) -100)
              (T    (setq opciones-ganadoras 
                        (count T (mapcar #'(lambda (x) (es-hilera-viable? x *símbolo-humano*)) posibles-posiciones)))
                    (setq opciones-perdedoras 
                        (count T (mapcar #'(lambda (x) (es-hilera-viable? x *símbolo-IA*)) posibles-posiciones)))
                    ;(format t "Ventaja IA: ~A~%" opciones-ganadoras )
                    ;(format t "Ventaja enemigo ~A~%" opciones-perdedoras )
                    (- opciones-ganadoras opciones-perdedoras))
        )
    )
)

(defun símbolo-enemigo (símbolo)
    (if (string= símbolo *símbolo-IA*) *símbolo-humano* *símbolo-IA*)
)

(defun convierte-viabilidad-a-numero (viabilidad)
    (if viabilidad 1 0)
)

(defun evaluación-viabilidad-hilera (hilera símbolo)
    ; (viabilidad de la casilla) * (3^(número de símbolos en la hilera))
    (* (convierte-viabilidad-a-numero (es-hilera-viable? hilera (símbolo-enemigo símbolo))) (expt 3 (count símbolo hilera))   )
)

;Evaluación del tablero DESDE la perspectiva de la IA
(defun evaluación-mejorada (tablero)
    (let (
            (opciones-ganadoras 0)
            (opciones-perdedoras 0)
            (posibles-posiciones (hileras-movimientos tablero))
        )
        (setq opciones-ganadoras  (reduce #'+ (mapcar #'(lambda (hilera) (evaluación-viabilidad-hilera hilera *símbolo-IA*)) posibles-posiciones)))
        (setq opciones-perdedoras (reduce #'+ (mapcar #'(lambda (hilera) (evaluación-viabilidad-hilera hilera *símbolo-humano*)) posibles-posiciones)))
        (- opciones-ganadoras opciones-perdedoras)
    )
)

(defun evaluación-relativa (tablero símbolo)
    (let (
            (opciones-ganadoras 0)
            (opciones-perdedoras 0)
            (posibles-posiciones (hileras-movimientos tablero))
        )
        (setq opciones-ganadoras  (reduce #'+ (mapcar #'(lambda (hilera) (evaluación-viabilidad-hilera hilera símbolo)) posibles-posiciones)))
        (setq opciones-perdedoras (reduce #'+ (mapcar #'(lambda (hilera) (evaluación-viabilidad-hilera hilera (símbolo-enemigo símbolo))) posibles-posiciones)))
        (- opciones-ganadoras opciones-perdedoras)
    )
)

(defun operadores-válidos (operadores tablero)
    (remove-if-not #'(lambda (operador) (operador-válido? operador tablero)) operadores)
)

(defun aplica-operador (operador símbolo tablero)
    (let* ((índice-renglón (first (second operador)))
          (índice-columna (second (second operador)))
        )
        (REPLACE (renglón índice-renglón tablero) (list símbolo) :start1 índice-columna  )
    )
)

(defun desaplica-operador (operador tablero)
    (aplica-operador operador NIL tablero)
)

(defun Negamax-alphabeta (estado profundidad max-prof α β)
    (when (or (= profundidad max-prof) (fin-de-juego? estado))
        (let ((signo  (if (= (mod profundidad 2 ) 0) 1 -1)))
            (return-from Negamax-alphabeta (list NIL (* signo (evaluación-mejorada estado)))) ;https://www.chessprogramming.org/Negamax#How_to_Use_NegaMax
        )        
    )
    (let (
            (valor 0)
            (mejor-valor MOST-NEGATIVE-FIXNUM)
            (mejor-mov NIL)
            (operadores (operadores-válidos *ops* estado))
            (símbolo (if (= (mod profundidad 2 ) 0) *símbolo-IA* *símbolo-humano*))
        )
        (dolist (operador operadores)
            (aplica-operador operador símbolo estado)
            (setq valor (- (second (Negamax-alphabeta estado (+ profundidad 1) max-prof (- β) (- (MAX α mejor-valor )) ))) )
            (desaplica-operador operador estado)
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


(defun tictactoe (tablero)
    (let* (
            (mejor-movimientovalor (Negamax-alphabeta tablero 0 *profundidad-máxima* MOST-NEGATIVE-FIXNUM MOST-POSITIVE-FIXNUM ))
            (mejor-movimiento (first mejor-movimientovalor)) ;Operador
            (mejor-valor (second mejor-movimientovalor))
            (tirada (third (second mejor-movimiento)))
        )        
        (format t "Operador a usar:~A con valor negamax: ~A. Tirada en la casilla ~A ~%" mejor-movimiento mejor-valor tirada)
        (setq *output* tirada)
    )  
)