;; Víctor Gibrán Moreno Zárate
;; http://idic.likufanele.com/~fundamentosia/
;;;======================================================================================
;;;  Ranas.lisp
;;;      Resuelve el problema de las ranas en el estanque con búsqueda ciega, 
;;;      a lo profundo y a lo ancho.
;;;   
;;;      Representación de los estados: 
;;;         Lista con dos sublistas internas, una por cada orilla. 
;;;         En cada orilla, posición de las ranas verdes (V) y de las ranas cafes (C)
;;;
;;;                 Estado incial:        Estado meta:
;;;                 (V V V _ C C C)       (C C C _ V V V)
;;;
;;;     Ej:  (blind-search '(V V V _ C C C)      '(C C C _ V V V )   :breath-first )
;;;     Original: Dr. Salvador Godoy C.
;;;     Modificado por: Víctor Gibrán Moreno Zárate
;;;======================================================================================

; TODO: Checar si el estado tambien esta en open (best first)
; TODO: Checar si el estado esta en open con mejor/peor prioridad (a*)

(defparameter  *open* '())    ;; Frontera de busqueda...                                              
(defparameter  *memory* '())  ;; Memoria de intentos previos

(defparameter  *ops*  '(; Ranas verdes
                        ; V,C - Color
                        ; 0-2 - Primera/segunda/tercera rana mas a la derecha/izquierda
                        ; 1-3 - Tamaño del salto
                        (:1st-rightmost-green-frog-1-jump   (V 0 1)) ;Celda inmediatamente sig.
                        (:1st-rightmost-green-frog-2-jump   (V 0 2)) ;Salta 1 ranas
                        (:1st-rightmost-green-frog-3-jump   (V 0 3)) ;Salta 2 ranas
                        (:2nd-rightmost-green-frog-1-jump   (V 1 1))
                        (:2nd-rightmost-green-frog-2-jump   (V 1 2))  
                        (:2nd-rightmost-green-frog-3-jump   (V 1 3))
                        (:3rd-rightmost-green-frog-1-jump   (V 2 1))
                        (:3rd-rightmost-green-frog-2-jump   (V 2 2))  
                        (:3rd-rightmost-green-frog-3-jump   (V 2 3))  
                        ;Ranas cafes
                        (:1st-leftmost_-brown-frog-1-jump    (C 0 1)) ;Celda inmediatamente sig.
                        (:1st-leftmost_-brown-frog-2-jump    (C 0 2)) ;Salta 1 ranas
                        (:1st-leftmost_-brown-frog-3-jump    (C 0 3)) ;Salta 2 ranas
                        (:2nd-leftmost_-brown-frog-1-jump    (C 1 1))
                        (:2nd-leftmost_-brown-frog-2-jump    (C 1 2))  
                        (:2nd-leftmost_-brown-frog-3-jump    (C 1 3))
                        (:3rd-leftmost_-brown-frog-1-jump    (C 2 1))
                        (:3rd-leftmost_-brown-frog-2-jump    (C 2 2))  
                        (:3rd-leftmost_-brown-frog-3-jump    (C 2 3))  
                        ) )

(defparameter  *id*  -1)  ;; Identificador del ultimo nodo creado
(defparameter  *current-ancestor*  nil)  ;;Id del ancestro común a todos los descendientes que se generen
(defparameter  *solucion*  nil)  ;;lista donde se almacenará la solución recuperada de la memoria

(defparameter *nodos-creados* 0)
(defparameter *nodos-expandidos* 0)
(defparameter *longitud-maxima-frontera-busqueda* 0)
(defparameter *longitud-solucion* 0)
(defparameter *tiempo1* 0 )
(defparameter *tiempo2* 0 )


;;;=======================================================================================
;;  Definiciones para la cola de prioridades 
;;  (prioridad  (nodos))
;;;=======================================================================================
(defun make-pq (alist)
  (stable-sort (copy-alist alist) (lambda (a b) (< (car a) (car b)))))

(define-modify-macro insert-pq (pair)
                     (lambda (pq pair) (make-pq (cons pair pq)))) 

(define-modify-macro remove-pq-aux () cdr) 

(defmacro remove-pq (pq)
  `(let ((aux (copy-alist ,pq)))
     (REMOVE-PQ-AUX ,pq)
     (car aux)))
;;;=======================================================================================
;;;=======================================================================================

;;;=======================================================================================
;;  CREATE-NODE (estado  op)  
;;      estado - Un estado del problema a resolver (sistema)...
;;          op - El operador cuya aplicación generó el [estado]...
;;;=======================================================================================
(defun  create-node (estado  op nivel)
  "Construye y regresa un nuevo nodo de búsqueda que contiene al estado y operador recibidos como parámetro"
      (incf  *id*)  ;;incrementamos primero para que lo último en procesarse sea la respuesta
      (list  *id*  estado  *current-ancestor*  (first op) nivel) )  ;;los nodos generados son descendientes de *current-ancestor*

;; Busca un estado en *open*
(defun is-in-open? (estado cola)
    (cond ((OR (null cola) (null estado) ) nil)
          ((equal (third (first cola)) estado ) (first cola))
          (T (is-in-open? estado (rest cola)))
    )
)

;;Insercion para A-star
(defun insert-A-star (nodo-en-open nuevo-nodo nueva-prioridad) 
    ;d) Si el estado se encuentra en OPEN, con PEOR COSTO
    ;que el recién calculado, eliminar el estado en OPEN y
    ;sustituirlo p 
    (cond ((> (first nodo-en-open) nueva-prioridad ) 
            (setq *open* (remove nodo-en-open *open*))
            (insert-pq *open* (cons nueva-prioridad nuevo-nodo))
        )
        (T NIL)     ;c) Si el estado se encuentra en OPEN, con un MEJOR
                    ;COSTO que el recién calculado, descartar el nuevo
                    ;estado y dejar el que está en OPEN.
    )
)

;;;=======================================================================================
;;  INSERT-TO-OPEN   y   GET-FROM-OPEN  
;;        
;;        Insert-to-open  recibe una lista y una llave que identifica el metodo a usar para insertar:
;;             :depth-first     Inserta los elementos de la lista en orden inverso y por el inicio de la lista
;;             :breath-first    Inserta los elementos de la lista en orden normal y por el final de la lista
;;        Get-from-open  siempre retira el primer elemento de la lista *open*
;;;=======================================================================================
;;TODO: Checar insercion bestfs y a-star
(defun insert-to-open (estado  op nivel metodo &optional prioridad) 
    "Permite insertar nodos de la frontera de busqueda *open* de forma apta para buscar a lo profundo y a lo ancho"
    (let ((nodo  (create-node  estado  op nivel)))
        (cond 
            ((eql  metodo  :depth-first)  (push  nodo  *open*))
            ((eql  metodo  :breath-first) (setq  *open*  (append  *open*  (list nodo)))) 
            ((eql  metodo  :best-first)   (if (NULL (is-in-open? estado *open*)) (insert-pq *open* (cons prioridad nodo )))) ; Best-Fit-> Si no esta open, insertalo, si no ignorar
            ((eql  metodo  :a-star)       (if (NULL (is-in-open? estado *open*)) (insert-pq *open* (cons prioridad nodo )) (insert-A-star (is-in-open? estado *open*) nodo prioridad ) ) )  ;Funcion insert a-star
            (T NIL)
        )
    )
    ; Indicadores 
    (incf *nodos-creados*)  
    (if (> (length *open*) *longitud-maxima-frontera-busqueda*) 
        (setq *longitud-maxima-frontera-busqueda*   (length *open*) )
    )
)

(defun aptitud (lista1 lista2)
    (cond 
        ((null lista1) 0)
        ((eql (first lista1) (first lista2) ) (+ 0  (aptitud (rest lista1) (rest lista2))))
        (T  (+ 1  (aptitud (rest lista1) (rest lista2))))
    )
)

(defun funcion-prioridad (estado metodo nivel)
    (cond ((eql metodo :best-first) (aptitud estado '(C C C _ V V V) ) )
          ((eql metodo :a-star) (+ nivel (aptitud estado '(C C C _ V V V) ) ))
          (T NIL)
    )    
)

(defun get-from-open (metodo)
"Recupera el siguiente elemento a revisar de  frontera de busqueda *open*"
    (incf *nodos-expandidos*)
    (cond 
        ((OR (eql metodo :best-first)  (eql metodo :a-star))  (rest (remove-pq *open*)) ) ; First-> prioridad ; Rest->Nodo
        (T (pop  *open*))
    )     
)

;;;=======================================================================================
;;  all-positions [needle , haystack]
;;        Devuelve todas las posiciones donde se encuentra el simbolo needle en la lista haystack
;;;=======================================================================================
(defun all-positions (needle haystack)
    (loop
        for element in haystack and position from 0
            when (eql element needle)
            collect position
    )
)

;;;=======================================================================================
;;  encuentra-posicion [color , posicion, estado]
;;        Devuelve la posición (primera mas a la derecha/izquierda, segunda ..., tercera... ) 
;;      de la rana segun su color en el estado.
;;;=======================================================================================
(defun encuentra-posicion (color posicion estado)
    (cond ((eql 'V color) (nth posicion (reverse (all-positions color estado))))
            ((eql 'C color) (nth posicion (all-positions color estado)))
            (T NIL)
    )
)


;;;=======================================================================================
;;  encuentra-nil [estado]
;;        Devuelve la posición donde se encuentra el simbolo _, que representa la posicion vacia en el estanque
;;;=======================================================================================
(defun encuentra-nil (estado)
    (position '_ estado)
)

;;;=======================================================================================
;;  VALID-OPERATOR [op, estado]
;;        Predicado.  Indica si es posible aplicar el operador [op] a [estado] segun los recursos en el estanque 
;;;=======================================================================================
(defun valid-operator? (op  estado)
    (let* ( (color          (first  (second op)))                         
            (posicion-nd    (second (second op)))   ; 1st, 2nd, 3rd
            (numero-saltos  (third  (second op)))
            (posicion-null  (encuentra-nil estado))
            (posicion-rana  (encuentra-posicion color posicion-nd  estado) )
            (nueva-posicion 0)
        )
        (cond ((eql color 'V) (setq nueva-posicion (+ posicion-rana numero-saltos)))
              ((eql color 'C) (setq nueva-posicion (- posicion-rana numero-saltos)))
        )
        (eql nueva-posicion posicion-null) ;Checa si la nueva posicion de la rana está vacía
    )
)  

;;;=======================================================================================
;;  VALID-STATE (estado)
;;        Predicado.  Indica si [estado]  es valido segun las restricciones del problema
;;                          Es decir, si por lo menos alguna rana puede seguir saltando
;;;=======================================================================================
(defun  valid-state? (estado)
    (if (equal estado '(C C C _ V V V)) ; Si el estado es el estado meta...
       T ;...Es válido, si no...
       (numberp (position T ( ; Verifica que almenos algún operador en *ops* siga siendo válido en este estado
                loop for op in *ops* ; Si es así, habrá por lo menos un True en la lista que regresa el collect
                collect (valid-operator? op estado) ;Con position obtenemos la posible posición de ese True, y si no es un
                                                    ;número entonces el estado ya no es válido
                )
            )
        ) 
    )
)

    
;;;=======================================================================================
;;  APPLY-OPERATOR (op, estado)
;;        Resuelve la tarea básica de cambiar de estado el sistema...
;;;=======================================================================================
(defun  apply-operator (op  estado) 
    "Obtiene el descendiente de [estado] al aplicarle  [op]  SIN VALIDACIONES"
    (let* ( (color          (first  (second op)))                         
            (posicion-nd    (second (second op)))   ; 1st, 2nd, 3rd
            (numero-saltos  (third  (second op)))
            (posicion-rana  (encuentra-posicion color posicion-nd estado) )
            (nueva-posicion 0)
            (nuevo-estado (copy-list estado))
        )
        (cond ((eql color 'V) (setq nueva-posicion (+ posicion-rana numero-saltos)))
              ((eql color 'C) (setq nueva-posicion (- posicion-rana numero-saltos)))
        )
        (rotatef (nth nueva-posicion nuevo-estado) (nth posicion-rana nuevo-estado))
        nuevo-estado
    )
)

;;;=======================================================================================
;;  EXPAND (estado)
;;        Construye y regresa una lista con todos los descendientes validos de [estado]
;;;=======================================================================================
;;; TODO: when (operador valido) -> aplicalo -> if estado valido? -> agregalo a los descendientes
;; TODO: Los saltos lo esta dando alreves :V
(defun expand (estado)
    "Obtiene todos los descendientes válidos de un estado, aplicando todos los operadores en *ops* en ese mismo órden"
    (let ((descendientes  nil)
            (nuevo-estado  nil)
        )
        (dolist  (op  *Ops*  descendientes)
            (when (valid-operator?  op  estado)
                (setq  nuevo-estado  (apply-operator  op estado))
                (if (valid-state?  nuevo-estado) 
                    (setq  descendientes  (cons  (list nuevo-estado op) descendientes))
                )
            )
        )
    )
)


;;;=======================================================================================
;;  REMEMBER-STATE?  y  FILTER-MEMORIES
;;        Permiten administrar la memoria de intentos previos
;;;=======================================================================================
(defun  remember-state?  (estado  lista-memoria)
"Busca un estado en una lista de nodos que sirve como memoria de intentos previos
     el estado tiene estructura:  [(<m0><c0><b0>) (<m1><c1><b1>)],
     el nodo tiene estructura : [<Id> <estado> <id-ancestro> <operador> ]"  
     (cond ((null  lista-memoria)  Nil)
	        ((equal  estado  (second (first  lista-memoria)))  T)  ;;el estado es igual al que se encuentra en el nodo?
		(T  (remember-state?  estado  (rest  lista-memoria))))  )


(defun  filter-memories (lista-estados-y-ops) 
"Filtra una lista de estados-y-operadores quitando aquellos elementos cuyo estado está en la memoria *memory*
     la lista de estados y operadores tiene estructura: [(<estado> <op>) (<estado> <op>) ... ]"
     (cond ((null  lista-estados-y-ops)  Nil)
	       ((remember-state? (first (first  lista-estados-y-ops)) *memory*)  ;; si se recuerda el primer elemento de la lista, filtrarlo...
		       (filter-memories  (rest  lista-estados-y-ops)))
		(T  (cons  (first lista-estados-y-ops) (filter-memories  (rest  lista-estados-y-ops))))) )  ;; de lo contrario, incluirlo en la respuesta

;;;=======================================================================================
;;  EXTRACT-SOLUTION  y  DISPLAY-SOLUTION
;;       Recuperan y despliegan la secuencia de solucion del problema...
;;       extract-solution   recibe un nodo (el que contiene al estado meta) que ya se encuentra en la memoria y
;;                                    rastrea todos sus ancestros hasta llegar  al  nodo que contiene al estado inicial...
;;       display-solution  despliega en pantalla la lista global *solucion* donde ya se encuentra, en orden correcto,
;;                                    el proceso de solución del problema...
;;;=======================================================================================
(defun extract-solution (nodo)
"Rastrea en *memory* todos los descendientes de [nodo] hasta llegar al estado inicial"
     (labels ((locate-node  (id  lista)       ;; función local que busca un nodo por Id  y si lo encuentra regresa el nodo completo
		  (cond ((null  lista)  Nil)
		        ((eql  id  (first (first  lista))) (first  lista))
		        (T  (locate-node  id (rest  lista))))))
	  (let ((current  (locate-node  (first  nodo)  *memory*)))
	     (loop  while  (not (null  current))  do                        
		 (push  current  *solucion*)     ;; agregar a la solución el nodo actual
		 (setq  current  (locate-node  (third  current) *memory*))))  ;; y luego cambiar a su antecesor...
	     *solucion*))


(defun  display-solution (lista-nodos)
"Despliega la solución en forma conveniente y numerando los pasos"
    (format  t  "Solución con ~A  pasos:~%~%" (1- (length  lista-nodos)))
    (setq *longitud-solucion* (1- (length  lista-nodos)))
    (let  ((nodo  nil))
         (dotimes  (i (length  lista-nodos))
	      (setq  nodo  (nth  i  lista-nodos))
	      (if  (= i 0)
		   (format t "Inicio en: ~A~%" (second  nodo))  ;; a partir de este estado inicial
	       ;;else
		   (format t "\(~2A\)  aplicando ~20A se llega a ~A~%"  i (fourth  nodo)  (second  nodo)))))  )  ;; imprimir el número de paso, operador y estado...


(defun display-indicadores ()
	(format  t  "~%Nodos creados: ~A ~%" *nodos-creados*)
	(format  t  "Nodos expandidos: ~A ~%" *nodos-expandidos*)
    (format  t  "Longitud máxima de la Frontera de búsqueda: ~A ~%" *longitud-maxima-frontera-busqueda*)
	(format  t  "Longitud de la solución: ~A operadores ~%" *longitud-solucion*)
	(format  t  "Tiempo para encontrar la solución: ~,4f segundos~%" (/ (- *tiempo2* *tiempo1*) internal-time-units-per-second) )
)

;;;=======================================================================================
;;  RESET-ALL  y  BLIND-SEARCH
;;
;;       Recuperan y despliegan la secuencia de solucion del problema...
;;
;;       reset-all   Reinicializa todas las variables globales para una nueva ejecución
;;       blind-search  Función principal, realiza búsqueda desde un estado inicial a un estado meta
;;;=======================================================================================
(defun reset-all () 
"Reinicia todas las variables globales para realizar una nueva búsqueda..."
    (setq  *open*  nil)
    (setq  *memory*  nil)
    (setq  *id*  0)
    (setq  *current-ancestor*  nil)
    (setq  *solucion*  nil)
    (setq *nodos-creados* 0)
    (setq *nodos-expandidos* 0)
    (setq *longitud-maxima-frontera-busqueda* 0)
    (setq  *longitud-solucion* 0)
    (setq *tiempo1* 0 )
    (setq *tiempo2* 0 )
)


(defun  blind-search (edo-inicial  edo-meta  metodo)
"Realiza una búsqueda ciega, por el método especificado y desde un estado inicial hasta un estado meta
    los métodos posibles son:  :depth-first - búsqueda en profundidad
                               :breath-first - búsqueda en anchura"
    (reset-all)
    (let ( ; Variables
            (nodo nil)
            (estado nil)
            (sucesores  '())
            (operador  nil)
            (meta-encontrada  nil)
            (ancestro nil)
            (nivel 0)
        )
        (setq *tiempo1* (get-internal-run-time))
        (insert-to-open   edo-inicial  nil 0 metodo (funcion-prioridad edo-inicial metodo nivel) )
        (loop until  (or  meta-encontrada (null *open*))  do
                (setq nodo    (get-from-open metodo)              ;;Extraer el siguiente nodo de la frontera de búsquea
                    estado      (second  nodo)               ;;Identificar el estado y operador que contiene
                    ancestro    (third nodo)
                    operador    (fourth  nodo)
                    nivel       (fifth nodo)
                )             
                (push  nodo  *memory*)                     ;;Recordarlo antes de que algo pueda pasar...
                (cond    
                    ((equal  edo-meta  estado)
                        (setq *tiempo2* (get-internal-run-time))  
                        (format  t  "Éxito. Meta encontrada en ~A  intentos~%" (first  nodo))
                        (display-solution  (extract-solution  nodo))
                        (display-indicadores)
                        (setq  meta-encontrada  T)
                    )
                    (t 
                        (setq  *current-ancestor*  (first  nodo)) 
                        ; (setq  *current-level*  (first  nodo)) 
                        (setq  sucesores  (expand estado))
                        (setq  sucesores  (filter-memories  sucesores))     ;;Filtrar los estados ya revisados...
                        (loop for  element  in  sucesores  do
                            (insert-to-open  (first element)  (second element) (+ 1 nivel) metodo  (funcion-prioridad (first element) metodo nivel)  )
                        )
                    )
                )
        )
    )
)
			     
     
;;;=======================================================================================
;;;=======================================================================================
