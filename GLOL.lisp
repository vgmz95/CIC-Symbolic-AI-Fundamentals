;; Víctor Gibrán Moreno Zárate
;;;======================================================================================
;;;  GLOL.lisp
;;;      Resuelve el problema del granjero, lobo, oveja, legumbre con búsqueda ciega, 
;;;      a lo profundo y a lo ancho.
;;;   
;;;      Representación de los estados: 
;;;         Lista con dos sublistas internas, una por cada orilla. 
;;;         En cada orilla, posición del lobo (L), Oveja (O), Legumbe (Le) y Barca-Granjero (G)
;;;
;;;                 Estado incial:             Estado meta:
;;;                 L O Le G   L O Le G         L O Le  G   L O Le G
;;;               ((1 1 1  1) (0 0 0  0))      ((0 0 0  0) (1 1 1  1)) 
;;;
;;      Ej: (blind-search '((1 1 1  1) (0 0 0  0))  '((0 0 0  0) (1 1 1  1))     :depth-first )
;;;     Original: Dr. Salvador Godoy C.
;;;     Modificado por: Víctor Gibrán Moreno Zárate
;;;======================================================================================
(defparameter  *open* '())    ;; Frontera de busqueda...                                              
(defparameter  *memory* '())  ;; Memoria de intentos previos

(defparameter  *ops*  '( (:Lobo      (1 0 0))
                         (:Oveja     (0 1 0))
                         (:Legumbre  (0 0 1))
                         (:Granjero-Solo   (0 0 0))
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
;;  CREATE-NODE (estado  op)  
;;      estado - Un estado del problema a resolver (sistema)...
;;          op - El operador cuya aplicación generó el [estado]...
;;;=======================================================================================
(defun  create-node (estado  op)
  "Construye y regresa un nuevo nodo de búsqueda que contiene al estado y operador recibidos como parámetro"
      (incf  *id*)  ;;incrementamos primero para que lo último en procesarse sea la respuesta
      (list  *id*  estado  *current-ancestor*  (first op)) )  ;;los nodos generados son descendientes de *current-ancestor*

;;;=======================================================================================
;;  INSERT-TO-OPEN   y   GET-FROM-OPEN  
;;        
;;        Insert-to-open  recibe una lista y una llave que identifica el metodo a usar para insertar:
;;             :depth-first     Inserta los elementos de la lista en orden inverso y por el inicio de la lista
;;             :breath-first    Inserta los elementos de la lista en orden normal y por el final de la lista
;;        Get-from-open  siempre retira el primer elemento de la lista *open*
;;;=======================================================================================
(defun insert-to-open (estado  op  metodo &optional prioridad) 
    "Permite insertar nodos de la frontera de busqueda *open* de forma apta para buscar a lo profundo y a lo ancho"
    (let ((nodo  (create-node  estado  op)))
        (cond 
            ((eql  metodo  :depth-first)  (push  nodo  *open*))
            ((eql  metodo  :breath-first)   (setq  *open*  (append  *open*  (list nodo))))
            ((eql  metodo  :best-fs)    
                (push  (cons prioridad nodo)  *open*) 
                (stable-sort (copy-alist *open*) #'< :key #'first) 
            )
            (T NIL)
        )
    )
    ; Indicadores 
    (incf *nodos-creados*)  
    (if (> (length *open*) *longitud-maxima-frontera-busqueda*) 
        (setq *longitud-maxima-frontera-busqueda*   (length *open*) )
    )
)

(defun diferencia-listas (lista1 lista2)
    (cond 
        ((null lista1) 0)
        ((eql (first lista1) (first lista2) ) (+ 0  (diferencia-listas (rest lista1) (rest lista2))))
        (T  (+ 1  (diferencia-listas (rest lista1) (rest lista2))))
    )
)
(defun aptitud (estado &optional metodo)
    (+ (diferencia-listas (first estado) '(0 0 0 0) ) (diferencia-listas (second estado) '(1 1 1 1) ) )
)



(defun get-from-open (&optional (metodo :best-fs))
"Recupera el siguiente elemento a revisar de  frontera de busqueda *open*"
    (incf *nodos-expandidos*)
    (cond 
        ((eql metodo :best-fs)   (rest (pop  *open*)))
        (T (pop  *open*))
    )     
)

;;;=======================================================================================
;;  BARGE-SHORE (estado)
;;        Regresa la orilla del rio en la que se encuentra la barca en  [estado]
;;           0 - Orilla origen (primer sublista del estado)
;;           1 - Orilla destino (segunda sublista del estado)
;;;=======================================================================================
(defun  barge-shore (estado)
"Regresa la orilla del río en la que se encuentra la barca en el estado recibido como parámetro:  
  0 - origen  1 - destino"
     (if  (= 1 (fourth (first  estado)))  0  1))


;;;=======================================================================================
;;  VALID-OPERATOR [op, estado]
;;        Predicado.  Indica si es posible aplicar el operador [op] a [estado] segun los recursos en la orilla de la barca
;;;=======================================================================================
(defun valid-operator? (op  estado)
    "Predicado. Valida la aplicación de un operador a un estado...
    el estado tiene estructura:  [(<l0><o0><le0><b0>) (<l1><o1><le1><b1>)],
    el operador tiene estructura : [<etiqueta-humana> <lista operador con (<num lobo><num oveja><num legumbre>)>]"  
    (let* ( (orilla     (barge-shore  estado))                         
            (lobo       (first  (nth  orilla  estado)))   
            (oveja      (second (nth  orilla  estado)))
            (legumbre   (third  (nth  orilla  estado)))
        )
        (and    (>=  lobo    (first (second op)))              
                (>=  oveja   (second (second op)))
                (>= legumbre (third (second op)))
        )
    )
)  


;;;=======================================================================================
;;  VALID-STATE (estado)
;;        Predicado.  Indica si [estado]  es valido segun las restricciones del problema
;;                          Es decir, que el lobo no se quede solo con la oveja,
;;                          y la oveja no se quede sola con la legumbre
;;;=======================================================================================
(defun  valid-state? (estado)
    "Predicado. Valida  un estado según las restricciones generales del problema...
    el estado tiene estructura:  [(<l0><o0><le0><b0>) (<l1><o1><le1><b1>)]"
    (let (  (l0     (first  (first estado)))
            (o0     (second (first estado)))
            (le0    (third  (first estado)))
            (b0     (fourth (first estado)))
            (l1     (first  (second estado)))
            (o1     (second (second estado)))
            (le1    (third  (second estado)))
            (b1     (fourth (second estado)))
        )  
        (  NOT (OR
            (AND (= l0 o0  1) (zerop b0)) ; El lobo no se puede quedar con la oveja sin el granjero
            (AND (= o0 le0 1) (zerop b0)) ; La oveja se puede quedar con la legumbre sin el granjero
            (AND (= l1 o1  1) (zerop b1))  
            (AND (= o1 le1 1) (zerop b1)) )
        )
    )
)

    
;;;=======================================================================================
;;  APPLY-OPERATOR (op, estado)
;;        Resuelve la tarea básica de cambiar de estado el sistema...
;;;=======================================================================================

(defun flip (bit)  (boole  BOOLE-XOR  bit  1))

(defun  apply-operator (op  estado) 
"Obtiene el descendiente de [estado] al aplicarle  [op]  SIN VALIDACIONES"
    (let* ((orilla1  (first  estado))
            (orilla2  (second  estado))
            (l0   (first    orilla1))
            (o0   (second   orilla1))
            (le0  (third    orilla1))
            (b0   (fourth   orilla1))
            (l1   (first    orilla2))
            (o1   (second   orilla2))
            (le1  (third    orilla2))
            (b1   (fourth   orilla2))
            (orilla-barca  (barge-shore estado)) 
	        (operador (first op)))        
        (case operador 
	        (:Lobo 
                (if (= orilla-barca 0)  ;; restar elementos de la orilla con la barca y sumarlos en la otra orilla...
	                (list  (list  (decf l0) o0 le0 (flip b0))   (list  (incf l1) o1 le1 (flip b1)))
				    (list  (list  (incf l0) o0 le0 (flip b0))   (list  (decf l1) o1 le1 (flip b1)))
                )
            )
            (:Oveja   
                (if (= orilla-barca 0)  
                    (list  (list  l0 (decf o0) le0 (flip b0))   (list  l1 (incf o1) le1 (flip b1)))
                    (list  (list  l0 (incf o0) le0 (flip b0))   (list  l1 (decf o1) le1 (flip b1)))
                )
            ) 
            (:Legumbre 
                (if (= orilla-barca 0)  
                    (list  (list  l0 o0 (decf le0) (flip b0))   (list  l1 o1 (incf le1) (flip b1)))
                    (list  (list  l0 o0 (incf le0) (flip b0))   (list  l1 o1 (decf le1) (flip b1)))
                )
            )    
            (:Granjero-Solo 
                (if (= orilla-barca 0)  
                    (list  (list  l0 o0 le0 (flip b0))   (list  l1 o1 le1 (flip b1)))
                    (list  (list  l0 o0 le0 (flip b0))   (list  l1 o1 le1 (flip b1)))
                )
            )
            (T "error")
        )
    )
)


;;;=======================================================================================
;;  EXPAND (estado)
;;        Construye y regresa una lista con todos los descendientes validos de [estado]
;;;=======================================================================================
(defun expand (estado)
"Obtiene todos los descendientes válidos de un estado, aplicando todos los operadores en *ops* en ese mismo órden"
     (let ((descendientes  nil)
	     (nuevo-estado  nil))
           (dolist  (op  *Ops*  descendientes) 
	         (setq  nuevo-estado  (apply-operator  op estado))  ;; primero se aplica el operador  y  después
		 (when (and (valid-operator?  op  estado)           ;; se valida el resultado...
			    (valid-state?  nuevo-estado))
	                (setq  descendientes  (cons  (list nuevo-estado op) descendientes))))) )


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
     (setq  *nodos-creados* 0)
     (setq  *nodos-expandidos* 0)
     (setq  *longitud-maxima-frontera-busqueda* 0)
     (setq  *longitud-solucion* 0)
     (setq  *tiempo1* 0 )
     (setq  *tiempo2* 0 ))
     


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
        )
        (setq *tiempo1* (get-internal-run-time))
        (insert-to-open   edo-inicial  nil  metodo (aptitud edo-inicial) )
        (loop until  (or  meta-encontrada (null *open*))  do
                (setq nodo    (get-from-open metodo)              ;;Extraer el siguiente nodo de la frontera de búsquea
                    estado  (second  nodo)               ;;Identificar el estado y operador que contiene
                    operador  (third  nodo)
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
                        (setq  sucesores  (expand estado))
                        (setq  sucesores  (filter-memories  sucesores))     ;;Filtrar los estados ya revisados...
                        (loop for  element  in  sucesores  do
                            (insert-to-open  (first element)  (second element)  metodo  (aptitud (first element))  )
                        )
                    )
                )
        )
    )
)
			     
     
;;;=======================================================================================
;;;=======================================================================================
