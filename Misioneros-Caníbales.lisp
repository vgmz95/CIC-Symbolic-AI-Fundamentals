;;;======================================================================================
;;;  Misioneros-Canibales.lisp
;;;      Resuelve el problema de misioneros y canibales con búsqueda ciega, a lo profundo y a lo ancho.
;;;   
;;;      Representación de los estados: 
;;;         Lista con dos sublistas internas, una por cada orilla. 
;;;         En cada orilla, número de misioneros(M), caníbales(C) y barcas(B).
;;;                 Estado inciial:        Estado meta:
;;;                 M C B   M C B         M C B   M C B
;;;               ((3 3 1) (0 0 0))     ((0 0 0) (3 3 1))
;;;
;;;      Dr. Salvador Godoy C.
;;;  enero, 2012
;;;======================================================================================
(defparameter  *open* '())    ;; Frontera de busqueda...                                              
(defparameter  *memory* '())  ;; Memoria de intentos previos

(defparameter  *ops*  '( (:Un-Misionero         (1 0))
                         (:Dos-Misioneros       (2 0))
                         (:Misionero-y-Caníbal  (1 1))
                         (:Dos-Caníbales        (0 2))     
                         (:Un-Caníbal           (0 1))) )


(defparameter *nodos-creados* 0)
(defparameter *nodos-expandidos* 0)
(defparameter *longitud-maxima-frontera-busqueda* 0)
(defparameter  *longitud-solucion* 0)
(defparameter *tiempo* 0)
(defparameter *tiempo1* 0 )
(defparameter *tiempo2* 0 )



(defparameter  *id*  -1)  ;; Identificador del ultimo nodo creado
(defparameter  *current-ancestor*  nil)  ;;Id del ancestro común a todos los descendientes que se generen
(defparameter  *solucion*  nil)  ;;lista donde se almacenará la solución recuperada de la memoria

;;;=======================================================================================
;;  CREATE-NODE (estado  op)  
;;      estado - Un estado del problema a resolver (sistema)...
;;          op - El operador cuya aplicación generó el [estado]...
;;;=======================================================================================
(defun  create-node (estado  op)
  "Construye y regresa un nuevo nodo de búsqueda que contiene al estado y operador recibidos como parámetro"
      (incf  *id*)  ;;incrementamos primero para que lo último en procesarse sea la respuesta
	  (incf *nodos-creados*) 
      (list  *id*  estado  *current-ancestor*  (first op)) )  ;;los nodos generados son descendientes de *current-ancestor*

;;;=======================================================================================
;;  INSERT-TO-OPEN   y   GET-FROM-OPEN  
;;        
;;        Insert-to-open  recibe una lista y una llave que identifica el metodo a usar para insertar:
;;             :depth-first     Inserta los elementos de la lista en orden inverso y por el inicio de la lista
;;             :breath-first    Inserta los elementos de la lista en orden normal y por el final de la lista
;;        Get-from-open  siempre retira el primer elemento de la lista *open*
;;;=======================================================================================
(defun insert-to-open (estado  op  metodo) 
"Permite insertar nodos de la frontera de busqueda *open* de forma apta para buscar a lo profundo y a lo ancho"
	(let ((nodo  (create-node  estado  op)))
		(cond ((eql  metodo  :depth-first)
					(push  nodo  *open*))
			((eql  metodo  :breath-first)
				(setq  *open*  (append  *open*  (list nodo))))
		(T  Nil)))  
	(if (> (length *open*) *longitud-maxima-frontera-busqueda*) 
		(setq *longitud-maxima-frontera-busqueda*   (length *open*) ))
)


(defun get-from-open ()
"Recupera el siguiente elemento a revisar de  frontera de busqueda *open*"
      (pop  *open*))

;;;=======================================================================================
;;  BARGE-SHORE (estado)
;;        Regresa la orilla del rio en la que se encuentra la barca en  [estado]
;;           0 - Orilla origen (primer sublista del estado)
;;           1 - Orilla destino (segunda sublista del estado)
;;;=======================================================================================
(defun  barge-shore (estado)
"Regresa la orilla del río en la que se encuentra la barca en el estado recibido como parámetro:  
  0 - origen  1 - destino"
     (if  (= 1 (third (first  estado)))  0  1))


;;;=======================================================================================
;;  VALID-OPERATOR [op, estado]
;;        Predicado.  Indica si es posible aplicar el operador [op] a [estado] segun los recursos en la orilla de la barca
;;;=======================================================================================
(defun  valid-operator? (op  estado)
"Predicado. Valida la aplicación de un operador a un estado...
     el estado tiene estructura:  [(<m0><c0><b0>) (<m1><c1><b1>)],
     el operador tiene estructura : [<etiqueta-humana> <lista operador con (<num misioneros><num caníbales>)>]"  
  (let*  ((orilla  (barge-shore  estado))                         
	    (misioneros  (first  (nth  orilla  estado)))   
	    (canibales    (second  (nth  orilla  estado))))
    (and  (>=  misioneros  (first (second op)))              ;;el número de misioneros a mover, primer elemento de la lista-operador
               (>=  canibales   (second (second op)))))  )  ;;el número de caníbales a mover, segundo elemento de la lista-operador


;;;=======================================================================================
;;  VALID-STATE (estado)
;;        Predicado.  Indica si [estado]  es valido segun las restricciones del problema
;;                          Es decir, si en c/orilla hay igual o mayor numero de misioneros que de canibales
;;;=======================================================================================
(defun  valid-state? (estado)
"Predicado. Valida  un estado según las restricciones generales del problema...
       el estado tiene estructura:  [(<m0><c0><b0>) (<m1><c1><b1>)]"
    (let ((m0  (first (first estado)))        ;;el estado tiene estructura ((<m0><c0><b0>) (<m1><c1><b1>)) ...
	    (c0  (second (first estado)))
	    (m1  (first (second estado)))
	    (c1  (second (second estado))))
      (and  (or  (>=  m0 c0) (zerop m0))  ;;mayor o igual número de misioneros que caníbales, excepto si hay 0 misioneros
    	        (or  (>=  m1 c1) (zerop m1))))  )

    
;;;=======================================================================================
;;  APPLY-OPERATOR (op, estado)
;;        Resuelve la tarea básica de cambiar de estado el sistema...
;;;=======================================================================================

(defun flip (bit)  (boole  BOOLE-XOR  bit  1))

(defun  apply-operator (op  estado) 
"Obtiene el descendiente de [estado] al aplicarle  [op]  SIN VALIDACIONES"
    (let*  ((orilla1  (first  estado))
	       (orilla2  (second  estado))
	       (m0  (first orilla1))
   	       (c0   (second orilla1))
	       (b0   (third  orilla1))
	       (m1  (first  orilla2))
	       (c1   (second  orilla2))
	       (b1   (third   orilla2))
	       (orilla-barca  (barge-shore estado)) 
	       (operador (first op)))     ;; este operador es la etiqueta humana del operador...
	 (case operador 
	    (:Dos-Misioneros (if (= orilla-barca 0)  ;; restar elementos de la orilla con la barca y sumarlos en la otra orilla...
	                        (list  (list  (- m0 2) c0 (flip b0))   (list  (+ m1 2) c1 (flip b1)))
				(list  (list  (+ m0 2) c0 (flip b0))  (list (- m1 2) c1 (flip b1)))))
	    (:Un-Misionero   (if (= orilla-barca 0)  
	                        (list  (list  (- m0 1) c0 (flip b0))   (list  (+ m1 1) c1 (flip b1)))
				(list  (list  (+ m0 1) c0 (flip b0))  (list (- m1 1) c1 (flip b1))))) 
	    (:Misionero-y-Caníbal  
	                 (if (= orilla-barca 0)  
	 		        (list  (list  (- m0 1) (- c0 1) (flip b0))   (list  (+ m1 1) (+ c1 1) (flip b1)))
				(list  (list  (+ m0 1) (+ c0 1) (flip b0))  (list (- m1 1) (- c1 1) (flip b1)))))
	    (:Un-Caníbal (if (= orilla-barca 0)  
			   (list  (list  m0  (- c0 1) (flip b0))   (list  m1  (+ c1 1) (flip b1)))
			   (list  (list  m0  (+ c0 1) (flip b0))  (list  m1  (- c1 1) (flip b1))))) 
	    (:Dos-Caníbales  (if (= orilla-barca 0)  
	                        (list  (list  m0  (- c0 2) (flip b0))   (list  m1 (+ c1 2) (flip b1)))
				(list  (list  m0 (+ c0 2) (flip b0))  (list m1 (- c1 2) (flip b1)))))
	    (T "error"))))


;;;=======================================================================================
;;  EXPAND (estado)
;;        Construye y regresa una lista con todos los descendientes validos de [estado]
;;;=======================================================================================
;;; todo: check
(defun expand (estado)
"Obtiene todos los descendientes válidos de un estado, aplicando todos los operadores en *ops* en ese mismo órden"
     (let ((descendientes  nil)
	     (nuevo-estado  nil))
           (dolist  (op  *Ops*  descendientes) 
	         (setq  nuevo-estado  (apply-operator  op estado))  ;; primero se aplica el operador  y  después
		 (when (and (valid-operator?  op  estado)           ;; se valida el resultado...
			    (valid-state?  nuevo-estado))
					(incf *nodos-expandidos*)
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
	(format  t  "Tiempo para encontrar la solución: ~A segundos~%" (/ (- *tiempo2* *tiempo1*) internal-time-units-per-second) )
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
     (setq  *solucion*  nil))


(defun  blind-search (edo-inicial  edo-meta  metodo)
"Realiza una búsqueda ciega, por el método especificado y desde un estado inicial hasta un estado meta
    los métodos posibles son:  :depth-first - búsqueda en profundidad
                               :breath-first - búsqueda en anchura"
  (reset-all)
  (let ((nodo nil)
	  (estado nil)
	  (sucesores  '())
	  (operador  nil)
	  (meta-encontrada  nil))
      (setq *tiempo1* (get-internal-run-time))
      (insert-to-open   edo-inicial  nil  metodo)
      (loop until  (or  meta-encontrada
                        (null *open*))  do
	   (setq nodo    (get-from-open)              ;;Extraer el siguiente nodo de la frontera de búsquea
		     estado  (second  nodo)               ;;Identificar el estado y operador que contiene
		     operador  (third  nodo))             
	   (push  nodo  *memory*)                     ;;Recordarlo antes de que algo pueda pasar...
	   (cond    ((equal  edo-meta  estado)
	   					(setq *tiempo2* (get-internal-run-time))  
		                (format  t  "Éxito. Meta encontrada en ~A  intentos~%" (first  nodo))
		                (display-solution  (extract-solution  nodo))
						(display-indicadores)
		                (setq  meta-encontrada  T))
		         (t (setq  *current-ancestor*  (first  nodo)) 
			     (setq  sucesores  (expand estado))
			     (setq  sucesores  (filter-memories  sucesores))     ;;Filtrar los estados ya revisados...
			      (loop for  element  in  sucesores  do
				    (insert-to-open  (first element)  (second element)  metodo))))))  )
			     
     
;;;=======================================================================================
;;;=======================================================================================
