(defparameter *varislas* '(() () () ()))
(defparameter *generaciones* '(1 1 1 1))
(defparameter *poblacionInicial* '())
(defparameter *genes* '())
(defparameter *grafoProblema* '( (A (B)) (B (C)) (C (D)) (D (E)) (E (F)) (F (A))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; La funcion generaGenes nos definirá aquellos genes permitidos (número del color) 
;; dentro del cromosoma, nosotros los acotaremos entre los valores del 0 y el número de 
;; nodos del grafo del problema  -1. Dado que como máximo en un grafo podrá haber un 
;; número de colores distintos  igual al número de nodos del grafo.	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun generaGenes () 
	(loop for i from 0 to (- (length *grafoProblema*) 1)do
		(push i *genes*)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Esta función nos servirá para construir las islas , le pasaremos un valor y su isla 
;; destino y introducirá dicho valor (valor de la entrada VALOR) en la ISLA 
;; correspondiente cuya posicion dentro de *varislas* vendra dado por el valor de 
;; entrada "isla" 	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun sustitucion (valor isla) 
  (setf (nth isla *varislas*) (push valor (nth isla *varislas*)))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Esta función la usaremos para, a partir de una población inicial que le pasamos por 
;; parametro, dividirla entre las 4 islas que quedaran guardadas en el parámetro 
;; *varislas* y sobre las que trabajaremos en nuestro algoritmo. Para llevar a cabo la 
;;división de la población inicial nos ayudaremos de la función auxiliar *varislas*																		     ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun generaislas (poblacion)
	(let* ((tamPoblacion 0)
         ) 
		(setq tamPoblacion (length poblacion)) 
		(loop for i from 0 to (- tamPoblacion 1 )do 
	   
		(sustitucion (nth i poblacion) (mod i 4)) 
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Esta función la usaremos para genera la población inicial
;; Para ello le pasaremos como parametros : tamaño de la población  y número de nodos 
;; del grafo ( será el tamaño de cada cromosoma)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun generaPoblacionInicial (tamPoblacion numNodos) 
	(let* ((cromosoma ())
		   (poblacion '())
		)
		(loop for i from 0 to (- tamPoblacion 1) do 
		(loop for i from 0 to (- numNodos 1) do
			  (push (random numNodos) cromosoma))
		 (push cromosoma poblacion)
		 (setq cromosoma ())
		)
		poblacion
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; De la función cromosomaAleatorio nos ayudaremos para "rellenar" los huecos dejados 
;; por la migración con cromosomas generados aleatoriamente 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun cromosomaAleatorio (tam) 
	(let* (cromosoma '())
		(loop for i from 0 to (- tam 1) do
			  (push (random tam) cromosoma))
		cromosoma
	)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nos devolverá la evaluación de la isla a la que migran los cromosomas para poder 
;; ver cuales cromosomas del destino son los peores;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun destinoIsla (posisla)
	(let* (evaluacion '())
		(if (= posisla 3)
			(setf evaluacion (evaluacionIndividuos 0))
			(setf evaluacion (evaluacionIndividuos (+ posisla 1)))
		
		)
		(reverse evaluacion)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Realiza la función principal del modelo de islas , dado un númera de cromosomas 
;; viajeros (viajantes) , la evaluacion de la isla origen y la posicion de dicha isla
;; dentro de las 4 islas :																																					   ;;
;;		1) Comprueba que el número de viajantes no es 0																								   ;;
;;		2) Si es 0 , no hace nada , si es distinto de 0 :																							   ;;
;;																																											   ;;
;;			*) Tomará los K mejores individuos del origen (k  es igual al número de 
;;				viajantes) y los enviará a la isla siguiente, en nuestro caso será la 
;; 				isla que tendrá a su derecha, pues nuestra migración es en forma de 
;; 				cuadrado como hemos explicado en la documentación.																											   ;;
;;			*) Como la isla origen se ha quedado con menos población , rellenaremos estos 
;;				huecos con nuevos cromosomas generados aleatoriamente
;;																																										       ;;
;;			*) Para terminar en el destino sustituimos los K peores cromosomas por los K 
;; 				cromosomas que han llegado a la isla , produciendose así la migración.																													   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
										
(defun migracion (viajantes evaluacion posisla)

	(let* ((tam (length (nth 0 *varislas*)))
		(evaluacionDestino (destinoIsla posisla))
		(evaluacionOrigen evaluacion)
		)
		(if (/= viajantes 0)
			(loop for i from 0 to (- viajantes 1) do
				
				(let* ((mejor (first (nth i evaluacionOrigen)))
					   (peor  (first (nth i evaluacionDestino)))
					   )
					(setf (nth posisla *varislas*)(remove mejor (nth posisla *varislas*)))
					(push (cromosomaAleatorio (length mejor)) (nth posisla *varislas*))
					
					(if (/= posisla 3)
						
						(setf (nth (+ i 1) *varislas*)(subst mejor peor (nth (+ i 1) *varislas*)) )
						(setf (nth 0 *varislas*) (subst mejor peor (nth 0 *varislas*)))
					)
				)
			)
		)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Nos devuelve el color de un vertice  a partir de un cromosoma, debemos recordar						 ;;
;;que el contenido de la posicion i-esima dentro del cromosoma será el color correspontiende al vertice i;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun getColor  (posvertice cromosoma)
	(nth posvertice cromosoma)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A esta función le pasaremos la lista de los vecinos de un vertice , y nos devolverá 
;; las posiciones de estos dentro del grafo del problema, ya que el orden en el que 
;; aparecen en el grafo del problema es el mismo orden que en el que aparecen en el 
;; cromosoma y de esta forma capturar de forma fácil los colores.																										;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun getPosiciones (lista)
	(let* ((listaPosiciones '()))
		(loop for i from 0 to (length lista) do
			(loop for j from 0 to (length *grafoProblema*) do
				(if (EQ (nth i lista) (first (nth j *grafoProblema*)))   (push j listaPosiciones) )
			)
		)
		(setf listaPosiciones (cdr listaPosiciones))
		(setf listaPosiciones (reverse listaPosiciones))
		listaPosiciones
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Esta función nos devuelve la lista de vecinos a partir de un vertice 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun getVecinos (posicionVertice)
		(second(nth posicionVertice *grafoProblema* ))
)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A partir de la lista de vecinos de un vértice y un cromosoma, nos devuelve todos 
;; los colores de ese vecino.																					   ;;	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun getColores (listaVecinos cromosoma)

	(let* ((listaColores '())
		   (listaPosiciones (getPosiciones listaVecinos))
		   )
		(loop for i from 0 to (- (length listaPosiciones) 1)  do
			(push  (getColor (nth i listaPosiciones) cromosoma)  listaColores)
		)
	(setf listaColores (reverse listaColores))
	listaColores
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; La siguiente función cálcula el número de vecinos que tienen el mismo color que el 
;; nodo en el que estamos ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun calculaConflicto (colorNodo listaColoresVecinos)
	(let* ((numConflictos 0))
		(loop for i from 0 to (- (length listaColoresVecinos ) 1) do
			(if (= colorNodo (nth i listaColoresVecinos)) (setf numConflictos (+ numConflictos 1)))
		)
		numConflictos
	)
)

(defun pertenece (elem lista)
	(let* ((aux 0))
		(loop for i from 0 to (- (length lista) 1) do
			(if (= (nth i  lista) elem)
				(setf aux 1)
			)
		)
		aux
	)
)

(defun coloresDistintos(cromosoma)
	(let* ((contador 0)
		   (comosomaAux cromosoma)
		   (recorridos '())
		   )
		(loop for i from 0 to  (- (length *genes*) 1) do
			(if (= (pertenece (nth i *genes*) comosomaAux) 1)
				(if (= (pertenece (nth i *genes*) recorridos) 0)
					(and (setf contador (+ contador 1)) (push (nth i *genes*) recorridos))
				)
			)
		)
		contador
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Obtiene el número total de "conflictos" de un cromosoma , como comsideramos los dos 
;; sentidos es decir , si un nodo A está conectado con un nodo B y ambos tienen el 
;; mismo color , se considera el conflicto de A-->B y el conflicto de B-->A .
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun objetivo (cromosoma)
	(let* ((coloresVecinos '())
		   (conflicto 0)
		)
		(loop for i from 0 to ( - (length cromosoma) 1)do 
			(setf conflicto (+ conflicto(calculaConflicto (getColor i cromosoma) (getColores ( second (nth i *grafoProblema*)) cromosoma))))
		)	
		(if (= conflicto 0)
			(setf conflicto ( + (- 0 (length cromosoma)) (coloresDistintos cromosoma)))
		)
		conflicto
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nos ayudaremos de la funcion auxiliar objetivo (Nos devuelve el número de conflictos)
;; para ver  si la isla pasada por parametros tiene solucion . Devuelve la lista de 
;; los conflictos de cada cromosoma de la isla.																																					  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun existeSolucionIsla (isla) 
	(let* (lista '())
		(loop for i from 0 to (- (length isla) 1) do
			(push (objetivo (nth i isla)) lista)
		)
		lista
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Recorrera las 4 islas , y para cada una de ellas usará la función existeSolucionIsla 
;; para ver la lista de conflictos , una vez evaluadas las 4 verá si hay ya algun 
;; cromosoma que no tenga conflictos, es decir si hay solución.																																 ;;					 ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun existeSolucion () 
	(let* ((lista '())
			(aux 0))
		(loop for i from 0 to 3 do
			(setf lista (append  (existeSolucionIsla (nth i *varislas*)) lista))
		)
		(loop for j from 0 to (- (length lista) 1) do
			(if( < (nth j lista) 0)
				(setf aux 1)
			)
		)
		aux
	)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Dada la posición de una isla pasada por parámetro, nos dará la evaluación de esta 
;; mediante una lista donde cada elemento será uno de los cromosomas concatenado con 
;; su puntuación.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun evaluacionIndividuos (posicionIsla)
	(let* ((listaEvaluacionIsla '())
		   (isla (nth posicionIsla *varislas*)))
		(loop for i from 0 to (- (length isla) 1) do
			(push (list (nth i isla) (objetivo (nth i isla))) listaEvaluacionIsla)
		)
		(sort listaEvaluacionIsla #'(lambda (x y)(< (second x) (second y))))
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Funcion que nos indicará el número de individuos que viajaran de una isla a otra 
;; (NUNCA podrá viajar la isla entera)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun numeroDeViajeros ()
	(let* ((numero 0))
		(setf numero (- (length (nth 0 *varislas*)) 1 ))
		(if (/= numero 0)
			(setf numero (random numero))
		)
	numero
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Algoritmos aleatorios para la mutacion aleatoria de los cromosomas
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun crea-gen ()
  (nth (random (length *genes*)) *genes*))

(defun muta-individuo (ind M)
  (loop for gen in ind
     collect (if (< (random 1.0) M)
		 (crea-gen)
		 gen)))


(defun muta (g M)
  (loop for individuo in g
     collect (muta-individuo individuo M)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; * Cruzar una pareja de cromosomas en un punto escogido
;;;   aleatoriamente:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cruza-pareja (individuo1 individuo2)
  (let ((posicion-cruce (+ 1 (random (- (length *genes*) 1)))))
    (list (append (subseq individuo1 0 posicion-cruce)
		  (subseq individuo2 posicion-cruce))
	  (append (subseq individuo2 0 posicion-cruce)
		  (subseq individuo1 posicion-cruce)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; * Cruzar dos a dos los individuos de una población (con un número
;;;   par de individuos):
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun cruza (g)
  (loop for subg on g by #'cddr
     append (cruza-pareja (first subg) (second subg))))	 
	 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;Algoritmo genetico básico para la realización del modelo de islas.  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
	 
(defun algoGenetico (posisla frecuencia m)
	(let* ((evaluacion (evaluacionIndividuos posisla))
		(viajeros (numeroDeViajeros )))
			(if (= (mod (nth posisla *generaciones*) frecuencia) 0)
					(migracion viajeros	evaluacion posisla)
		)
	)
	(setf (nth posisla *varislas*) (muta (nth posisla *varislas*) m))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Nos evalua la población entera , es decir las 4 islas y nos la ordena segun la 
;; puntuacion de cada cromosoma de menor a mayor (cuanto menor valor , menos conflictos) 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun evaluaPoblacion ( ) 
	(let* ( (lista '()) )
		(loop for i from 0 to 3  do
			(setf lista (append (evaluacionIndividuos i)  lista) )
		)
		(sort lista #'(lambda (x y)(< (second x) (second y))))
	)
)


(defun vaciarislas () 
	(loop for i from 0 to 3 do
		(setf (nth i *varislas*) ())
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; El siguiente algoritmo llamado "modeloIslas" sera el algoritmo principal, a partir 
;; del cual pasandole los parametros típicos de un algoritmo genético realizará el 
;; algoritmo genético paralelo "modelo de islas", cuyo funcionamiento es el siguiente:																;;
;;																																								;;
;; La idea básica consiste en dividir la población total en varias subpoblaciones en 
;; cada una de las cuales se lleva a cabo un Algoritmo Genético.
;; Cada cierto número de generaciones, se efectúa un intercambio de información entre 
;; las subpoblaciones, proceso que se denomina emigración.
;; La introducción de la emigración hace que los modelos de islas sean capaces de 
;; explotar las diferencias entre las diversas subpoblaciones, obteniéndose de esta 
;; manera una fuente de diversidad genética. Cada subpopulación es una "isla", 
;; definiéndose un procedimiento por medio del cual se mueve el material genético de
;; una "isla" a otra. La determinación de la tasa de migración, es un asunto de capital 
;; importancia, ya que de ella puede depender la convergencia prematura de la busqueda.																											;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun modeloIslas (tamPoblacion frecuencia m g) 
(vaciarislas)
(setf *poblacionInicial* (generaPoblacionInicial tamPoblacion(length *grafoProblema*)))
(generaIslas *poblacionInicial*)
(generaGenes)
	(let* ((poblacionOrdenada '())
		(existeSolucion2 0))
		(print (length (nth 0 *varislas*)))
		(loop for i from 0 to 3 do
			(loop for j from 0 to g do
				(algoGenetico i frecuencia m)
				(setf (nth i *generaciones*) ( + (nth i *generaciones*) 1))
			)
		)
		
		(setf poblacionOrdenada (evaluaPoblacion))
		(print(first poblacionOrdenada) )
	)
))
