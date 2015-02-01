(defparameter *colores* '())
(defparameter *grafoProblema* '((CA (SE HU MA)) (HU (SE CA)) (SE (CA HU CO MA)) (CO (SE MA GR JA)) (MA (CA SE CO GR)) (JA (CO GR)) (GR (MA CO JA AL)) (AL (GR))))

(defparameter *poblacionInicial* '())



(defun generaColores () 
	(loop for i from 0 to (- (length *grafoProblema*) 1)do
		(push i *colores*)
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;										
;; Nos ayuda a generar la solucion Inicial de forma aleatoria 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;										

(defun inicioAleatorio (tam) 
	(let* (cromosoma '())
		(loop for i from 0 to (- tam 1) do
			  (push (random tam) cromosoma))
		cromosoma
	)
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;										
;;A esta función le pasaremos la lista de los vecinos de un vertice , y nos devolverá 
;;las posiciones de estos dentro del grafo del problema, ya que el orden en el que 
;;aparecen en el grafo del problema es el mismo orden que en el que aparecen en el 
;;cromosoma y de esta forma	capturar de forma fácil los colores.																										;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;										

(defun getPosiciones (lista)
	(let* ((listaPosiciones '()))
		(loop for i from 0 to (length lista) do
			(loop for j from 0 to (length *grafoProblema*) do
				(if (EQ (nth i lista) (first (nth j *grafoProblema*))) (push j listaPosiciones) )
			)
		)
		(setf listaPosiciones (cdr listaPosiciones))
		(setf listaPosiciones (reverse listaPosiciones))
		listaPosiciones
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;										
;;Nos devuelve el color de un vertice  a partir de un cromosoma, debemos recordar
;;que el contenido de la posicion i-esima dentro del cromosoma será el color 
;;correspontiende al vertice i
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;										
(defun getColor  (posvertice cromosoma)

	(nth posvertice cromosoma)

)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;										
;; A partir de la lista de vecinos de un vértice y un cromosoma, nos devuelve todos 
;; los colores de ese vecino.
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
;; nodo en el que estamos 
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
			(generaColores)
			(loop for i from 0 to  (- (length *colores*) 1) do
				(if (= (pertenece (nth i *colores*) comosomaAux) 1)
						(if (= (pertenece (nth i *colores*) recorridos) 0)
							(and (setf contador (+ contador 1)) (push (nth i *colores*) recorridos))
						)
				)
			)
			contador
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;										
;; Obtiene el número total de "conflictos" de un cromosoma , como comsideramos los dos 
;; sentidos es decir , si un nodo A está conectado con un nodo B y ambos tienen el mismo 
;; color , se considera el conflicto de A-->B y el conflicto de B-->A .														;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;										

(defun objetivo (cromosoma)
	(let* ((coloresVecinos '())
		   (conflicto 0)
			)
		(loop for i from 0 to ( - (length cromosoma) 1)do 
			(setf conflicto (+ conflicto(calculaConflicto (getColor i cromosoma) (getColores ( second (nth i *grafoProblema*)) cromosoma))))
		)
		(if (= conflicto 0) 
			(setf conflicto (+ (- 0 (length cromosoma)) (coloresDistintos cromosoma)))
		 )
		(list cromosoma conflicto)
	)
)

(defun movimiento (lista posAEliminar nuevoElem)
	(let* ((principio (butlast lista (- (length lista) posAEliminar)))
		   (fin (subseq lista (+ posAEliminar 1) (length lista)))
		   (nuevo (append principio (append (list nuevoElem)  fin) ))
		)
		(objetivo nuevo)
	)
)

(defun vecindario (actual)
	(let* ((movimientosPosibles '())
		   (vecinos '())
		 )
		  
		(loop for i from 0 to ( - (length *grafoProblema*) 1) do
			(push i movimientosPosibles)
		)
		(setf movimientosPosibles (reverse movimientosPosibles))
	  
	  
		(loop for i from 0 to  (-(length (first actual)) 1) do
			(let* ((movimientos (remove (nth i ( first actual)) movimientosPosibles))
				)
				(loop for j from 0 to (- (length movimientos) 1) do
					(let* ((nuevaSolucion (movimiento (first actual) i (nth j movimientos)))
						  )
								( if ( <(second nuevaSolucion) (second actual) )
									(push nuevaSolucion vecinos)
								)
					)
				)
			)
		)
		vecinos
	)
)

(defun aspirantesFuncion (actual listaTabu )
	(let* ( (aspirantes '())
		  ) 
		(loop for i from 0 to (- (length listaTabu) 1) do
			(if (< (second (objetivo (nth i listaTabu))) (second (objetivo actual)))
				(push  (nth i listaTabu)  aspirantes)
			)
		)
		aspirantes
	)
)

(defun mejor(conjuntoAspirantes)
	(let* ( (conjuntoOrdenado '())
		  )
		(setf conjuntoOrdenado (sort conjuntoAspirantes #'(lambda (x y)(< (second x) (second y)))))
		(first conjuntoOrdenado)
	)
)

(defun dameConjunto (aspirantes listaTabu vecinos)
	(let* ((aux '()))
		(setf aux vecinos)
		(loop for i from 0 to (length listaTabu)  do
			(setf aux (remove (nth i listaTabu) aux))
		)
		(setf aux (append aux aspirantes))
		aux
	)
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ALGORTIRMO BUSQUEDA TABU
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;										

(defun busquedaTabu (iter)
	(let* (
		(solucionActual (objetivo (inicioAleatorio (length *grafoProblema*))))
			(iteraciones 1)
			(salida 0)
		)
		(loop while  (and (= salida 0)(/= iteraciones iter) )  do
			(print solucionActual)
			
			(let* ((vecinos '())
				   (listaTabu '())
				   (aspirantes '())
				   (conjuntoAspirantes '())
				   (mejorAspirante '())
				   (viejaSolucion '())
				   )
				;; Identificamos Vecinos
				(setf vecinos (vecindario solucionActual))
				;;Identificamos Aspirantes
				(setf aspirantes (aspirantesFuncion solucionActual listaTabu))
				;;Definimos los posibles candidatos totales
				(setf conjuntoAspirantes (dameConjunto aspirantes listaTabu vecinos))
				;Tomamos el mejor aspirante
				(setf mejorAspirante (mejor conjuntoAspirantes))
				(setf viejaSolucion solucionActual)		
				;(print (< (second mejorAspirante) (second solucionActual)))
				( if (/= (length vecinos) 0)
					(if(< (second mejorAspirante) (second solucionActual))
						(setf solucionActual mejorAspirante)
					)
					(setf salida 1)
				)		  
				(push viejaSolucion listaTabu)
				(setf iteraciones (+ iteraciones 1))
			)
		)
		(print "La solucion mas optima encontrada es la siguiente: ")
		solucionActual
	)
)