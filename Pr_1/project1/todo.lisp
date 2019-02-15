;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EJERCICIO 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; Apartado 1

;;;;;;;;;;;;;;;;; Recursive

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; norma-rec
;;; 
;;; Calcula la norma de un vector de forma recursiva
;;;
;;; INPUT:  x: vector, representado como una lista
;;; OUTPUT: norma de x
;;;

(defun norma-rec (x)
  (if (null x)
      0
    (+
     (* (first x) (first x))
     (norma-rec (rest x)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; producto-escalar-rec
;;; 
;;; Calcula el producto escalar de dos vectores de forma recursiva
;;; Se asume que los dos vectores de entrada tienen la misma longitud.
;;;
;;; INPUT:  x: vector, representado como una lista
;;;         y: vector, representado como una lista
;;; OUTPUT: producto escalar de x e y
;;;

(defun producto-escalar-rec (x y)
  (cond
   ((null x) 0)
   ((null y) 0)
   (T (+ (* (first x) (first y))
         (producto-escalar-rec (rest x) (rest y))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cosine-distance-rec (x y)
;;; Calcula la distancia coseno de un vector de forma recursiva
;;; Se asume que los dos vectores de entrada tienen la misma longitud.
;;;
;;; INPUT: x: vector, representado como una lista
;;;         y: vector, representado como una lista
;;; OUTPUT: distancia coseno entre x e y
;;;

(defun cosine-distance-rec (x y)
  (let
      ((nx (norma-rec x))
       (ny (norma-rec y)))
    (cond
     ((and (= nx 0) (= ny 0)) 0)
     ((= nx 0) 1)
     ((= ny 0) 1)
     (T (- 1
           (/ (producto-escalar-rec x y)
              (* (sqrt nx)
                 (sqrt ny))))))))

;;;;;;;;;; Mapcar

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; norma-mapcar
;;; 
;;; Calcula la norma de un vector usando mapcar
;;;
;;; INPUT:  x: vector, representado como una lista
;;; OUTPUT: norma de x
;;;

(defun norma-mapcar (x)
  (apply #'+ (mapcar #'* x x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; producto-escalar-mapcar
;;; 
;;; Calcula el producto escalar de dos vectores usando mapcar
;;; Se asume que los dos vectores de entrada tienen la misma longitud.
;;;
;;; INPUT:  x: vector, representado como una lista
;;;         y: vector, representado como una lista
;;; OUTPUT: producto escalar de x e y
;;;

(defun producto-escalar-mapcar (x y)
  (cond
   ((null x) 0)
   ((null y) 0)
   (T (apply #'+ (mapcar #'* x y)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; cosine-distance-mapcar
;;; Calcula la distancia coseno de un vector usando mapcar
;;; Se asume que los dos vectores de entrada tienen la misma longitud.
;;;
;;; INPUT:  x: vector, representado como una lista
;;;         y: vector, representado como una lista
;;; OUTPUT: distancia coseno entre x e y
;;;

(defun cosine-distance-mapcar (x y)
  (let
      ((nx (norma-mapcar x))
       (ny (norma-mapcar y)))
    (cond
     ((and (= nx 0) (= ny 0)) 0)
     ((= nx 0) 1)
     ((= ny 0) 1)
     (T (- 1
           (/ (producto-escalar-mapcar x y)
              (* (sqrt nx) (sqrt ny))))))))


;;;;;;;;;; Apartado 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; calcular-confianza
;;;
;;; Calcula la confianza de dos vectores
;;;
;;; INPUT:  x: primer vector
;;;         y: segundo vector
;;; OUTPUT: Valor de la confianza entre ambos vectores
;;;

(defun calcular-confianza (x y)
  (- 1 (cosine-distance-rec x y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; insertar-ordenado
;;;
;;; Funcion recursiva que inserta un elemento en una lista, ordenado
;;; por su confianza con un vector
;;;
;;; INPUT:  elemento: elemento a insertar
;;;         lista: lista donde se quiere insertar el elemento
;;;         vector: vector con el que se comparan
;;; OUTPUT: Lista con el elemento extra ordenados con respecto a
;;; su semejanza a una categoria
;;;

(defun insertar-ordenado (elemento lista vector)
  (if (null lista)
      (cons elemento nil)
    (if (> (calcular-confianza elemento vector) (calcular-confianza (first lista) vector))
        (cons elemento lista)
      (cons
       (first lista)
       (insertar-ordenado elemento (rest lista) vector)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; insertar-ordenados
;;;
;;; Inserta los elementos de una lista en otra, ordenados
;;; por su confianza con un vector
;;;
;;; INPUT:  origen: lista de origen
;;;         destino: lista de destino
;;;         vector: vector con el que se comparan
;;; OUTPUT: Vectores ordenados por su semejanza con respecto a
;;; una categoria
;;;

(defun insertar-ordenados (origen destino vector)
  (if (null origen)
      destino
    (insertar-ordenados
     (rest origen)
     (insertar-ordenado (first origen) destino vector)
     vector)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ordenar-lista
;;;
;;; Ordena una lista respecto a la confianza de cada elemento con respecto
;;; a un vector
;;;
;;; INPUT:  lista: vector de vectores
;;;         vector: vector con el que se compara cada elemento
;;; OUTPUT: Vectores ordenados por su semejanza con respecto a
;;; una categoria
;;;

(defun ordenar-lista (lista vector)
  (insertar-ordenados lista '() vector))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; filtrar
;;;
;;; Filtra de una lista aquellos elementos que tengan una confianza
;;; respecto a un vector menor a un límite inferior
;;;
;;; INPUT:  vector: vector que representa a una categoria,
;;;                 representado como una lista
;;;         lista: vector de vectores
;;;         confidence-level: Nivel de confianza minimo
;;; OUTPUT: Vectores cuya semejanza con respecto a la
;;;         categoria es superior al nivel de confianza ,
;;;

(defun filtrar (lista vector minima-confianza)
  (if (null lista)
      nil
    (if (>= (calcular-confianza (first lista) vector) minima-confianza)
        (cons (first lista) (filtrar (rest lista) vector minima-confianza))
      (filtrar (rest lista) vector minima-confianza))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; order-vectors-cosine-distance
;;;
;;; Devuelve aquellos vectores similares a una categoria
;;;
;;; INPUT:  vector: vector que representa a una categoria,
;;;                 representado como una lista
;;;         lst-of-vectors vector de vectores
;;;         confidence-level: Nivel de confianza (parametro opcional)
;;; OUTPUT: Vectores cuya semejanza con respecto a la
;;;         categoria es superior al nivel de confianza ,
;;;         ordenados
;;;

(defun order-vectors-cosine-distance (vector lst-of-vectors &optional (confidence-level 0))
  (ordenar-lista (filtrar lst-of-vectors vector confidence-level) vector))



	
;;;;;;;;;; Apartado 3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; best-category
;;; 
;;; Funcion recursiva que para una lista de categorias y un texto
;;; devuelve un par formado por el identificador de la categoria
;;; con menor distancia y la respectiva distancia
;;;
;;; INPUT : categories: vector de vectores, representado como
;;;                     una lista de listas
;;;         text:       vectores, que representa un texto
;;;         distance-measure: funcion de distancia
;;; OUTPUT: Par formado por el vector que identifica la categoria
;;;         de menor distancia, junto con el valor de dicha distancia
;;;

(defun best-category (categories text distance-measure)
  (if (null categories)
      nil
    (let
        ((min-parcial (best-category (rest categories) text distance-measure))
         (nuevo-min (cons (first (first categories))
                          (cons (funcall distance-measure (rest (first categories)) (rest text)) nil))))
      (cond
       ((null min-parcial) nuevo-min)
       ((< (first (rest nuevo-min)) (first (rest min-parcial))) nuevo-min)
       (T min-parcial)))))
    
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; get-vectors-category (categories vectors distance-measure)
;;; Clasifica a los textos en categorias .
;;;
;;; INPUT : categories: vector de vectores, representado como
;;;                     una lista de listas
;;;         texts:      vector de vectores, representado como
;;;                     una lista de listas
;;;         distance-measure: funcion de distancia
;;; OUTPUT: Pares formados por el vector que identifica la categoria
;;;         de menor distancia , junto con el valor de dicha distancia
;;;

(defun get-vectors-category (categories texts distance-measure) ;;;;;;;;; FALTA ESTO
  (if (null texts)
      nil
    (cons
     (best-category categories (first texts) distance-measure)
     (get-vectors-category categories (rest texts) distance-measure))))
     
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EJERCICIO 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;Apartado 1


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; iteracion
;;; Calcula el siguiente punto iterando el punto anterior
;;; segun el metodo de Newton
;;;
;;; INPUT : f: funcion cuyo cero se desea encontrar
;;; df: derivada de f
;;; x0: estimacion del cero
;;; OUTPUT : siguiente paso de la iteracion, NIL si
;;;          el punto es suficientemente grande como
;;;          para concluir que no converge
;;;
(defun iteracion (f df x0)
  (if (> x0 100000)
      NIL
    (- x0 (/ (funcall f x0) (funcall df x0)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; ultima-iteracion
;;; Realiza la ultima iteracion por Newton, esta vez
;;; teniendo en cuenta si el punto cumple la condicion de
;;; tolerancia
;;;
;;; INPUT : f: funcion cuyo cero se desea encontrar
;;; df: derivada de f
;;; penult: ultima estimacion antes de la estimacion final
;;; tol: tolerancia para convergencia ( parametro opcional )
;;; OUTPUT : estimacion del cero de f, o NIL si no converge
;;;
(defun ultima-iteracion (f df penult tol)
  (let ((ultimo (iteracion f df penult)))
    (cond
     ((null ultimo) NIL)
     ((< (abs (- penult ultimo)) tol) ultimo)
     (T NIL))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; newton-rec
;;; Funcion recursiva utilizada para hacer la funcion newton,
;;; donde calculamos el punto sin tener en cuenta la tolerancia
;;;
;;; INPUT : f: funcion cuyo cero se desea encontrar
;;; df: derivada de f
;;; max-iter : iteraciones restantes hasta el caso base
;;; x0: estimacion del cero
;;; OUTPUT : estimacion del cero de f, o NIL si no converge
;;;

(defun newton-rec (f df max-iter x0)
  (if (> max-iter 0)
      (iteracion f df (newton-rec f df (- max-iter 1) x0))
    (iteracion f df x0)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; newton
;;; Estima el cero de una funcion mediante Newton-Raphson
;;;
;;; INPUT : f: funcion cuyo cero se desea encontrar
;;; df: derivada de f
;;; max-iter : maximo numero de iteraciones
;;; x0: estimacion inicial del cero ( semilla )
;;; tol: tolerancia para convergencia ( parametro opcional )
;;; OUTPUT : estimacion del cero de f, o NIL si no converge
;;;

(defun newton (f df max-iter x0 &optional (tol 0.001)) 
  (cond ((<= tol 0) NIL)
        ((< max-iter 0) NIL)
        ((= max-iter 0) x0)
        (T 
         (ultima-iteracion f df (newton-rec f df (- max-iter 1) x0) tol))))



;;;;;;;;;;;;;;;;;;;;;;Apartado 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; one-root-newton
;;; Prueba con distintas semillas iniciales hasta que Newton
;;; converge
;;;
;;; INPUT : f: funcion de la que se desea encontrar un cero
;;; df: derivada de f
;;; max-iter : maximo numero de iteraciones
;;; semillas : semillas con las que invocar a Newton
;;; tol: tolerancia para convergencia ( parametro opcional )
;;;
;;; OUTPUT : el primer cero de f que se encuentre , o NIL si se diverge
;;; para todas las semillas
;;;

(defun one-root-newton (f df max-iter semillas &optional (tol 0.001))
  (if (null semillas)
      NIL
    (let ((nx (newton f df max-iter (first semillas) tol)))
      (if (null nx)
           (one-root-newton f df max-iter (rest semillas) tol)
         nx))))

;;;;;;;;;;;;;;;;;;;;;;Apartado 3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; all-roots-newton
;;; Prueba con distintas semillas iniciales y devuelve las raices
;;; encontradas por Newton para dichas semillas
;;;
;;; INPUT : f: funcion de la que se desea encontrar un cero
;;; df: derivada de f
;;; max-iter : maximo numero de iteraciones
;;; semillas : semillas con las que invocar a Newton
;;; tol: tolerancia para convergencia ( parametro opcional )
;;;
;;; OUTPUT : las raices que se encuentren para cada semilla o nil
;;; si para esa semilla el metodo no converge
;;;

(defun all-roots-newton (f df max-iter semillas &optional (tol 0.001))
  (if (null semillas)
      NIL
    (cons (newton f df max-iter (first semillas) tol) 
          (all-roots-newton f df max-iter (rest semillas) tol))))

;;;;;;;;;;;;;;;;;;;;;;;;Apartado 3.1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; list-not-nil-roots-newton
;;; Prueba con distintas semillas iniciales y devuelve solo las
;;; raices que converjan (no devuelve ningun NIL)
;;;
;;; INPUT : f: funcion de la que se desea encontrar un cero
;;; df: derivada de f
;;; max-iter : maximo numero de iteraciones
;;; semillas : semillas con las que invocar a Newton
;;; tol: tolerancia para convergencia ( parametro opcional )
;;;
;;; OUTPUT : las raices que se encuentren para cada semilla (si existe)
;;;

(defun list-not-nil-roots-newton (f df max-iter semillas &optional (tol 0.001))
  (mapcan (lambda (x) (if (null x) NIL (list x))) 
          (all-roots-newton f df max-iter semillas tol)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EJERCICIO 3 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;; Apartado 1

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-elt-lst
;;; Combina un elemento dado con todos los elementos de una lista
;;;
;;; INPUT: elem: elemento a combinar
;;;        lst: lista con la que se quiere combinar el elemento
;;;
;;; OUTPUT: lista con las combinacion del elemento con cada uno de los
;;;         de la lista

(defun combine-elt-lst (elt lst)
  (cond
   ((and (null elt) (null lst)) nil)
   ;((null lst) (cons elt nil))
   ((null lst) nil)
   ((null elt) (cons
                (cons (first lst) nil)
                (combine-elt-lst elt (rest lst))))
   (T (cons
       (cons elt (cons (first lst) nil))
       (combine-elt-lst elt (rest lst))))))



;;;;;;; Apartado 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-lst-lst
;;; Calcula el producto cartesiano de dos listas
;;;
;;; INPUT: lst1: primera lista
;;;        lst2: segunda lista
;;;
;;; OUTPUT: producto cartesiano de las dos listas

(defun combine-lst-lst (lst1 lst2)
  (cond
   ((and (null lst1) (null lst2)) nil)
   ((null lst1) nil)
   (T (append (combine-elt-lst (first lst1) lst2)
              (combine-lst-lst (rest lst1) lst2)))))



;;;;;;;;;; Apartado 3

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-elt-cons
;;; Funcion recursiva que junta un elemento
;;; con otra lista formada por cons
;;;
;;; INPUT: elt: elemento a juntar
;;;        list-of-cons: lista de cons
;;;
;;; OUTPUT: lista con todas las posibles combinaciones de elementos

(defun combine-elt-cons (elt list-of-cons)
  (if (null list-of-cons)
      nil
    (cons
     (cons elt (first list-of-cons))
     (combine-elt-cons elt (rest list-of-cons)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-lst-with-list-of-cons
;;; Funcion recursiva que junta un lista de atomos
;;; con otra lista formada por cons
;;;
;;; INPUT: lst: lista de atomos
;;;        list-of-cons: lista de cons
;;;
;;; OUTPUT: lista con todas las posibles combinaciones de elementos

(defun combine-lst-with-list-of-cons (list list-of-cons)
  (cond
   ((null list) nil)
   ((null (first list-of-cons)) (cons (cons (first list) nil)
                                      (combine-lst-with-list-of-cons (rest list) list-of-cons)))
   (T (append
       (combine-elt-cons (first list) list-of-cons)
       (combine-lst-with-list-of-cons (rest list) list-of-cons)))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; combine-list-of-lsts
;;; Calcula todas las posibles disposiciones de elementos
;;; pertenecientes a N listas de forma que en cada disposicion 
;;; aparezca unicamente un elemento de cada lista
;;;
;;; INPUT: lstolsts: lista de listas
;;;
;;; OUTPUT: lista con todas las posibles combinaciones de elementos

(defun combine-list-of-lsts (lstoflsts)
  (cond
   ((null lstoflsts) nil)
   ((null (first lstoflsts)) '(nil))
   (T (combine-lst-with-list-of-cons (first lstoflsts) (combine-list-of-lsts (rest lstoflsts))))))




