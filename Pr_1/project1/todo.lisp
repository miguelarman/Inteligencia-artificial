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

(defun get-vectors-category (categories texts distance-measure) 
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


;; Ejercicio 4

(defconstant +bicond+ '<=>)
(defconstant +cond+ '=>)
(defconstant +and+ '^)
(defconstant +or+ 'v)
(defconstant +not+ '!)

;; True si x es True o NIL
(defun truth-value-p (x)
  (or (eql x T) (eql x NIL)))

;; Comprueba que el conector es 1-ario
(defun unary-connector-p (x)
  (eql x +not+))

;; Comprueba que el conector el binario
(defun binary-connector-p (x)
  (or (eql x +bicond+)
      (eql x +cond+)))


;; Comprueba que el conector es n-ario
(defun n-ary-connector-p (x)
  (or (eql x +and+)
      (eql x +or+)))

;; Comprueba si el conector es <=>
(defun bicond-connector-p (x)
  (eql x +bicond+))

;; Comprueba si el conector es =>
(defun cond-connector-p (x)
  (eql x +cond+))

;; Comprueba si es un conector
(defun connector-p (x)
  (or (unary-connector-p x)
      (binary-connector-p x)
      (n-ary-connector-p x)))

;; Comprueba si es un atomo
(defun positive-literal-p (x)
  (and (atom x)
       (not (truth-value-p x))
       (not (connector-p x))))

;; Comprueba si es un not
(defun negative-literal-p (x)
  (and (listp x)
       (eql +not+ (first x))
       (null (rest (rest x)))
       (positive-literal-p (second x))))

;; Comprueba si es un atomo o un not
(defun literal-p (x)
  (or (positive-literal-p x)
      (negative-literal-p x)))

;; Comprueba si la lista es un and unicamente
(defun only-and-list (x)
  (and (eql +and+ (first x))
       (null (rest x))))

;; Apartado 1
  
        

(defun par-vacio (par)
  (and
   (null (first par))
   (null (first (rest par)))))

(defun lista-pares-vacia (lista)
  (and
   (par-vacio (first lista))
   (null (rest lista))))
   
   
(defun combinar-listas-or (pares1 pares2)
  (cond
   ((lista-pares-vacia pares1) pares2)
   ((lista-pares-vacia pares2) pares1)
   (T (append pares1 pares2))))





(defun combinar-par-par-aux (par1 par2)
  (let
      ((atomos-positivos (append (first par1) (first par2)))
       (atomos-negativos (append (first (rest par1))
                                 (first (rest par2)))))
    (cons atomos-positivos (cons atomos-negativos nil))))

(defun combinar-par-lista-aux (par pares)
  (if (null pares)
      nil
    (cons (combinar-par-par-aux par (first pares))
          (combinar-par-lista-aux par (rest pares)))))
  
(defun combinar-listas-and (pares1 pares2)
  (if (null pares1)
      nil
    (append (combinar-par-lista-aux (first pares1) pares2)
            (combinar-listas-and (rest pares1) pares2))))




(defun lista-de-atomo (atomo)
  (cond
   ((positive-literal-p atomo) (cons (cons
                                      (cons atomo nil)
                                      (cons '() nil))
                                     nil))
   ((negative-literal-p atomo) (cons (cons nil
                                           (cons (cons (first (rest atomo)) nil) nil))
                                     nil))
   (T 'err)))

;; Funcion que expande una expresion con formato correcto, y devuelve todas
;; las hojas del arbol de verdad que representa a la expresion
(defun expand-truth-tree-aux (expresion)
  (cond
   ((only-and-list expresion) '((nil nil)))
   
   ; la expresion es +and+ y atomo, devuelve el atomo
   ((and (positive-literal-p (first (rest expresion)))
         (null (rest (rest expresion))))
    (lista-de-atomo (first (rest expresion))))
     
   ; la expresion es un +and+ con varios elementos
   ((and (not (null (rest (rest expresion)))))
    (combinar-listas-and (expand-truth-tree-aux (cons +and+
                                                      (cons (first (rest expresion)) nil)))
                         (expand-truth-tree-aux (cons +and+
                                                      (rest (rest expresion))))))
   
   ; la expresion es un +and+ con un solo elemento
   
   ; el elemento empieza por otro +and+
   ((and (eql +and+ (first expresion))
         (null (rest (rest expresion)))
         (eql +and+ (first (first (rest expresion)))))
    (expand-truth-tree-aux (first (rest expresion))))
   
   ; el elemento empieza por un +or+
   
   ; Caso base (^ (V))
   ((and (eql +and+ (first expresion))
         (null (rest (rest expresion)))
         (null (rest (first (rest expresion)))) ; Nueva linea
         (eql +or+ (first (first (rest expresion)))))
    '((nil nil)))
   
   ((and (eql +and+ (first expresion))
         (null (rest (rest expresion)))
         (eql +or+ (first (first (rest expresion)))))
    (combinar-listas-or
     (expand-truth-tree-aux (cons +and+
                                  (cons
                                   (first (rest
                                           (first (rest expresion))))
                                   nil)))
     (expand-truth-tree-aux (cons +and+
                                  (cons
                                   (cons +or+
                                         (rest (rest
                                                (first (rest expresion)))))
                                   nil)))))
   ; el elemento en un literal negado
   ((and (null (rest (rest expresion)))
         (negative-literal-p (first (rest expresion))))
    (lista-de-atomo (first (rest expresion))))
   
   ;; Si se llega a este caso, ha habido un error, por lo que
   ;; devuelve un par no satisfacible
   (T '(((A) (A))))))

;; Apartado 2

(defun lista-contiene-elemento (elt lista)
  (cond
   ((null lista) nil)
   ((eql elt (first lista)) T)
   (T (lista-contiene-elemento elt (rest lista)))))


(defun interseccion-listas-vacia (lista1 lista2)
  (cond
   ((null lista1) T)
   ((lista-contiene-elemento (first lista1) lista2) nil)
   (T (interseccion-listas-vacia (rest lista1) lista2))))


(defun par-satisfacible (par) 
  (cond
   ((null par) nil)
   ((par-vacio par) T)
   (T (interseccion-listas-vacia (first par) (first (rest par))))))


(defun lista-satisfacible (lista)
  (cond
   ((null lista) T)
   ((par-satisfacible (first lista)) (lista-satisfacible (rest lista)))
   (T nil)))

;;Evalua una funcion en  una lista de sentencias
(defun funcion-lista (lista funcion)
  (cond
   ((null lista) NIL)
   (T (cons 
       (funcall funcion (first lista))
       (funcion-lista (rest lista) funcion)))))
        
;;Permite que todas las negaciones sean atomicas
(defun negar (expresion)
  (cond
   ((positive-literal-p expresion)
    (cons '!
          (cons expresion nil)))
   ((unary-connector-p (first expresion)) (first (rest expresion)))
   ((eql +and+ (first expresion))
    (cons +or+ 
          (funcion-lista (rest expresion) #'negar)))
   ((eql +or+ (first expresion))
    (cons +and+ 
          (funcion-lista (rest expresion) #'negar)))
   (T
    (cons '!
          (cons expresion nil)))))

;; Desarrolla los =>
(defun desarrollar-cond (expresion)
  (cons '^
        (cons (cons 'v
                    (cons (negar (sintetizar-cond (first (rest expresion))))
                          (cons (sintetizar-cond (first (rest (rest expresion)))) nil)))
              nil)))

;; Desarrolla los <=>
(defun desarrollar-bicond (expresion)
  (cons +and+ (cons
        (let ((a (sintetizar-bicond (first (rest expresion))))
              (b (sintetizar-bicond (first (rest (rest expresion))))))
          (cons +or+
                (cons 
                 (cons +and+
                       (cons a (cons b nil)))
                 (cons(cons +and+
                            (cons (negar a) (cons (negar b) nil)))nil)))) nil)))

;; Explora la expresion y encuentra los bicondicionales a desarrollar
(defun sintetizar-bicond (expresion)
  (cond
   ((or (positive-literal-p expresion) (negative-literal-p expresion)) expresion)
   ((bicond-connector-p (first expresion)) (desarrollar-bicond expresion))
   (T
    (cons 
     (first expresion) 
     (funcion-lista (rest expresion) #'sintetizar-bicond)))))

;; Explora la expresion y encuentra los condicionales a desarrollar
(defun sintetizar-cond (expresion)
  (cond
   ((or (positive-literal-p expresion) (negative-literal-p expresion))
    expresion)
   ((cond-connector-p (first expresion)) (desarrollar-cond expresion))
   (T
    (cons 
     (first expresion) 
     (funcion-lista (rest expresion) #'sintetizar-cond)))))

;; Explora la expresion y encuentra las negaciones a desarrollar,
;; y dejar como negaciones de átomos
(defun sintetizar-negar (expresion)
  (cond
   ((or (positive-literal-p expresion) (negative-literal-p expresion))
    expresion)
   ((eql +not+ (first expresion)) (negar (first (rest expresion))))
   (T (cons (first expresion)
            (funcion-lista (rest expresion) #'sintetizar-negar)))))
  

;; Simplifica la expresion a and, or y negaciones atomicas
(defun sintetizar (expresion)
  (cons +and+
        (cons (sintetizar-negar
               (sintetizar-cond
                (sintetizar-bicond expresion)))
              nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRUTH-TREE
;; Recibe una expresion y construye su arbol de verdad para comprobar
;; si es SAT o UNSAT
;; PARAMS : exp - expresion a analizar .
;; RETURN : t - la expresion es SAT
;;          nil - la expresion es UNSAT

(defun truth-tree (expresion)
  (lista-satisfacible
   (expand-truth-tree-aux
    (sintetizar expresion))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Breadth-first-search in graphs
;;;
(defun bfs (end queue net)
  (if (null queue) '()
    (let* ((path (first queue))
           (node (first path)))
      (if (eql node end)
          (reverse path)
        (bfs end
             (append (rest queue)
                     (new-paths path node net))
             net)))))

(defun new-paths (path node net)
  (mapcar #'(lambda (n)
              (cons n path))
    (rest (assoc node net))))
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun shortest-path (start end net)
  (bfs end (list (list start)) net))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lista-contiene (lista elt)
  (if (null (find elt lista))
      nil
    T))

(defun bfs-improved-aux (end queue net visitados)
  (if (null queue) '()
    (let* ((path (first queue))
           (node (first path)))
      (if (eql node end)
          (reverse path)
        (if (lista-contiene visitados node)
            (bfs-improved-aux end
                              (rest queue)
                              net
                              visitados)
          (bfs-improved-aux end
               (append (rest queue)
                       (new-paths path node net))
               net
               (cons node visitados)))))))

  
  
  
  
  
(defun bfs-improved (end queue net)
  (bfs-improved-aux end queue net nil))


(defun shortest-path-improved (end queue net)
  (bfs-improved end (list (list start)) net))

