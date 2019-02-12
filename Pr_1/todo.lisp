;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EJERCICIO 1 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; Apartado 1

;;;;;;;;;;;;;;;;; Recursive
(defun norma-rec (x)
  (if (null x)
      0
    (+
     (* (first x) (first x))
     (norma-rec (rest x)))))

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

(defun norma-mapcar (x)
  (apply #'+ (mapcar #'* x x)))

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

(setf lsta '(1 2 3))
(setf lstb '(2 3 4))

(= (cosine-distance-rec lsta lstb) (cosine-distance-mapcar lsta lstb))

(cosine-distance-rec '(1 2) '(1 2 3))
(cosine-distance-rec nil '(1 2 3))
(cosine-distance-rec '() '())
(cosine-distance-rec '(0 0) '(0 0))

;;;;;;;;;; Apartado 2

(defun calcular-confianza (x y)
  (- 1 (cosine-distance-rec x y)))

(defun insertar-ordenado (elemento lista vector)
  (if (null lista)
      (cons elemento nil)
    (if (> (calcular-confianza elemento vector) (calcular-confianza (first lista) vector))
        (cons elemento lista)
      (cons
       (first lista)
       (insertar-ordenado elemento (rest lista) vector)))))

(defun insertar-ordenados (origen destino vector)
  (if (null origen)
      destino
    (insertar-ordenados
     (rest origen)
     (insertar-ordenado (first origen) destino vector)
     vector)))

(defun ordenar-lista (lista vector)
  (insertar-ordenados lista '() vector))

(defun filtrar (lista vector minima-confianza)
  (if (null lista)
      nil
    (if (>= (calcular-confianza (first lista) vector) minima-confianza)
        (cons (first lista) (filtrar (rest lista) vector minima-confianza))
      (filtrar (rest lista) vector minima-confianza))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; order-vectors-cosine-distance
;;; Devuelve aquellos vectores similares a una categoria
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

(order-vectors-cosine-distance '(1 2 3) '((32 454  123) (133 12 1) (4 2 2)) 0.5)
(order-vectors-cosine-distance '(1 2 3) '((32 454  123) (133 12 1) (4 2 2)) 0.3)
(order-vectors-cosine-distance '(1 2 3) '((32 454  123) (133 12 1) (4 2 2)) 0.99)


	
;;;;;;;;;; Apartado 3

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
           
(setf categories '((1 43 23 12) (2 33 54 24)))
(setf texts '((1 3 22 134) (2 43 26 58)))
(get-vectors-category categories texts #'cosine-distance-rec) ;; ---> ((2 0.510181) (1 0.184449))
(get-vectors-category categories texts #'cosine-distance-mapcar) ;; ---> ((2 0.510181) (1 0.184449))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EJERCICIO 2 ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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

(combine-elt-lst 'a '(1 2 3)) ;; --> ((A 1) (A 2) (A 3))

(combine-elt-lst 'a nil)
(combine-elt-lst nil nil)
(combine-elt-lst nil '(a b))

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

(combine-lst-lst '(a b c) '(1 2)) ;; --> ((A 1) (A 2) (B 1) (B 2) (C 1) (C 2))
(combine-lst-lst nil nil)
(combine-lst-lst '(a b c) nil)
(combine-lst-lst nil '(a b c))

;;;;;;;;;; Apartado 3

(defun combine-elt-cons (elt list-of-cons)
  (if (null list-of-cons)
      nil
    (cons
     (cons elt (first list-of-cons))
     (combine-elt-cons elt (rest list-of-cons)))))

(defun combine-lst-with-list-of-cons (list list-of-cons)
  (cond
   ((null list) nil)
   ((null (first list-of-cons))
    (cons (cons (first list) nil)
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
  (if (null (first lstoflsts))
      '(nil)
    (combine-lst-with-list-of-cons (first lstoflsts) (combine-list-of-lsts (rest lstoflsts)))))


(combine-list-of-lsts '((a b c) (+ -) (1 2 3 4)))
;; ((A + 1) (A + 2) (A + 3) (A + 4) (A - 1) (A - 2) (A - 3) (A - 4)
;;  (B + 1) (B + 2) (B + 3) (B + 4) (B - 1) (B - 2) (B - 3) (B - 4)
;;  (C + 1) (C + 2) (C + 3) (C + 4) (C - 1) (C - 2) (C - 3) (C - 4))

(combine-list-of-lsts ’(() (+ -) (1 2 3 4)))
(combine-list-of-lsts ’((a b c) () (1 2 3 4)))
(combine-list-of-lsts ’((a b c) (1 2 3 4) ()))
(combine-list-of-lsts ’((1 2 3 4)))
(combine-list-of-lsts ’(nil))
(combine-list-of-lsts nil)

