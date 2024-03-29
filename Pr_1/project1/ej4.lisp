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
;; y dejar como negaciones de �tomos
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