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
  (and (eql (first x) +and+)
       (null (rest x))))

;; Apartado 1

(defun negar (expresion)
  (cond
   ((positive-literal-p expresion)
    (cons '!
          (cons expresion nil)))
   ((unary-connector-p (first expresion)) (first (rest expresion)))
   (T (cons '!
            (cons expresion nil)))))

(defun desarrollar-cond (expresion)
  (cons '^
        (cons (cons 'v
                    (cons (negar (first (rest expresion)))
                          (cons (first (rest (rest expresion))) nil)))
              nil)))

(defun desarrollar-bicond (expresion)
  (cons '^
        (cons (cons '=>
                    (cons (first (rest expresion))
                          (cons (first (rest (rest expresion)))
                                nil)))
              (cons (cons '=>
                          (cons (first (rest (rest expresion)))
                          (cons (first (rest expresion))
                                nil)))nil))))

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

(defun expand-truth-tree-aux (nodos expresion)
  (cond
   ((null expresion) NIL)
   ((only-and-list expresion) nodos)
   (T (cond
       ((cond-connector-p (first expresion)) (expand-truth-tree-aux nodos (desarrollar-cond expresion)))
       ((bicond-connector-p (first expresion)) (expand-truth-tree-aux nodos (desarrollar-bicond expresion)))
       (nil)))))
        
        
        
        
        
        
        
        
        
        

;; Apartado 2

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TRUTH-TREE
;; Recibe una expresion y construye su arbol de verdad para comprobar
;; si es SAT o UNSAT
;; PARAMS : exp - expresion a analizar .
;; RETURN : t - la expresion es SAT
;;          nil - la expresion es UNSAT


  