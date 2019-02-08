;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; EJEMPLO 1
;;;
;;; Expresiones y evaluaciones (pag. 1-2)
;;;
;;;	En este código encontrarás una función
;;;	adicional list que forma una lista con los
;;;	elementos que se le pasan. Explica los
;;;	resultados obtenidos, con especial hincapié
;;;	en las sentencias con un doble eval
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter a (+ 2 3))
(eval a)

(defparameter b '(+ 2 3))
(eval b)

(defparameter c (quote (+ 2 3)))
(eval c)

(defparameter d (list 'quote '(+ 2 3)))
(eval d)
(eval (eval d))

(defparameter d (list 'quote (+ 2 3)))
(eval d)
(eval (eval d))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; EJEMPLO 2
;;;
;;; Predicados (pag. 2)
;;;
;;;	Comenta brevemente los resultados obtenidos
;;;	de evaluar las siguientes expresiones
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(atom 12)
(atom 'abc)
(atom a)
(atom '(a b))
(atom '())

(null '())
(null nil)

(symbolp 12)
(symbolp 'abc)
(symbolp a)
(symbolp nil)

(numberp 123)
(numberp 'a)
(defparameter a 3)
(numberp a)

(listp '(a b))
(listp '())
(listp nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; EJEMPLO 3
;;;
;;; Estructura y funciones de listas (pag. 2-3)
;;;
;;;	Ejecuta y comprende el siguiente código y
;;;	plantea el pseudocódigo de una implementación
;;;	propia de 'length' que utilice recursión sobre
;;;	la lista pasada. Llama a esta función
;;;	'my-length'
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter milista nil)
(defparameter milista '(- 6 2))
(defparameter milista (- 6 2))
(defparameter milista '(8 (9 10) 11))
(car milista)
(first milista)
(cdr milista)
(rest milista)
(car (cdr milista))
(length milista)
(defparameter milista (cons 4 '(3 2)))
(defparameter milista (list '+ 4 3 2))
(eval milista)
(defparameter milista (append '(4 3) '(2 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; EJEMPLO 4
;;;
;;; Operadores lógicos y condicionales (pag. 3-4)
;;;
;;;	Evalua el siguiente código y asegurate de
;;;	entender por qué cada caso devuelve lo que
;;;	devuelve. Prueba a modificar el valor de nota
;;;	para que el cond devuelva los otros casos.
;;;	Prueba también a modificar el cond para que
;;;	devuelva nil
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(if t 1 2)
(if nil 1 2)
(when t 1 2 3)
(when nil 1 2 3)
(unless t 1 2 3)
(unless nil 1 2 3)
(defparameter nota 7)
(cond ((<= nota 5) 'suspenso)
      ((<= nota 7) 'aprobado)
      ((<= nota 9) 'notable)
       (t 'sobresaliente))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; EJEMPLO 5
;;;
;;; Definición de funciones
;;;
;;;	Evalua el siguiente código y asegurate de
;;;	entender cómo funcionan las funciones ahí
;;;	definidas. A continuación, implementa en LISP
;;;	la función 'my-length' cuyo pseudocódigo
;;;	escribiste anteriormente, y comprueba su
;;;	correcto funcionamiento mediante diferentes
;;;	pruebas.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun elimina-primero (lista)
  (rest lista))
(defparameter lista '(1 2 3 4 5 6 7))
(elimina-primero lista)
lista

(defun elimina-segundo (lista)
  (cons (first lista)
        (rest (rest lista))))
(defparameter lista '(1 2 3 4 5 6 7))
(elimina-segundo lista)
lista

(defun elimina-enesimo (lista n)
  (if (<= n 1)
      (rest lista)
      (cons (first lista)
            (elimina-enesimo (rest lista) (- n 1)))))
(defparameter lista '(1 2 3 4 5 6 7))
(elimina-enesimo lista 4)
lista

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; EJEMPLO 6
;;;
;;; Aplicación repetida de funciones (pag. 4-5)
;;;
;;;	Una vez hayas comprendido el siguiente código,
;;;	implementa una función 'sum-range' que reciba
;;;	un número n y devuelva el resultado de sumar
;;;	todos los números desde 1 hasta n. Por
;;;	ejemplo, (sum-range 10) debe dar 55 y
;;;	(sum-range 100) es 5050. Si el número pasado
;;;	no es positivo haz que 'sum-range' devuelva
;;;	nil. Puedes definir funciones auxiliares si
;;;	te facilita la tarea
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(mapcar #'(lambda (x) (* x x)) '(1 2 3))
(defparameter sqr (lambda (x) (* x x)))
(mapcar sqr '(1 2 3))
(maplist #'append '(1 2 3 4) '(1 2) '(1 2 3))
(funcall sqr 3)
(apply sqr '(3))
(apply #'+ '(1 2 3 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; EJEMPLO 7
;;;
;;; Comparaciones y búsquedas (pag. 5-6)
;;;
;;;	Estudia el siguiente código y razona las
;;;	semejanzas y las diferencias entre los
;;;	resultados obtenidos con eq, eqly equal.
;;;
;;;	Una vez entendidas, implementa una versión
;;;	propia de 'member' llamada 'my-member' que
;;;	reciba 3 argumentos: el elemento, la lista
;;;	el comparador a usar. Prueba con distintos
;;;	ejemplos tanto con 'eql' como con 'equal',
;;;	y asegúrate de que devuelve lo mismo que el
;;;	'member' nativo con el test adecuado.
;;;
;;;	Cuando hayas comprobado su corrección,
;;;	codifica una función  'my-count' que haga uso
;;;	de 'my-member'. De nuevo, comprueba que
;;;	obtienes los mismos resultados que el
;;;	'count' nativo.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter a 3)
(defparameter b 3)
(eq a a)
(eq a 3)
(eq a b)
(eq a 3.0)
(eql a 3)
(eql a b)
(eql a 3.0)
(equal a 3)
(equal a b)
(equal a 3.0)
(= a 3)
(= a b)
(= a 3.0)

(defparameter lst '(1 2 3))
(defparameter lst2 '(1 2 3))
(eq lst lst)
(eq lst '(1 2 3))
(eq lst lst2)
(eql lst '(1 2 3))
(eql lst lst2)
(equal lst '(1 2 3))
(equal lst lst2)

(member 2 lst)
(member-if #'oddp lst)
(position 1 lst)
(position-if #'zerop lst)
(remove 3 lst)
(remove-if #'evenp lst)
(every #'numberp lst)
(some #'minusp lst)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; EJEMPLO 8
;;;
;;; Comparación de implementaciones (pag. 6)
;;;
;;;	Estudia el siguiente código donde se dan tres
;;;	versiones de una función que obtiene los
;;;	elementos pares de una lista. Comenta las
;;;	ventajas e inconvenientes que le ves a cada
;;;	una de esas implementaciones.
;;;
;;;	Comprueba la eficiencia de estas
;;;	implementaciones. Para ello, construye una
;;;	lista grande con ayuda de la función 'make-list'
;;;	(por ejemplo de tamaño 1000) y mide tiempos de
;;;	ejecución aciendo uso de la función 'time'.
;;;	Analiza los resultados obtenidos.
;;;
;;;	A la vista de estos resultados, ¿crees que
;;;	podrías implementar más eficientemente
;;;	'my-length' y 'my-member'? En caso de que sí,
;;;	prueba a reimplementarlas y compara
;;;	ejecuciones con time a ver si estás en lo
;;;	cierto.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; obten-pares: Implementación con remove-if
;;;

(defparameter lista '(1 2 4 5 0 6 8 7 4))

(defun obten-pares (lista)
  (remove-if #'oddp lista))

(obten-pares lista)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; obten-pares: Implementación recursiva
;;;

(defun obten-pares-recursiva (lista)
  (unless (null lista)
    (let ((primero (first lista)))
      (if (evenp primero)
        (cons primero (obten-pares-recursiva (rest lista)))
        (obten-pares-recursiva (rest lista))))))

(obten-pares-recursiva lista)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; obten-pares: Implementación con mapcar
;;;

(defun obten-pares-mapcar (lista)
  (remove NIL
          (mapcar #'(lambda (x)
                      (when (evenp x) x))
            lista)))

(obten-pares-mapcar lista)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  EJEMPLO 9
;;;
;;;  Funciones destructivas (pag. 6-7)
;;;
;;;	Estudia el siguiente código y verifica cómo
;;;	las funciones destructivas modifican los
;;;	argumetnos que se les pasan, mientras que las
;;;	no destructivas no lo hacen.
;;;
;;;	Codifica una función 'sorted-occurrences' que
;;;	cuente el número de veces que aparece cada
;;;	elemento distinto en una lista. Debe devolver
;;;	pares de la forma (elemento, número de
;;;	ocurrencias), donde el orden es de mayor a
;;;	menor según el número de ocurrencias. Por
;;;	ejemplo, (sorted-occurrences '(3 1 4 2 3 1 3
;;;	2 3 1)) debe devolver ((3 4) (1 3) (2 2)
;;;	(4 1)). Asegúrate de que la función no es
;;;	destructiva.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; remove (no destructiva) / delete (destructiva)
;;;

(defparameter *lista-amigos* '(jaime maria julio))

(defparameter *lista-amigos-masculinos* (remove 'maria *lista-amigos*))
*lista-amigos*

(defparameter *lista-amigos-masculinos* (delete 'maria *lista-amigos*))
*lista-amigos*

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; cons (no destructiva) / push (destructiva)
;;; first (no destructiva) / pop (destructiva)
;;;

(defparameter *dias-libres* '(domingo))

(cons 'sabado *dias-libres*)
*dias-libres*

(push 'sabado *dias-libres*)
*dias-libres*

(defparameter *dias-libres* (cons 'viernes *dias-libres*))
*dias-libres*

(first *dias-libres*)
*dias-libres*

(pop *dias-libres*)
*dias-libres*

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Ordenación de una lista por el cadr (second) de
;;;  sus elementos (versión no destructiva)
;;;

(defparameter lst '((a -4) (b -3)  (c 1)  (d 9)))
(sort (copy-list lst)                        ; copia solo el esqueleto de la lista
      #'(lambda(x y) (< (abs x) (abs y)))    ; compara valor abs
      :key #'second)                         ; del cadr
lst

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Ordenación de una lista por el cadr de sus
;;;  elementos (versión destructiva)
;;;

(defparameter lst '((a -4) (b -3)  (c 1)  (d 9)))
(sort lst
      #'(lambda(x y) (< (abs x) (abs y))) ; compara valor abs
      :key #'second)                      ; del cadr
lst

