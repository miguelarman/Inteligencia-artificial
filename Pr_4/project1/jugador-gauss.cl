(defpackage :2301_P04_7edf0 ; se declara un paquete con el grupo, la pareja y
  
  ; el código
  (:use :common-lisp :conecta4) ; el paquete usa common-lisp y conecta4
  (:export :heuristica :*alias*)) ; exporta la función de evaluación y un alias

(in-package 2301_P04_7edf0)

(defvar *alias* '|Padilla|) ; alias que aparece en el ranking

(defvar *gauss*
    (list
     (list 3 4 5 7 5 4 3)
     (list 4 6 8 10 8 6 4)
     (list 5 8 11 13 11 8 5)
     (list 5 8 11 13 11 8 5)
     (list 4 6 8 10 8 6 4)
     (list 3 4 5 7 5 4 3)))

(defun array-get (array i j)
  (nth j (nth i array)))

(defun cuenta-tablero (estado fila columna array)
    (let*
      ((tablero (estado-tablero estado)))
    (if (not (dentro-del-tablero-p tablero columna fila))
        0
      (let*
          ((valor (array-get array fila columna))
           (casilla (obtener-ficha tablero columna fila))
           (jugador (estado-turno estado))
           (contribucion (cond
                          ((null casilla) 0)
                          ((= casilla jugador) valor)
                          (T (- 0 valor)))))
      (if (not (dentro-del-tablero-p tablero (+ 1 columna) fila)) 
          (+
           (cuenta-tablero estado (+ 1 fila) 0 array)
           contribucion)
        (+
         (cuenta-tablero estado (+ 1 columna) fila array)
         contribucion))))))


(defun heuristica (estado)
  (if (juego-terminado-p estado)
      (let*
          ((ganador (ganador estado))
           (ficha (estado-turno estado)))
        (cond
         ((not ganador) 0)
         ((eql ganador ficha) +val-max+)
         (t +val-min+)))
    (cuenta-tablero estado 0 0 *gauss*)))
