(defpackage :2301_P04_ef3e9 ; se declara un paquete con el grupo, la pareja y
  
  ; el c�digo
  (:use :common-lisp :conecta4) ; el paquete usa common-lisp y conecta4
  (:export :heuristica :*alias*)) ; exporta la funci�n de evaluaci�n y un alias

(in-package 2301_P04_ef3e9)

(defvar *alias* '|Maldonado|) ; alias que aparece en el ranking

(defun heuristica (estado)
  (if (juego-terminado-p estado)
      (let*
          ((ganador (ganador estado))
           (ficha (estado-turno estado)))
        (cond
         ((not ganador) 0)
         ((eql ganador ficha) +val-max+)
         (t +val-min+)))
    (-
     (finales-posibles estado jugador)
     (finales-posibles estado (siguiente-jugador jugador)))))

(defun contar-derecha-aux (tablero jugador columna fila n)
  (if (not (dentro-del-tablero-p tablero columna fila)) ; Caso base de la recusi�n
      0
    (let*
        ((ficha (obtener-ficha tablero columna fila))) 
      (if (= 1 n) ; Variable de la recursi�n que indica cuantas casillas a comprobar
          (if (or (null ficha) (eql ficha jugador))
              1
            0)
        (if (or (null ficha) (eql ficha jugador))
            (contar-derecha-aux tablero jugador (+ 1 columna) fila (- n 1))
          0)))))

(defun contar-abajo-aux (tablero jugador columna fila n)
  (if (not (dentro-del-tablero-p tablero columna fila))
      0
    (let*
        ((ficha (obtener-ficha tablero columna fila))) 
      (if (= 1 n)
          (if (or (null ficha) (eql ficha jugador))
              1
            0)
        (if (or (null ficha) (eql ficha jugador))
            (contar-abajo-aux tablero jugador columna (+ 1 fila) (- n 1))
          0)))))

(defun contar-abajo-derecha-aux (tablero jugador columna fila n)
  (if (not (dentro-del-tablero-p tablero columna fila))
      0
    (let*
        ((ficha (obtener-ficha tablero columna fila))) 
      (if (= 1 n)
          (if (or (null ficha) (eql ficha jugador))
              1
            0)
        (if (or (null ficha) (eql ficha jugador))
            (contar-abajo-derecha-aux tablero jugador (+ 1 columna) (+ 1 fila) (- n 1))
          0)))))

(defun contar-abajo-izquierda-aux (tablero jugador columna fila n)
  (if (not (dentro-del-tablero-p tablero columna fila)) ; Caso base de 
      0
    (let*
        ((ficha (obtener-ficha tablero columna fila))) 
      (if (= 1 n)
          (if (or (null ficha) (eql ficha jugador))
              1
            0)
        (if (or (null ficha) (eql ficha jugador))
            (contar-abajo-izquierda-aux tablero jugador (- columna 1) (+ 1 fila) (- n 1))
          0)))))




(defun finales-posibles-casilla (estado jugador columna fila)
  (let
      (tablero (estado-tablero estado))
    (+
     (contar-derecha-aux tablero jugador columna fila 4)
     (contar-abajo-aux tablero jugador columna fila 4)
     (contar-abajo-derecha-aux tablero jugador columna fila 4)
     (contar-abajo-izquierda-aux tablero jugador columna fila 4))))


(defun finales-posibles-recursiva (estado jugador columna fila)
  (let*
      ((tablero (estado-tablero estado)))
    (if (not (dentro-del-tablero-p tablero columna fila))
        0
      (if (not (dentro-del-tablero-p tablero (+ 1 columna) fila))
          (+
           (finales-posibles-recursiva estado jugador 0 (+ 1 fila))
           (finales-posibles-casilla estado jugador columna fila))
        (+
         (finales-posibles-recursiva estado jugador (+ 1 columna) fila)
         (finales-posibles-casilla estado jugador columna fila))))))
        
(defun finales-posibles (estado jugador)
  (finales-posibles-recursiva estado jugador 0 0))