(defpackage :2301_P04_cac9e ; se declara un paquete con el grupo, la pareja y
  
  ; el código
  (:use :common-lisp :conecta4) ; el paquete usa common-lisp y conecta4
  (:export :heuristica :*alias*)) ; exporta la función de evaluación y un alias

(in-package :2301_P04_cac9e)

(defvar *alias* '|Bravo|) ; alias que aparece en el ranking

(defun heuristica (estado)
  (let
      ((jugador (estado-turno estado)))
    (cond
     ((ganador estado) +val-max+)
     ((tablas-p estado) 0) ; Ajustado este valor dandole mas peso
     (T
      (-
       (finales-posibles-mas estado jugador nil)
       (finales-posibles-mas estado (siguiente-jugador jugador) T))))))


(defun contar-derecha-aux-mas (tablero jugador columna fila n)
  (if (not (dentro-del-tablero-p tablero columna fila))
      nil
    (let*
        ((ficha (obtener-ficha tablero columna fila))
         (ficha-debajo (obtener-ficha tablero columna (- fila 1)))
         (conteo (contar-derecha-aux-mas tablero jugador (+ 1 columna) fila (- n 1)))) 
      (if (= 1 n)
          (cond ((eql ficha jugador) 2)
                ((and (null ficha) (or (= fila 1) (not (null ficha-debajo)))) 1)
                ((null ficha) 0)
                (T nil))
        (cond ((and (= n 4) (null conteo)) 0)
              ((null conteo) nil)
              ((eql ficha jugador) (+ 2 conteo))
              ((and (null ficha) (or (= fila 1) (not (null ficha-debajo)))) (+ 1 conteo))
              ((null ficha) conteo)
              (T nil))))))

(defun contar-abajo-aux-mas (tablero jugador columna fila n)
  (if (not (dentro-del-tablero-p tablero columna fila))
      nil
    (let*
        ((ficha (obtener-ficha tablero columna fila))
         (conteo (contar-abajo-aux-mas tablero jugador columna (+ 1 fila) (- n 1))))
      (if (= 1 n) ;Caso base
          (cond ((eql ficha jugador) 1)
                ((null ficha) 0)
                (T nil))
        (cond ((and (= n 4) (null conteo)) 0) ;Nil si no hay espacio para hacer 4 o si el enemigo lo ha interceptado
              ((null conteo) nil) ;1 si tenemos una ficha nuestra puesta
              ((null ficha) conteo) ;0 si no hay una ficha puesta aun
              ((eql ficha jugador) (+ 1 conteo))
              (T nil))))))

(defun contar-abajo-derecha-aux-mas (tablero jugador columna fila n)
  (if (not (dentro-del-tablero-p tablero columna fila))
      nil
    (let*
        ((ficha (obtener-ficha tablero columna fila))
         (ficha-debajo (obtener-ficha tablero columna (- fila 1)))
         (conteo (contar-derecha-aux-mas tablero jugador (+ 1 columna) (+ 1 fila) (- n 1)))) 
      (if (= 1 n)
          (cond ((eql ficha jugador) 2)
                ((and (null ficha) (or (= fila 1) (not (null ficha-debajo)))) 1)
                ((null ficha) 0)
                (T nil))
        (cond ((and (= n 4) (null conteo)) 0)
              ((null conteo) nil)
              ((eql ficha jugador) (+ 2 conteo))
              ((and (null ficha) (or (= fila 1) (not (null ficha-debajo)))) (+ 1 conteo))
              ((null ficha) conteo)
              (T nil))))))

(defun contar-abajo-izquierda-aux-mas (tablero jugador columna fila n)
  (if (not (dentro-del-tablero-p tablero columna fila))
      nil
    (let*
        ((ficha (obtener-ficha tablero columna fila))
         (ficha-debajo (obtener-ficha tablero columna (- fila 1)))
         (conteo (contar-derecha-aux-mas tablero jugador (+ 1 columna) (+ 1 fila) (- n 1)))) 
      (if (= 1 n)
          (cond ((eql ficha jugador) 2)
                ((and (null ficha) (or (= fila 1) (not (null ficha-debajo)))) 1)
                ((null ficha) 0)
                (T nil))
        (cond ((and (= n 4) (null conteo)) 0)
              ((null conteo) nil)
              ((eql ficha jugador) (+ 2 conteo))
              ((and (null ficha) (or (= fila 1) (not (null ficha-debajo)))) (+ 1 conteo))
              ((null ficha) conteo)
              (T nil))))))




(defun finales-posibles-casilla-mas (estado jugador columna fila enemigo)
  (let*
      ((tablero (estado-tablero estado))
       (horizontal (contar-derecha-aux-mas tablero jugador columna fila 4))
       (vertical (contar-abajo-aux-mas tablero jugador columna fila 4))
       (diag1 (contar-abajo-derecha-aux-mas tablero jugador columna fila 4))
       (diag2 (contar-abajo-izquierda-aux-mas tablero jugador columna fila 4)))
    (if (and enemigo (or ;Damos maxima prioridad a que el enemigo no gane en la siguiente tirada
         (= vertical 3)
         (= horizontal 7)
         (= diag1 7)
         (= diag2 7)))
        (- +val-max+ 1)
      (+ horizontal vertical diag1 diag2) )))


(defun finales-posibles-recursiva-mas (estado jugador columna fila enemigo)
  (let*
      ((tablero (estado-tablero estado)))
    (if (not (dentro-del-tablero-p tablero columna fila))
        0
      (if (not (dentro-del-tablero-p tablero (+ 1 columna) fila))
          (+
           (finales-posibles-recursiva-mas estado jugador 0 (+ 1 fila) enemigo)
           (finales-posibles-casilla-mas estado jugador columna fila enemigo))
        (+
         (finales-posibles-recursiva-mas estado jugador (+ 1 columna) fila enemigo)
         (finales-posibles-casilla-mas estado jugador columna fila enemigo))))))
        
(defun finales-posibles-mas (estado jugador enemigo)
  (finales-posibles-recursiva-mas estado jugador 0 0 enemigo))