(defpackage :3012_P01_7edf0 ; se declara un paquete con el grupo, la pareja y
  
  ; el código
  (:use :common-lisp :conecta4) ; el paquete usa common-lisp y conecta4
  (:export :heuristica :*alias*)) ; exporta la función de evaluación y un alias

(in-package 3012_P01_7edf0)

(defvar *alias* '|Balodding optimista v2|) ; alias que aparece en el ranking

(defun heuristica (estado) (let
      ((jugador (estado-turno estado)))
    (-
     (finales-posibles estado jugador)
     (finales-posibles estado (siguiente-jugador jugador)))))

(defun finales-posibles-vertical-casilla (estado jugador columna fila)
  (let*
      ((tablero (estado-tablero estado)))
    (if (not (dentro-del-tablero-p tablero columna fila))
        0
      (let*
          ((cuenta (contar-abajo tablero jugador columna fila)))
        (cond
         ((and (> cuenta 0) (< cuenta 4)) 0)          ; Casilla propia pero no hay suficientes conectadas
         ((null (obtener-ficha tablero columna fila)) ; Casilla libre
          (+ 1 (finales-posibles-vertical-casilla
                estado jugador
                columna (+ 1 fila))))
         (T                                           ; Casilla enemiga
          0))))))

(defun finales-posibles-horizontal-casilla (estado jugador columna fila)
  (let*
      ((tablero (estado-tablero estado)))
    (if (not (dentro-del-tablero-p tablero columna fila))
        0
      (let*
          ((cuenta (contar-derecha tablero jugador columna fila)))
        (cond
         ((and (> cuenta 0) (< cuenta 4)) 0)          ; Casilla propia pero no hay suficientes conectadas
         ((null (obtener-ficha tablero columna fila)) ; Casilla libre
          (+ 1 (finales-posibles-horizontal-casilla
                estado jugador
                (+ 1 columna) fila)))
         (T                                           ; Casilla enemiga
          0))))))

(defun finales-posibles-oblicuo-derecha-casilla (estado jugador columna fila)
  (let*
      ((tablero (estado-tablero estado)))
    (if (not (dentro-del-tablero-p tablero columna fila))
        0
      (let*
          ((cuenta (contar-abajo-derecha tablero jugador columna fila)))
        (cond
         ((and (> cuenta 0) (< cuenta 4)) 0)          ; Casilla propia pero no hay suficientes conectadas
         ((null (obtener-ficha tablero columna fila)) ; Casilla libre
          (+ 1 (finales-posibles-oblicuo-derecha-casilla
                estado jugador
                (+ 1 columna) (+ 1 fila))))
         (T                                           ; Casilla enemiga
          0))))))

(defun finales-posibles-oblicuo-izquierda-casilla (estado jugador columna fila)
  (let*
      ((tablero (estado-tablero estado)))
    (if (not (dentro-del-tablero-p tablero columna fila))
        0
      (let*
          ((cuenta (contar-abajo-izquierda tablero jugador columna fila)))
        (cond
         ((and (> cuenta 0) (< cuenta 4)) 0)          ; Casilla propia pero no hay suficientes conectadas
         ((null (obtener-ficha tablero columna fila)) ; Casilla libre
          (+ 1 (finales-posibles-oblicuo-izquierda-casilla
                estado jugador
                (- columna 1) (+ 1 fila))))
         (T                                           ; Casilla enemiga
          0))))))


(defun finales-posibles-casilla (estado jugador columna fila)
  (+
   (finales-posibles-vertical-casilla estado jugador columna fila)
   (finales-posibles-horizontal-casilla estado jugador columna fila)
   (finales-posibles-oblicuo-derecha-casilla estado jugador columna fila)
   (finales-posibles-oblicuo-izquierda-casilla estado jugador columna fila)))

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