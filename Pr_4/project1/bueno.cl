(defpackage :2301_P04_ef3e9 ; se declara un paquete con el grupo, la pareja y
  
  ; el código
  (:use :common-lisp :conecta4) ; el paquete usa common-lisp y conecta4
  (:export :heuristica :*alias*)) ; exporta la función de evaluación y un alias

(in-package 2301_P04_ef3e9)

(defvar *alias* '|Maldonado|) ; alias que aparece en el ranking

(defun heuristica (estado)
  ; current player standpoint
  (let* ((tablero (estado-tablero estado))
         (ficha-actual (estado-turno estado))
         (ficha-oponente (siguiente-jugador ficha-actual)))
    (if (juego-terminado-p estado)
        (let ((ganador (ganador estado)))
          (cond ((not ganador) 0)
                ((eql ganador ficha-actual) +val-max+)
                (t +val-min+)))
      (let ((puntuacion-actual 0)
            (puntuacion-oponente 0))
        (loop for columna from 0 below (tablero-ancho tablero) do
              (let* ((altura (altura-columna tablero columna))
                     (fila (1- altura))
                     (abajo (contar-abajo tablero ficha-actual columna fila))
                     (der (contar-derecha tablero ficha-actual columna fila))
                     (izq (contar-izquierda tablero ficha-actual columna fila))
                     (abajo-der (contar-abajo-derecha tablero ficha-actual columna fila))
                     (arriba-izq (contar-arriba-izquierda tablero ficha-actual columna fila))
                     (abajo-izq (contar-abajo-izquierda tablero ficha-actual columna fila))
                     (arriba-der (contar-arriba-derecha tablero ficha-actual columna fila)))
                (setf puntuacion-actual
                  (+ puntuacion-actual
                     (cond ((= abajo 0) 0)
                           ((= abajo 1) 10)
                           ((= abajo 2) 1000)
                           ((= abajo 3) 10000))
                     (cond ((= der 0) 0)
                           ((= der 1) 10)
                           ((= der 2) 1000)
                           ((= der 3) 10000))
                     (cond ((= izq 0) 0)
                           ((= izq 1) 10)
                           ((= izq 2) 1000)
                           ((= izq 3) 10000))
                     (cond ((= abajo-izq 0) 0)
                           ((= abajo-izq 1) 10)
                           ((= abajo-izq 2) 1000)
                           ((= abajo-izq 3) 10000)))))
              (let* ((altura (altura-columna tablero columna))
                     (fila (1- altura))
                     (abajo (contar-abajo tablero ficha-oponente columna fila))
                     (der (contar-derecha tablero ficha-oponente columna fila))
                     (izq (contar-izquierda tablero ficha-oponente columna fila))
                     (abajo-der (contar-abajo-derecha tablero ficha-oponente columna fila))
                     (arriba-izq (contar-arriba-izquierda tablero ficha-oponente columna fila))
                     (abajo-izq (contar-abajo-izquierda tablero ficha-oponente columna fila))
                     (arriba-der (contar-arriba-derecha tablero ficha-oponente columna fila)))
                (setf puntuacion-oponente
                  (+ puntuacion-oponente
                     (cond ((= abajo 0) 0)
                           ((= abajo 1) 10)
                           ((= abajo 2) 1000)
                           ((= abajo 3) 10000))
                     (cond ((= der 0) 0)
                           ((= der 1) 10)
                           ((= der 2) 1000)
                           ((= der 3) 10000))
                     (cond ((= izq 0) 0)
                           ((= izq 1) 10)
                           ((= izq 2) 1000)
                           ((= izq 3) 10000))
                     (cond ((= abajo-izq 0) 0)
                           ((= abajo-izq 1) 10)
                           ((= abajo-izq 2) 1000)
                           ((= abajo-izq 3) 10000))))))
        (- puntuacion-actual puntuacion-oponente)))))