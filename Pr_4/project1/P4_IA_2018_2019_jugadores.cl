(use-package 'conecta4)

(declaim #+sbcl(sb-ext:muffle-conditions style-warning))

;; -------------------------------------------------------------------------------
;; Funciones de evaluación 
;; -------------------------------------------------------------------------------

(defun f-eval-bueno (estado)
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
			       ((= abajo 2) 100)
			       ((= abajo 3) 1000))
			 (cond ((= der 0) 0)
			       ((= der 1) 10)
			       ((= der 2) 100)
			       ((= der 3) 1000))
			 (cond ((= izq 0) 0)
			       ((= izq 1) 10)
			       ((= izq 2) 100)
			       ((= izq 3) 1000))
			 (cond ((= abajo-izq 0) 0)
			       ((= abajo-izq 1) 10)
			       ((= abajo-izq 2) 100)
			       ((= abajo-izq 3) 1000)))))
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
			       ((= abajo 2) 100)
			       ((= abajo 3) 1000))
			 (cond ((= der 0) 0)
			       ((= der 1) 10)
			       ((= der 2) 100)
			       ((= der 3) 1000))
			 (cond ((= izq 0) 0)
			       ((= izq 1) 10)
			       ((= izq 2) 100)
			       ((= izq 3) 1000))
			 (cond ((= abajo-izq 0) 0)
			       ((= abajo-izq 1) 10)
			       ((= abajo-izq 2) 100)
			       ((= abajo-izq 3) 1000))))))
	(- puntuacion-actual puntuacion-oponente)))))

;; -------------------------------------------------------------------------------
;; Jugadores 
;; -------------------------------------------------------------------------------

(defvar *jugador-aleatorio* (make-jugador :nombre 'Jugador-aleatorio
					  :f-jugador #'f-jugador-aleatorio
					  :f-eval  #'f-eval-aleatoria))

(defvar *jugador-bueno* (make-jugador :nombre 'Jugador-bueno
				      :f-jugador #'f-jugador-negamax
				      :f-eval  #'f-eval-bueno))

(defvar *jugador-humano* (make-jugador :nombre 'Jugador-humano
				       :f-jugador #'f-jugador-humano
                                       :f-eval  #'f-no-eval))

; -------------------------------------------------------

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

(defun diferencia-finales (estado)
  (let
      ((jugador (estado-turno estado)))
    (cond
     ((ganador estado) +val-max+)
     ((tablas-p estado) 0)                 ; Ajustar este valor
     (T
      (-
       (finales-posibles estado jugador)
       (finales-posibles estado (siguiente-jugador jugador)))))))



(defvar *jugador-optimista* (make-jugador :nombre 'Optimista
                                     :f-jugador #'f-jugador-negamax
                                     :f-eval  #'diferencia-finales))
; --------------------------------------------------------
; Aleatoria pero escoge los finales inmediatos
(defun f-aleatoria-mejorada (estado)
  (cond
   ((ganador estado) +val-max+)
   ((tablas-p estado) 0)                 ; Ajustar este valor
   (T (random 100))))

(defvar *jugador-aleatorio-mejorado* (make-jugador :nombre 'Aleatorio-mejorado
                                     :f-jugador #'f-jugador-negamax
                                     :f-eval  #'f-aleatoria-mejorada))
; --------------------------------------------------------


;; -------------------------------------------------------------------------------
;; Algunas partidas de ejemplo:
;; -------------------------------------------------------------------------------

(setf *verbose* nil)
;(setf *verbose* t)

;(print (partida *jugador-aleatorio* *jugador-aleatorio*))
;(print (partida *jugador-aleatorio* *jugador-bueno* 4))
;(print (partida *jugador-bueno* *jugador-aleatorio* 4))
;(print (partida *jugador-bueno* *jugador-bueno* 4))
;(print (partida *jugador-humano* *jugador-humano*))
;(print (partida *jugador-humano* *jugador-aleatorio* 4))
;(print (partida *jugador-humano* *jugador-bueno* 4))
;(print (partida *jugador-aleatorio* *jugador-humano*))

;(print (partida *jugador-aleatorio* *jugador-optimista* 4))

(defun prueba (n jugador enemigo)
  (setq val1 0)
  (setq val2 0)
  (dotimes (x n)
    (setq val1 (+ val1
                  (let
                      ((puntuacion (partida enemigo jugador 4)))
                    (if (null puntuacion)
                        0
                      puntuacion)))))
  (dotimes (x n)
    (setq val2 (+ val2 (let
                      ((puntuacion (partida jugador enemigo 4)))
                    (if (null puntuacion)
                        0
                      puntuacion)))))
  (print val1)
  (print (- n val2))
  (print (/ (+ val1 (- n val2)) (* 2 n))))


(print 'aleatorio_contra_optimista)
;(prueba 100 *jugador-optimista* *jugador-aleatorio*)

(print 'bueno_contra_optimista)
;(prueba 10 *jugador-optimista* *jugador-bueno*)

(print 'optimista_contra_optimista)
;(prueba 10 *jugador-optimista* *jugador-optimista*)

(print 'aleatorio_contra_aleatorio_mejorado)
;(prueba 100 *jugador-aleatorio-mejorado* *jugador-aleatorio*)

(print 'bueno_contra_aleatorio_mejorado)
;(prueba 100 *jugador-aleatorio-mejorado* *jugador-bueno*)

(print 'optimista_contra_aleatorio_mejorado)
;(prueba 100 *jugador-optimista* *jugador-aleatorio*)