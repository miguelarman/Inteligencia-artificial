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
      ((tablero (estado-tablero tablero))
       (cuenta (contar-abajo tablero ficha columna fila)))
    (cond
     ((and (> cuenta 0) (< cuenta 4)) 0)
     (T
      0))))

(defun finales-posibles-vertical-recursiva (estado jugador columna fila)
  (let*
      ((tablero (estado-tablero tablero)))
    (if (not (dentro-del-tablero-p tablero columna fila))
        0
      (if (not (dentro-del-tablero tablero (+ 1 columna) nfila))
          (+
           (finales-posibles-vertical-recursiva tablero (+ 1 columna) 0)
           (finales-posibles-vertical-casilla estado jugador columna fila))
        (+
         (finales-posibles-vertical-recursiva tablero columna (+ 1 fila))
         (finales-posibles-vertical-casilla estado jugador columna fila))))))
        
            
(defun finales-posibles-vertical (estado jugador)
  (finales-posibles-vertical-recursiva (estado jugador 0 0)))

(defun finales-posibles-horizontal (estado jugador) 0)

(defun finales-posibles-oblicuo-derecha (estado jugador) 0)

(defun finales-posibles-oblicuo-derecha (estado jugador) 0)

(defun finales-posibles (estado jugador)
  (+
   (finales-posibles-vertical (estado jugador))
   (finales-posibles-horizontal (estado jugador))
   (finales-posibles-oblicuo-derecha (estado jugador))
   (finales-posibles-oblicuo-derecha (estado jugador)))

(defun diferencia-finales (estado)
  (let
      ((jugador (estado-turno estado)))
    (-
     (finales-posibles estado jugador)
     (finales-posibles estado (siguiente-jugador jugador)))))



(defvar *otro-jugador* (make-jugador :nombre 'Otro-jugador
                                     :f-jugador #'f-jugador-negamax
                                     :f-eval  #'diferencia-finales))
; --------------------------------------------------------


;; -------------------------------------------------------------------------------
;; Algunas partidas de ejemplo:
;; -------------------------------------------------------------------------------

(setf *verbose* t)

;(print (partida *jugador-aleatorio* *jugador-aleatorio*))
;(print (partida *jugador-aleatorio* *jugador-bueno* 4))
;(print (partida *jugador-bueno* *jugador-aleatorio* 4))
;(print (partida *jugador-bueno* *jugador-bueno* 4))
;(print (partida *jugador-humano* *jugador-humano*))
;(print (partida *jugador-humano* *jugador-aleatorio* 4))
;(print (partida *jugador-humano* *jugador-bueno* 4))
;(print (partida *jugador-aleatorio* *jugador-humano*))

(print (partida *jugador-aleatorio* *otro-jugador* 4))

;;
