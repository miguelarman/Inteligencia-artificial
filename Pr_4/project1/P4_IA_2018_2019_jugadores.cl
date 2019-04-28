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

(defun contar-derecha-aux (tablero jugador columna fila n)
  (if (not (dentro-del-tablero-p tablero columna fila))
      0
    (let*
        ((ficha (obtener-ficha tablero columna fila))) 
      (if (= 1 n)
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
  (if (not (dentro-del-tablero-p tablero columna fila))
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
  (+
   (contar-derecha-aux (estado-tablero estado) jugador columna fila 4)
   (contar-abajo-aux (estado-tablero estado) jugador columna fila 4)
   (contar-abajo-derecha-aux (estado-tablero estado) jugador columna fila 4)
   (contar-abajo-izquierda-aux (estado-tablero estado) jugador columna fila 4)))

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
(defun f-eval-mas (estado)
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
         (ficha-debajo (if (= fila 0) 1 (obtener-ficha tablero columna (- fila 1))))
         (conteo (contar-derecha-aux-mas tablero jugador (+ 1 columna) fila (- n 1)))) 
      (if (= 1 n)
          (cond ((eql ficha jugador) 2)
                ((and (null ficha) (or (= fila 0) (not (null ficha-debajo)))) 1)
                ((null ficha) 0)
                (T nil))
        (cond ((and (= n 4) (null conteo)) 0)
              ((null conteo) nil)
              ((eql ficha jugador) (+ 2 conteo))
              ((and (null ficha) (or (= fila 0) (not (null ficha-debajo)))) (+ 1 conteo))
              ((null ficha) conteo)
              ((= n 4) 0)
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
              ((= n 4) 0)
              (T nil))))))

(defun contar-abajo-derecha-aux-mas (tablero jugador columna fila n)
  (if (not (dentro-del-tablero-p tablero columna fila))
      nil
    (let*
        ((ficha (obtener-ficha tablero columna fila))
         (ficha-debajo (if (= fila 0) 1 (obtener-ficha tablero columna (- fila 1))))
         (conteo (contar-derecha-aux-mas tablero jugador (+ 1 columna) (+ 1 fila) (- n 1)))) 
      (if (= 1 n)
          (cond ((eql ficha jugador) 2)
                ((and (null ficha) (or (= fila 0) (not (null ficha-debajo)))) 1)
                ((null ficha) 0)
                (T nil))
        (cond ((and (= n 4) (null conteo)) 0)
              ((null conteo) nil)
              ((eql ficha jugador) (+ 2 conteo))
              ((and (null ficha) (or (= fila 0) (not (null ficha-debajo)))) (+ 1 conteo))
              ((null ficha) conteo)
              ((= n 4) 0)
              (T nil))))))

(defun contar-abajo-izquierda-aux-mas (tablero jugador columna fila n)
  (if (not (dentro-del-tablero-p tablero columna fila))
      nil
    (let*
        ((ficha (obtener-ficha tablero columna fila))
         (ficha-debajo (if (= fila 0) 1 (obtener-ficha tablero columna (- fila 1))))
         (conteo (contar-derecha-aux-mas tablero jugador (+ 1 columna) (+ 1 fila) (- n 1)))) 
      (if (= 1 n)
          (cond ((eql ficha jugador) 2)
                ((and (null ficha) (or (= fila 0) (not (null ficha-debajo)))) 1)
                ((null ficha) 0)
                (T nil))
        (cond ((and (= n 4) (null conteo)) 0)
              ((null conteo) nil)
              ((eql ficha jugador) (+ 2 conteo))
              ((and (null ficha) (or (= fila 0) (not (null ficha-debajo)))) (+ 1 conteo))
              ((null ficha) conteo)
              ((= n 4) 0)
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
      (+ horizontal vertical diag1 diag2))))


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

(defvar *jugador-mas-optimista* (make-jugador :nombre 'Mas-optimista
                                          :f-jugador #'f-jugador-negamax
                                          :f-eval  #'f-eval-mas))
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
                        1
                      puntuacion)))))
  (print val1)
  (print (- n val2))
  (print (float (/ (+ val1 (- n val2)) (* 2 n)))))

; (setf l '(a b)) -> (A B)
; (car l) -> A
; (cadr l) -> B
; (setf lista '(0 0))
; (setf lista (cons (+ 0 (car lista)) (cons (+ 0 (cadr lista)) nil)))



;(print 'optimista_contra_aleatorio)
;(time (prueba 25 *jugador-optimista* *jugador-aleatorio*))

;(print 'optimista_contra_bueno)
;(prueba 10 *jugador-optimista* *jugador-bueno*)

;(print 'optimista_contra_optimista)
;(prueba 10 *jugador-optimista* *jugador-optimista*)

;(print 'aleatorio_mejorado_contra_aleatorio)
;(prueba 100 *jugador-aleatorio-mejorado* *jugador-aleatorio*)

;(print 'aleatorio_mejorado_contra_bueno)
;(prueba 100 *jugador-aleatorio-mejorado* *jugador-bueno*)

;(print 'optimista_contra_aleatorio_mejorado)
;(prueba 20 *jugador-optimista* *jugador-aleatorio-mejorado*)


;(print 'mas_optimista_contra_aleatorio)
;(time (prueba 25 *jugador-mas-optimista* *jugador-aleatorio*))

;(print 'mas_optimista_contra_bueno)
;(prueba 1 *jugador-mas-optimista* *jugador-bueno*)

;(print 'mas_optimista_contra_mas_optimista)
;(prueba 1 *jugador-mas-optimista* *jugador-mas-optimista*)

;(print 'mas_optimista_contra_optimista)
;(prueba 1 *jugador-mas-optimista* *jugador-optimista*)
