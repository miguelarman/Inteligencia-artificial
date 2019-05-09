(defpackage :2301_P04_cac9e ; se declara un paquete con el grupo, la pareja y
  
  ; el código
  (:use :common-lisp :conecta4) ; el paquete usa common-lisp y conecta4
  (:export :heuristica :*alias*)) ; exporta la función de evaluación y un alias

(in-package 2301_P04_cac9e)

(defvar *alias* '|Bravo|) ; alias que aparece en el ranking

(defun heuristica (estado)
  (if (juego-terminado-p estado)
      (let
          ((ganador (ganador estado))
           (ficha-actual (estado-turno estado)))
        (cond ((not ganador) 0)
              ((eql ganador ficha-actual) +val-max+)
              (t +val-min+)))
    (random 100)))
