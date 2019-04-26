(defpackage :2301_P04_cac9e ; se declara un paquete con el grupo, la pareja y
  
  ; el código
  (:use :common-lisp :conecta4) ; el paquete usa common-lisp y conecta4
  (:export :heuristica :*alias*)) ; exporta la función de evaluación y un alias

(in-package 2301_P04_cac9e)

(defvar *alias* '|Bravo|) ; alias que aparece en el ranking

(defun heuristica (estado)
  (cond
   ((ganador estado) +val-max+)
   ((tablas-p estado) (/ +val-max+ 2))  ; Ajustado este valor dando mas peso
   (T (random 100))))
