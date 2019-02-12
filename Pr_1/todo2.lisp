;;Ejercicio 2
;;Apartado 1

(defun iteracion (f df x0)
  (if (> x0 100000)
      NIL
    (- x0 (/ (funcall f x0) (funcall df x0)))))

(defun ultima-iteracion (f df penult tol)
  (let ((ultimo (iteracion f df penult)))
    (cond
     ((null ultimo) NIL)
     ((< (abs (- penult ultimo)) tol) ultimo)
     (T NIL))))

(defun newton-rec (f df max-iter x0)
  (if (> max-iter 0)
      (iteracion f df (newton-rec f df (- max-iter 1) x0))
    (iteracion f df x0)))

(defun newton (f df max-iter x0 &optional (tol 0.001)) 
  (cond ((<= tol 0) NIL)
        ((< max-iter 0) NIL)
        ((= max-iter 0) x0)
        (T 
         (ultima-iteracion f df (newton-rec f df (- max-iter 1) x0) tol))))



;;Apartado 2

(defun one-root-newton (f df max-iter semillas &optional (tol 0.001))
  (if (null semillas)
      NIL
    (let ((nx (newton f df max-iter (first semillas) tol)))
      (if (null nx)
           (one-root-newton f df max-iter (rest semillas) tol)
         nx))))

;;Apartado 3

(defun all-roots-newton (f df max-iter semillas &optional (tol 0.001))
  (if (null semillas)
      NIL
    (cons (newton f df max-iter (first semillas) tol) 
          (all-roots-newton f df max-iter (rest semillas) tol))))

;;Apartado 3.1

(defun list-not-nil-roots-newton (f df max-iter semillas &optional (tol 0.001))
  (mapcan (lambda (x) (if (null x) NIL (list x))) 
          (all-roots-newton f df max-iter semillas tol)))