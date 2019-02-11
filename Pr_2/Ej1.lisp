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
       