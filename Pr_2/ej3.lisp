(defun all-roots-newton (f df max-iter semillas &optional (tol 0.001))
  (if (null semillas)
      NIL
    (cons (newton f df max-iter (first semillas) tol) 
          (all-roots-newton f df max-iter (rest semillas) tol))))
