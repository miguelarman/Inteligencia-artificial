(defun one-root-newton (f df max-iter semillas &optional (tol 0.001))
  (if (null semillas)
      NIL
    (let ((nx (newton f df max-iter (first semillas) tol)))
      (if (null nx)
           (one-root-newton f df max-iter (rest semillas) tol)
         nx))))