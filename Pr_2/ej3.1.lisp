(defun list-not-nil-roots-newton (f df max-iter semillas &optional (tol 0.001))
  (mapcan (lambda (x) (if (null x) NIL (list x))) 
          (all-roots-newton f df max-iter semillas tol)))