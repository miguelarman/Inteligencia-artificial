;;;;;;;;;;;;;;;;; Recursive
(defun norma-rec (x)
	(if(null x)
		0
		(+
			(* (first x) (first x))
			(norma-rec (rest x)))))

(defun producto-escalar-rec (x y)
	(cond
		((null x) 0)
		((null y) 0)
		(T
			(+ 
				(* (first x) (first y))
				(producto-escalar-rec (rest x) (rest y))))))

(defun cosine-distance-rec (x y)
	(let
		((nx-r (norma-rec x))
		 (ny-r (norma-rec y)))
	(cond
		((and (= nx-r 0)
			  (= ny-r 0))
				0)
		((= nx-r 0) 1)
		((= ny-r 0) 1)
		(T (- 1
				(/  (producto-escalar-rec x y)
					(*  (sqrt nx-r)
		 			 	(sqrt ny-r))))))))

;;;;;;;;;; Mapcar

(defun norma-mapcar (x)
	(apply #'+
		(mapcar #'* x x)))

(defun producto-escalar-mapcar (x y)
	(cond
		((null x) 0)
		((null y) 0)
		(T
			(apply #'+
				(mapcar #'* x y)))))

(defun cosine-distance-mapcar (x y)
	(let
		((nx-m (norma-mapcar x))
		 (ny-m (norma-mapcar y)))
	(cond
		((and (= nx-m 0)
			  (= ny-m 0))
				0)
		((= nx-m 0) 1)
		((= ny-m 0) 1)
		(T (- 1
				(/  (producto-escalar-mapcar x y)
					(*  (sqrt nx-m)
		 			 	(sqrt ny-m))))))))

(setf lsta '(1 2 3))
(setf lstb '(2 3 4))

(= (cosine-distance-rec lsta lstb) (cosine-distance-mapcar lsta lstb))
	
