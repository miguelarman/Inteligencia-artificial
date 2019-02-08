;;;;;;;;;; Apartado 2

(defun calcular-confianza (x y)
	(/ 1 (+ 1 (cosine-distance-rec x y))))

(defun order-vectors-cosine-distance (vector lst-of-vectors &optional (confidence-level 0))
	(if (null lst-of-vectors)
		nil
		(let
			((n1 (first lst-of-vectors)))
			(if (>= (calcular-confianza vector n1) confidence-level)
				(cons n1 (order-vectors-cosine-distance vector (rest lst-of-vectors) confidence-level))
				(order-vectors-cosine-distance vector (rest lst-of-vectors) confidence-level)))))

(order-vectors-cosine-distance '(1 2 3) '((32 454  123) (133 12 1) (4 2 2)) 0.5)
(order-vectors-cosine-distance '(1 2 3) '((32 454  123) (133 12 1) (4 2 2)) 0.3)
(order-vectors-cosine-distance '(1 2 3) '((32 454  123) (133 12 1) (4 2 2)) 0.99)
