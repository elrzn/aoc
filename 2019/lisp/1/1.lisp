(defun fuel (mass)
  (- (floor (/ mass 3)) 2))

(defun part-one ()
  (with-open-file (stream "./input.txt")
    (loop for line = (read-line stream nil 'eof)
          until (eq line 'eof)
          summing (fuel (parse-integer line)))))
