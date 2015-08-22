;;;;Call this function like this: (addTwoNumbers 10 20)
(defun sum1(x y)
  "Sum any two numbers after printing a message."
  (format t "Summing ~d and ~d.~%" x y)
  (+ x y ))

(defun sum2(x y)
  "Sum any two numbers after printing a message."
  (format t "~d + ~d = ~d.~%" x y (+ x y)))

(defun sum3(a b  &optional (c 0) (d 0))
  (+ a b c d))


 
