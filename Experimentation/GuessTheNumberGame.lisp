;A game about guessing the numbers

(defparameter *small* 1)
(defparameter *big* 100)

(defun guess-the-number ()
  (ash (+ *small* *big*)-1))

(defun smaller ()
  (setf *big* (- (guess-the-number) 1))
  (guess-the-number))

(defun bigger ()
  (setf *small* (+ (guess-the-number) 1))
  (guess-the-number))

(defun resetNumbers()
  (setf *big* 100)
  (setf *small* 1)
  (guess-the-number))



