;;;;OBLIGATORY EXERCISE 1, INF4820, FALL 2015
;;;;Written my Andreas Oven Aalsaunet, andreaoa@ifi.uio.no
;;;;Lisp environment used: SBCL

;;; 1. List processing
(defparameter *list* '(apple orange pear lemon))
(defun task1 ()
  "1. List processing"
  ;; A
  (print (first (rest (rest *list*))))
  (print (nth 2 *list*))

  ;; B
  (setf *list* '((apple orange) (pear lemon)))
  (print (first (first (rest *list*))))
  (print (nth 0 (nth 1 *list*)))
  (print (nth 0 (nth 0 (rest *list*))))

  ;; C
  (setf *list* '((apple) (orange) (pear)))
  (print (first (first (rest (rest *list*)))))
  (print (nth 0 (nth 2 *list*)))
  (print (nth 0 (first (rest (rest *list*)))))


  ;; D.b
  (print (cons '(apple orange) (cons '(pear lemon) nil)))
  ;; D.c
  (print (cons '(apple) (cons '(orange) (cons '(pear) nil))))
  T)

;; E
(defparameter *foo* '(a b c d e f g h i j k l m n o p q r s t u v w x y z))

(defun foo-1 (list)
  "Using the list length to find the next-to-last element"
  (let ((list_length (length list)))
    (nth (- list_length 2) list)))

(defun foo-2 (list)
  "Reversing the list and picking the second element"
  (nth 1 (reverse list)))

(defun foo-3 (list)
  "Using recursion for finding the next-to-last element"
  (if (not (equal (nth 2 list) NIL))
      (foo-3 (rest list))
      (nth 0 list)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; 2. Interpreting Common Lisp
(defun foo (foo)
  (if foo
      (+ 1 (foo (rest foo)))
	 0))

;; The purpose of this function is to count the number of elements in the list given as the
;; argument to the function in its first iteration.
;; The function does so recursively by calling itself over and over,
;; reducing the list by one element for each call (using rest), until the function is called with
;; an empty list/NIL-value which evalutes and returns 0. For the rest of the function calls on
;; the stack, previous to this, 1 is returned. The resulting value is thus "This += 1" for each
;; iteration which evalutes to a sum equal to the number of elements in the list.

;; Note that the symbol 'foo' is used twice in this function. The first foo is the symbol
;; representing the function name, while the second is representing the parameter to the function/
;; the name of the list. This works nicely as Lisp knows whether to expect a function-symbol or a
;; list-symbol as a given argument to an operation. E.g calling '(rest foo)' does the rest-operation
;; on the list while 'foo(...)' refers to the function.

;;; 3. Variable assignment
;; A.
(defun task3A () 
  (let ((foo (list 0 42 2 3)))
    (pop foo)
    (first foo)))

;; B.
(defun task3B ()
  (let* ((keys '(:a :b :c))
	 (values '(0 1 2))
	 (pairs (pairlis keys values)))
	 (setf pairs (acons :b 42 pairs))
    (rest (assoc :b pairs))))

;; C.
(defun task3C ()
  (let ((foo (make-hash-table)))
    (setf (gethash 'meaning foo) 41)
    (incf (gethash 'meaning foo))
    (gethash 'meaning foo)))

;; D.
(defun task3D ()
  (let ((foo (make-array 5)))
    (setf (aref foo 2) 42)
    (aref foo 2)))
  
;;; 4. Recursion and iteration
;; A.

(defun count-member-recursively (symbol list)
  (if (eql list nil)
      0
      (if (eql symbol (first list))
	  (+ 1 (count-member symbol (rest list)))
	  (+ 0 (count-member symbol (rest list))))))
;; B.
(defun count-member-iteratively (symbol list)
  (loop
     for i in list
     when (eql symbol i)
       count i))
       

(defun count-member-dotimes (symbol list)
  (let ((x 0))
    (dotimes (y (length list))
      (when (eql symbol (nth y list))
	(setf x (+ x 1))))
    x))
  

