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
	  (+ 1 (count-member-recursively symbol (rest list)))
	  (+ 0 (count-member-recursively symbol (rest list))))))
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

;;; 5. Reading a corpus file; basic counts
;; A.

;; with-open-file used open to create a file stream to the specified file, which in this
;; case is the file "brown1000.txt". It then goes into a loop and reads one line at the
;; time using (read-line stream nil) and stores the result in line. It checks that line is not
;; nil (i.e no more lines in the file) before calling the function tokenize with the line.
;; Tokenize starts a loop and sets the variables "start", which is the index for the first
;; character of a word, "space", which is the first occurence of a whitespace, and "token" which
;; stores the characters from start until space using subseq (subsequence/substring).

;; Token is therefore responsible for storing the current word. If the token is not empty, it is
;; collected (put into a list). The loop continues until space is nil, i.e the end of the line is
;; reached. It then returns the tokens as a list of words to the calling function
;; (with-open-file) which append it to its return value. The result is a list of all the words
;; in the input file.

;;(with-open-file (stream "brown1000.txt" :direction :input)
;;  (loop
;;     for line = (read-line stream nil)
;;     while line
;;     append (tokenize line)))

(defparameter *corpus* (read-corpus))
(defparameter *hashtable* (make-corpus-hashtable))

(defun tokenize (string)
  (loop
     for start = 0 then (+ space 1)
     for space = (position #\space string :start start)
     for token = (subseq string start space)
     unless (string= token "") collect token
     until (not space)))

(defun read-corpus ()
  (let ((file-stream (open "brown1000.txt")))
    (loop
       for line = (read-line file-stream nil)
       while line
       append (tokenize line))))

;; B.
;; There are 23132 tokens in our corpus
(defun length-of-corpus ()
  (length *corpus*))

;; C.
;; Our current strategy is to dive the text into tokens where whitespaces appear so that
;; "Hei Erik" becomes ("Hei" "Erik"). The problem with this is that if we wrote "Hei, Erik!"
;; the token will look like ("Hei," "Erik!"). Since this strategy just divide by whitespaces we
;; get all sorts of other characters attached to the words, or as words themselves. We can see
;; examples of this in our *corpus*-variable as we can find numbers (e.g "37"), other
;; characters (e.g ":" and "--"), single letters (e.g "J") and different capitalization of
;; words (e.g "The" and "the"). All these issues should be addressed.

;; D.
(defun make-corpus-hashtable ()
(let ((corpus-hashtable (make-hash-table)))
    (dotimes (i (length *corpus*))
      (if (gethash (nth i *corpus*) corpus-hashtable)
	  (incf (gethash (nth i *corpus*) corpus-hashtable))
	  (incf (gethash (nth i *corpus*) corpus-hashtable 0))))
    '(corpus-hashtable)))

