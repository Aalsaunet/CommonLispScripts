;;;; Assignment 2a in INF4820, Autumn 2015
;;;; Andreas Oven Aalsaunet, andreaoa@ifi.uio.no

;;; Task 1A.
;; There are several ways of defining context of words in  addition to the bag-of-words approuch
;; this assignment will use. One alternative to the bag-of-words definition is to use context
;; windows. With the context windows method we define context as the N number of words before
;; and after the current word we are focusing on. If we have the sentence
;; "Here are five given words" and we have "five" as our focus words and N = 1,
;; then the features for "five" is ["are", "given"].

;; We could also have defined context as grammatical context which looks at the grammatical
;; relations to other words.

;;; Task 2A

(defstruct (vs)
  (matrix (make-hash-table :test #'equal))
  similarity-fn)

(defparameter vs-instance (make-vs)) 

;;; Task 2B
(defparameter *stop-list*
  '("a" "about" "also" "an" "and" "any" "are" "as" "at" "be" "been"
    "but" "by" "can" "could" "do" "for" "from" "had" "has" "have"
    "he" "her" "him" "his" "how" "i" "if" "in" "is" "it" "its" "la"
    "may" "most" "new" "no" "not" "of" "on" "or" "she" "some" "such"
    "than" "that" "the" "their" "them" "there" "these" "they" "this"
    "those" "to" "was" "we" "were" "what" "when" "where" "which"
    "who" "will" "with" "would" "you"))

(defun normalize-token (word)
  (setf word (string-downcase word))
  (string-trim '(#\Space #\Tab #\Newline #\. #\, #\; #\: #\- #\_ #\? #\! #\') word))

(defun read-words-to-hash (words)
  (let ((file-stream (open words)))
    (loop
       for line = (read-line file-stream nil)
       while line
       do (setf (gethash (normalize-token line) (vs-matrix vs-instance))
		(make-hash-table :test #'equal))))
  )

;; (defun read-corpus-to-hash (corpus)

;;   )

(defun read-corpus-to-vs (words)
  (read-words-to-hash words)
  )

(defparameter *space* (read-corpus-to-vs "words.txt"))



;;; Metoder for å skrive ut hashverdier
;; (defun print-hash-entry (key value)
;;     (format t "The value associated with the key ~S is ~S~%" key value))

;; (maphash #'print-hash-entry (vs-matrix vs-instance))
