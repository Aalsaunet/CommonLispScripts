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

(defun normalize-token (focusword)
  (string-trim '(#\Space #\Tab #\Newline #\. #\, #\; #\: #\- #\_ #\? #\! #\' #\" #\( #\) )
	       (string-downcase focusword)))


;;; Method for reading in focuswords and making the 1st level hash
;;; 1. Read in line by line (lines should consist of only one word) until EOF
;;; 2. Normalize the words (remove whitespaces, newlines etc )
;;; 3. Make every focusword a key in the 1st level hash

(defun read-words-to-hash (focuswords)
  (let ((file-stream (open focuswords)))
    (loop
       for line = (read-line file-stream nil)
       while line
       do (setf (gethash (normalize-token line) (vs-matrix vs-instance))
		(make-hash-table :test #'equal))))
  )

;;; Method for reading and matching words in corpus to focuswords
;;; and making the 2nd level hashes
;;; 1. Read in a sentence by sentence (sentence = words until ".") until EOF.
;;; 2. Tokenize the sentence (split sentence into list of words)
;;; 3. Normalize each word in the word list
;;; 4. Remove words in the list that are present in the *stop-list*
;;; 5. For each word left in the list: check if its a focusword (key in 1st level hash)
;;; 6. If it is, remove that word from the list of words. No focus words: Next sentence.
;;; 7. And do one of the following:
;;; 7a Add word(s) as keys in the 2nd level hash of that focusword and set their
;;;    value to 1.
;;; 7b OR if the word(s) is already a key in the 2nd level hash, increment the
;;;    value of that hash by 1.

;; (defun read-corpus-to-hash (corpus)

;;   )

(defun read-corpus-to-vs (focuswords)
  (read-words-to-hash focuswords)
  )

(defparameter *space* (read-corpus-to-vs "words.txt"))



;;; Method for printing out hash keys and values for the 1st level hash
;; (defun print-hash-entry (key value)
;;     (format t "The value associated with the key ~S is ~S~%" key value))

;; (maphash #'print-hash-entry (vs-matrix vs-instance))