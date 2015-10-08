;;;; Assignment 2b in INF4820, Autumn 2015
;;;; Written by Andreas Oven Aalsaunet, andreaoa@ifi.uio.no
;;;; Lisp environment used: SBCL
;;;; See the end of this file for documentation beyond regular comments

;;;;;;;;;;;;;;;;;;;;;;;;;;; EXERCISE 2A STUFF ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Task 1A.
;; There are several ways of defining context of words in  addition to the bag-of-words approach
;; this assignment will use. One alternative to the bag-of-words definition is to use context
;; windows. With the context windows method we define context as the N number of words before
;; and after the current word we are focusing on. If we have the sentence
;; "Here are five given words" and we have "five" as our focus words and N = 1,
;; then the features for "five" is '("are" "given").

;; We could also have defined context as grammatical context which looks at the grammatical
;; relations to other words.

;;; Task 2A
(defstruct (vs)
  (matrix (make-hash-table :test #'equal))
  (similarity-fn 'dot-product)
  (classes (make-hash-table :test #'equal))
  (proxy-matrix (make-hash-table :test #'equal)))

;;; Task 2B
;; For creating the vector space I decided to use a two level hash where
;; each value of the first level hash is itself a hash table. I chose this approach
;; to avoid many empty-/zero-values (sparse feature vectors), which takes time and space
;; to save and iterate over. The lvl1 key is thereby a focusword (e.g. from words.txt),
;; a lvl2 key is one feature of that word (which in itself is a word) and
;; lvl2 value is its length/magnitude. This gives us something like this:
;;
;; Matrix structure: Hash lvl1 -> Key lvl1 -----> Value lvl1 -> Key lvl2 -> Value lvl2
;; Concrete example: vs-matrix -> "university" -> HASH -------> "college"-> 15

;; The stop-list is a list for readability, although it probably would have been
;; more efficent to use other datastructes such as a hash, and check for existence via gethash.
;; Instead this program uses a list and check for existence with the "member". As the
;; efficency loss is trivial for this task, I thereby value readability more.

(defparameter *stop-list*
  '("" "a" "about" "also" "an" "and" "any" "are" "as" "at" "be" "been"
    "but" "by" "can" "could" "do" "for" "from" "had" "has" "have"
    "he" "her" "him" "his" "how" "i" "if" "in" "is" "it" "its" "la"
    "may" "most" "new" "no" "not" "of" "on" "one" "or" "she" "some" "such"
    "than" "that" "the" "their" "them" "there" "these" "they" "this"
    "those" "to" "was" "we" "were" "what" "when" "where" "which"
    "who" "will" "with" "would" "you"))

;; This function trims the defined character of the word given and downcase it.
(defun normalize-token (word)
  (string-trim '(#\Space #\Tab #\Newline #\. #\, #\; #\: #\- #\_ #\? #\! #\' #\" #\( #\) )
  	       (string-downcase word)))

;; Opens the file with filename=focuswords. Reads line by line (word by word),
;; normalized the word and makes a hash in the vs-matrix where the words are keys. 
(defun read-words-to-hash (focuswords vs-instance)
  (let ((file-stream (open focuswords)))
    (loop
       for line = (read-line file-stream nil)
       while line
       do (setf (gethash (normalize-token line) (vs-matrix vs-instance))
		(make-hash-table :test #'equal))))
  )

;; Receives a normalized word list and iterates through every word. If a word is a
;; key in the vs-matrix, thus a focusword, the rest of the words in the sentence
;; is stored as feature vectors for that word and the word itself is removed from
;; the normalized-list. 
(defun corpus-to-hash (normalized-list vs-instance)
  (dolist (word normalized-list)
    (if (gethash word (vs-matrix vs-instance))
	(dolist (remaining-word (remove word normalized-list :test #'equal :count 1))
	    (if (gethash remaining-word (gethash word (vs-matrix vs-instance)))
		(incf (gethash remaining-word (gethash word (vs-matrix vs-instance))))
		(setf (gethash remaining-word (gethash word (vs-matrix vs-instance))) 1))))))

;; Receives a word list, calls normalize-token on each word and checks if it is in
;; the stoplist. If it isnt it is added to the normalized-list. After checking all
;; the words corpus-to-hash is called with the normalized-list.
(defun filter-words (wordlist vs-instance)
  (let ((normalized-list '())
	(currentWord nil))
    (dolist (word wordlist)
      (setf currentWord (normalize-token word))
      (if (not (member currentWord *stop-list* :test #'equal))
	  (push currentWord normalized-list)))
    (corpus-to-hash normalized-list vs-instance)))

;; Tokenizes the the sentence given (split sentence into word list) and returns it.
(defun tokenize (string)
  (loop
     for start = 0 then (+ space 1)
     for space = (position #\space string :start start)
     for token = (subseq string start space)
     unless (string= token "") collect token
     until (not space)))

;; Reads line by line from the corpus and calls the tokenize and filter-words
;; functions (in that order) on each line.
(defun read-from-corpus (corpus vs-instance)
  (let ((file-stream (open corpus)))
    (loop
       for line = (read-line file-stream nil)
       while line
       do (filter-words (tokenize line) vs-instance))))

;; Creates an instance of vs and calls read-words-to-hash and read-from-corpus, before
;; "returning" the vs-object
(defun read-corpus-to-vs (corpus focuswords)
  (let ((vs-instance (make-vs)))
    (read-words-to-hash focuswords vs-instance)
    (read-from-corpus corpus vs-instance)
    (eval vs-instance)))

;; This variable calls read-corpus-to-vs to create an vs-object, read and
;; create a vectorspace. The vs-instance in then "returned" from read-corpus-to-vs
;; and stored in this variable
(defparameter *space* (read-corpus-to-vs "brown2.txt" "words.txt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Task 2C
;; Gets the feature vector of a given words in a given vector space.
(defun get-feature-vector (vs-struct word)
  (gethash word (vs-matrix vs-struct)))

;;; Task 2D
;; Prints a sorted list of the given number of "limit" features with the
;; highest count/value for the given word "word" in vectorspace "vectorspace"
(defun print-features (vs-struct word limit)
  (let ((key-value-list '()))
    (maphash #'(lambda (key value)
		 (push (list key value) key-value-list))
	     (get-feature-vector vs-struct word))
    (setf key-value-list (sort key-value-list #'> :key #'cadr))
    (dotimes (i limit)
      (let ((pair (pop key-value-list)))
	(format t "The feature ~S appears ~S times ~C" (car pair) (cadr pair) #\newline)))))
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Task 3A
;; Computes the norm of the given feature vector
(defun euclidean-length (feature-vector)
   (let ((sum 0))
     (maphash (lambda (key value)
		(declare (ignore key))
		(setf sum (+ sum (* value value)))) feature-vector)
     (sqrt sum)))
	 	     
;;; Task 3B
;; Normalizes the given vector space so all feature-vectors have unit length
(defun length-normalize-vs (vs-struct)
  (maphash (lambda (key value)
	     (let ((vlength (euclidean-length (get-feature-vector vs-struct key))))
	       (maphash (lambda (k v)
			  (setf (gethash k value) (/ v vlength))) value)))
	       (vs-matrix vs-struct)))

;;; Task 3C
;; Uses the cosine measure for finding the similarity of the two given words.
;; This function only work as intended with normalized vectors. 
(defun dot-product (vector1 vector2)
  (let ((sum 0))
    (maphash (lambda (key value)
	       (if (gethash key vector2)
		   (setf sum (+ sum (* value (gethash key vector2)))))) vector1)
    (eval sum)))


;;; Task 3D
;; Takes an instance of struct "vs" and two strings as arguments (focuswords)
;; and find the words corresponding vectors from the vs-matrix.
;; The function then used the vs-similarity-fn to find the similarity
;; and prints the result.
(defun word-similarity (vs-struct word1 word2)
  (let ((vector1 (get-feature-vector vs-struct word1))
	(vector2 (get-feature-vector vs-struct word2)))
  (format t "The Cosine similarity between the words ~S and ~S is ~S"
	 word1 word2 (funcall (vs-similarity-fn vs-struct) vector1 vector2))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Method for printing out hash keys and values
(defun print-hash-entry (key value)
  (format t "The value associated with the key ~S is ~S~%" key value))

;; For testing purposes:
;;(maphash #'print-hash-entry (vs-proxy-matrix *space*))
;;(maphash #'print-hash-entry (gethash "university" (vs-matrix vs-instance)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;; EXERCISE 2B ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SET UP FUNCTION ;;;
(defun build-vs (vs-struct)
  (read-classes vs-struct "classes.txt")
  (compute-class-centroids vs-struct)
  (length-normalize-vs vs-struct)
  (compute-proximities vs-struct))


;;; TASK 1A ;;;
;; Builds the proximity-matrix.
(defun compute-proximities (vs-struct)
  (maphash (lambda (key value)
	     (declare (ignore value))
	     (setf (gethash key (vs-proxy-matrix vs-struct))
			(make-hash-table :test #'equal))
		  (maphash (lambda (key2 value2)
			     (declare (ignore value2))
			     (if (not (gethash key2 (vs-proxy-matrix vs-struct)))
				 (setf (gethash key2
						(gethash key (vs-proxy-matrix vs-struct)))
				       (funcall (vs-similarity-fn vs-struct)
						(get-feature-vector vs-struct key)
						(get-feature-vector vs-struct key2)))))
			   (vs-matrix vs-struct)))
	   (vs-matrix vs-struct)))

(defun get-proximities (vs-struct word1 word2)
  (let ((proximity-score (gethash word1 (gethash word2 (vs-proxy-matrix vs-struct)))))
    (if proximity-score
	(return-from get-proximities proximity-score))
    (return-from get-proximities
      (gethash word2 (gethash word1 (vs-proxy-matrix vs-struct))))))

;;; TASK 1B ;;;
(defun find-knn (vs-struct word &optional (k 5))
  (let ((similarity-list '())
	(ranked-list '()))
    (maphash (lambda (key value)
	       (if (equal word key)
		   (maphash (lambda (key2 value2)
			      (push (list key2 value2) similarity-list)) value)
		   (maphash (lambda (key2 value2)
			      (if (equal word key2)
				  (push (list key value2) similarity-list))) value)))
	     (vs-proxy-matrix vs-struct))
    (setf similarity-list (sort similarity-list #'> :key #'cadr))
    (dotimes (i k)
      (let ((pair (pop similarity-list)))
	(push (car pair) ranked-list)))
    (return-from find-knn ranked-list)))

;;; TASK 2A ;;;
;; Utility function used for a line or sentence
(defun trim-parenthesis (word)
  (string-trim '(#\Space #\Tab #\Newline #\( #\) )
  	       (string-downcase word)))

;; This function takes a vs-struct and a file as parameteres and iterates through each
;; line in the file (which contains the training data). For each line the line is trimmed
;; by removing whitespaces and parenthesis. If the trimmed lines first character is a colon(:)
;; its a class and the line/word is used as a new key in the vs-class hash. Its value is set
;; to be an empty list. If a line/word is not blank, and it doesnt start with a colon (:), its
;; regarded as a member of the current class and it is appended to the currents class' list.

(defun read-classes (vs-struct trainingdata)
  (let ((file-stream (open trainingdata))
	(current-key nil))
    (loop
       for line = (read-line file-stream nil)
       while line
       do (let ((line (trim-parenthesis line)))
	   (if (not (equal (trim-parenthesis line) "")) 
	   (if (equal (aref (trim-parenthesis line) 0) #\:)
	       (progn
		 (setf current-key line)
		 (setf (gethash line (vs-classes vs-struct)) (list)))
	       (push line (gethash current-key (vs-classes vs-struct)))))))))

;;; TASK 2B ;;;
;; This function takes a vs-struct and iterates through all the keys in the vs-class hash
;; (i.e the classes). If the class is not the "unknown-class" it creates a centroid variable
;; (a hash) and iterates through the words in that class. For each word in the class, the
;; feature vector is retrieved and its values are assigned to the centroid. After all the
;; feature vectors and its values are summed up in the centroid, the centroid is divided by
;; its cardinality and stored in the vs-matrix with its class name as key (e.g. :title).

(defun compute-class-centroids (vs-struct)
  (maphash (lambda (key value)
	     (if (not (equal key ":unknown"))
	     (let ((centroid (make-hash-table :test #'equal))
		   (wordcount (length value)))
	       (dolist (word value)
		 (maphash (lambda (key2 value2)
      			    (incf (gethash key2 centroid 0) value2))
			  (get-feature-vector vs-struct word)))
	       (maphash (lambda (key2 value2)
			  (setf (gethash key2 centroid) (/ value2 wordcount)))
			centroid)
	       (setf (gethash key (vs-matrix vs-struct)) centroid))))
	     (vs-classes vs-struct)))

;;; TASK 2C ;;;
;; This function takes a vs-struct and iterates through all the words in the class ":unknown",
;; which is a key in the vs-classes hash. It then iterates through all the centroids in the
;; vs-matrix and computes the proximity between the word and the centroid. The value and
;; centroid name (class name) is stored if the value is the highest yet. When all the centroids
;; are checked, the function removes the word from the class ":unknown" and assigns it to the
;; class which had the nearest centroid and prints out the word, the class it was assign to
;; and the proximity of those two.

(defun rocchio-classify (vs-struct)
  (dolist (word (gethash ":unknown" (vs-classes vs-struct)))
    (let ((max-similarity-score 0)
	  (max-similarity-centroid ""))
      (maphash (lambda (key value)
	       (declare (ignore value))
	       (if (not (equal key ":unknown"))
		   (if (> (get-proximities vs-struct word key) max-similarity-score)
		       (progn
			 (setf max-similarity-score (get-proximities vs-struct word key))
			 (setf max-similarity-centroid key)))))
	       (vs-classes vs-struct))
      	   (pop (gethash ":unknown" (vs-classes vs-struct)))
      	   (push word (gethash max-similarity-centroid (vs-classes vs-struct)))
      (format t "Assigned ~S to ~S. Similarit is ~S ~C"
	      word max-similarity-centroid max-similarity-score #\newline))))

;;; TASK 2D ;;;;

;; In this program centroid are treated as word, in the sense that we use the same data type
;; for the centroids as we do for the feature vectors. As centroid also are vectors, and
;; has to be of the same type as our feature vectors to be calculated with our existing
;; functions, the argument for a two level hash as our data type for vectors is still valid.
;; Hash tables can have some preformance issues as it has to rehash a lot if we require more
;; space than our initial size, so an option would be to use other data structures such as
;; an assosiative list for our centroids, as both avoid sparseness (empty slots) but this
;; would require to redo or add more on our existing implemetation.

;; In this program we also
;; input the centroids into our vs-matrix (our feature-vector space) for conveniance as it
;; simplifies some operations and enables us to use our existing functions without modification
;; (using centroids as words). This is probably not the wisest of choices however, since
;; centroids are abstract 'words' that is only the average of its members. If we were to
;; extend the program by e.g. adding more words from the vs-matrix into one class
;; 'automagically' we would run the risk of accidentally classifying one class inside
;; another (e.g setting ':instiutuion' to be a member of class ':place_name'. 
 

;;; TASK 3A ;;;

;; Two types of commonly used classification algorithms is the rocchio classification and
;; the kNN classification. These have some similarities, but a lot of differences as well.
;; Both of these algorithms is based on supervised learning: They are provided test data
;; ('The gold standard'), that defines the classes and defines some members of the classes.
;; Rocchio classification uses this training data to construct centroids, a centerpoint of
;; the class, which is an abstract normalized vector with the average values of its class
;; members. The centroid construct is therefore the same as the other objects. When we try
;; to classify an object in rocchio, we ofter use 'hard classes' and classifies the given
;; object by finding the closest centroid and assigning the object to the class which that
;; centroid represent. This results in linear decision boundaries where the distance from a
;; given centroid to a given object is the only thing that matters
;; (at least in 'hard classification'). Rocchio classifier is thereby only really suited
;; for linearly seperable classes and does not work well for classes where a single prototype
;; or center cant accuratly represent the class. In addition to this, the Rocchio classifier
;; ignores the local distribution of members and implicitly assumes that classes are spheres
;; with similar radiuses.

;; The kNN classifications differs from Rocchio classification in that is doesnt define any
;; centroids (or metroids) by its training data. Instead kNN classification assigns classes
;; to the objects defined by the training data and then classifies others objects by looking
;; at the class(es) of its nearest (object) neighbours. If e.g k = 30, the kNN looks at the
;; 30 closest neighbours and their classes and assigns the object to the most occurring class
;; among the 30 neighbours. This can also typically be done 'soft', as opposed to
;; 'hard (classes)' by assigning it 'degrees of several classes', e.g object A is 0.60
;; class X and 0.30 class Y. Thus the classes are determined locally
;; (just in regards to nearest neighbours), which results in non-linear decision boundaries,
;; which is more suited for a non-linear disribution than e.g. the Rocchio classification.
;; That being said, kNN classification is in some ways 'dumber' than e.g. Rocchio
;; classification in that is does not have any concept of a class other than that some
;; objects (the training data) belonging to a class and assigning the nearest object of
;; that classified object to the same class, while Rocchio 'constructs' a
;; class (i.e a centroid), based of its members.

;;; TASK 3B ;;; 
