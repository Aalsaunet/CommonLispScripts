;;;; INF4820 - exercise3a by Andreas Oven Aalsaunet, andreaoa.
;;;; CL environment used: SBCL

;;; TASK 1A
;; I.

;; We can solve these transition probabilities by using the formula
;; P(ti|ti-1) = C(ti-1|ti)/C(ti-1) or by counting the occurrance of tags and grouping
;; them (i.e we got 4 RB, 1 NNP, 1 POS, 1 NN, 1 VBZ, 1 VBG, 1 VBN, 2 ',' and 1 '.'.
;; For each of the groups the denominator will we the occurence count and the numinator
;; will be the number of times the word is preceeded with a RB tags. This gives us:

;; P(PB|PB) = 1/4 = 0.25, P(NNP|PB) = 0, P(POS|RB) = 0, P(NN|RB) = 0, P(VBZ|RB) = 0,
;; P(VBG|RB) = 0, P(VBN|RB) = 0, P(','|RB) = 2/2 = 1/1 = 1, P('.'|RB) = 1/1 = 1.

;; II.
;; The emission probabilty of a certain word to a certain tag, e.g. P(move|NNP), is the
;; probability of a word having that tag. We can calulate this using the formula:
;; P(w|t) = C(t|w)/C(t). This gives us:

;; P(move|NNP) = 0/1 = 0
;; P(move|NN)  = 1/1 = 1
;; P(well|RB)  = 1/4 = 0.25

;;; TASK 1B
;; The problem with Maximum Likelihood Estimation (MLE) of N-grams is data sparseness.
;; Even though we have a perfectly acceptable N-gram this might not have been observed in our
;; data, and the likelihood of that N-gram to occur is thus 0. We can remedy this by something
;; called 'smoothing'. This involves reassigning some of the probability mass of frequent
;; events to less or non-occurring events. One of the more common ways to do this is Laplace
;; smoothing. Why smoothing is importaint is because it compensates for a lacking complexity
;; or richness in our training data, since many acceptable combinations will not be present.
;; If we observe how the probability of a sentence is calculated we also see that if one or
;; more words isnt represented in our training data, the probability of that N-gram will
;; be 0, and thus the probability for the whole sentence will be 0.

;; After applying Laplace smoothing our transition probabilities now look like this:
;; P(PB|PB) = (1+1)/(4+9)  = 2/13 = 0.154
;; P(NNP|PB) = 1/(4+9)     = 1/13 = 0.077
;; P(POS|RB) = 1/(4+9)     = 1/13 = 0.077
;; P(NN|RB)  = 1/(4+9)     = 1/13 = 0.077
;; P(VBZ|RB) = 1/(4+9)     = 1/13 = 0.077
;; P(VBG|RB) = 1/(4+9)     = 1/13 = 0.077
;; P(VBN|RB) = 1/(4+9)     = 1/13 = 0.077
;; P(','|RB) = (2+1)/(4+9) = 3/13 = 0.231
;; P('.'|RB) = (1+1)/(4+9) = 2/13 = 0.154

;; The probability of the tag bi-gram <NN,POS>, i.e. a common noun followed by a
;; possessive marker is 0.1 according to the Laplace formula:
;; PL(POS, NN) = C(NN, POS)/(NN + V) = (0 + 1)/(1 + 9) = 1/10 = 0.1

;;; TASK 1C ;;;
;; In Part-of-speech tagging we usually want to find the state sequence that maximizes
;; the P(S|O). The problem that quickly arises from this is that for N observations 
;; with L states there are L^N sequences. This means that if we have 10 observations 
;; with 5 states we end up with 9 765 625 (5^10) sequences, which already is pretty 
;; computationally demanding. The Viterbi algorithm is a dynamic programming algorithm 
;; which stores previous results for reuse so the program don't have to solve the same 
;; partial calculations over and over again. This is done by recording and storing 
;; backpointers that show which previous state led to the maximum probability. This 
;; enables our program to go 'backwards' from the end of our sentence/sequence, after 
;; computing every transition probability, by following the backpoints and thus more 
;; easily find the maximum probability sequence.

;; In the Viterbi Algorithm we create two matrices (trellises), one path probability
;; matrix and one path backpointer matrix, both with the dimensions [N, L + 2]. The first
;; one is responsible for for storing the probability for moving from one observation Oi to
;; observation Oi+1 with state Si. This means that one row of the matrix holds one observation
;; with all its possible states and the cell values is the probabilty of that observation
;; 'moving' to the state represented by its column. An example is that index [1, 1] holds
;; the probability of observation 1 to transition to observation 2, state 1, while [1, 3] 
;; holds the probability of observation 1 to transition to observation 2, state 3.

;; The second matrix, the path backpointer matrix, is responsible for storing backpointers.
;; This is done by looking back at observation Oi-1 and determining which previous observation
;; is most likely to 'arrive from'. If we figure out that the most probable path from O1 to O2
;; is by moving from S1 to S2. A backpointer from [O2, S2] to [O1, S1]. Backpointer matrices
;; can typically be sparse as [Oi-1, S1] might be best for all [Oi, Sj] and backpointer is
;; therefore only stores for that path. When we have reached the end of our sequence we will
;; only have one backpointer to the previous observation, and by following the backpointers
;; we get the unambigious highest probability route (i.e the maximum probability state
;; sequence).

;; The Big-O notations dictates the complexity of the algorithms with the formula O(L²N)
;; This means that if we have three observations (N = 3) and two states (L = 2) we have
;; 2²*3 = 12 in the worst-case order of growth (i.e in the worst-case-scenario we have to
;; iterate/compute 12 times).

;; I.: In regards to the length of the input sequence (i.e the number of observations = N) this
;;     increases the Big-O in a linear fashion.

;; II.: In regards to the size of the tag set (i.e the number of states = L) this increases
;;      square growth rate.
									  
;;; TASK 2A ;;;
;; States is a hash table with the a state name/tag as key and index as a value

;; n is the number of states

;; transitions is a 2 dimentional array (matrix) with size [n + 1, n + 1]. The extra row
;; and column slot holds the name of the tags and the intersection of these holds the
;; transition probability.

;; emision is an array where each index correspond to the index of a state in the states
;; hash. Each of the elements in the array holds a hash table with key = observation
;; and value = emission probability of that state-word-pair

;; (defstruct hmm
;;   (states (make-hash-table :test #'equal))
;;   (n 1)
;;   (transitions (make-array (list n n) :initial-element 0))
;;   (emissions (make-array n)))

(defstruct hmm states n transitions emissions next-available-state-index)

;;; TASK 2B ;;;
(defun transition-probability (hmm stateId1 stateId2)
  (aref (hmm-transitions hmm) stateId1 stateId2))

(defun emission-probability (hmm stateId word)
  (gethash word (aref (hmm-emissions hmm) stateId)))

(defun state2id (hmm state-label)
  (let ((index (gethash state-label (hmm-states hmm))))
  (if index
      (return-from state2id index)
      (progn
	(setf (gethash state-label (hmm-states hmm)) (hmm-next-available-state-index hmm))
	(incf (hmm-next-available-state-index hmm))
	(return-from state2id (gethash state-label (hmm-states hmm)))))))

;;; TASK 3A ;;;
(defun create-hmm (state-count)
  (make-hmm :states (make-hash-table :test #'equal)
	    :n state-count
	    :transitions (make-array (list state-count state-count) :initial-element 0)
	    :emissions (make-array state-count :initial-element 0)
	    :next-available-state-index 0))

;; Tokenizes the the sentence given (split sentence into word list) and returns it.
(defun tokenize (string)
  (loop
     for start = 0 then (+ space 1)
     for space = (position #\Tab string :start start)
     for token = (subseq string start space)
     unless (string= token "") collect token
     until (not space)))



(defun read-corpus (corpus state-count)
  (let ((hmm (create-hmm (+ state-count 2)))
	(file-stream (open corpus))
	(previous-state "<s>"))
    (state2id hmm "<s>")
    (state2id hmm "</s>")
    (loop
       for line = (read-line file-stream nil)
       while line
       do (let ((tokens (tokenize line)))
	    (if (not (car tokens))
		(progn
		  (incf (aref (hmm-transitions hmm) (state2id hmm previous-state)
			      (state2id hmm "</s>")))
		  (setf previous-state "<s>")
		  )
		(progn
		  (incf (aref (hmm-transitions hmm) (state2id hmm previous-state)
			      (state2id hmm (cadr tokens))))
		  (setf previous-state (cadr tokens))		  
		  (if (equal (aref (hmm-emissions hmm) (state2id hmm (cadr tokens))) 0)
		      (setf (aref (hmm-emissions hmm) (state2id hmm (cadr tokens)))
			    (make-hash-table :test #'equal)))
		  (incf (gethash (car tokens) (aref (hmm-emissions hmm)
						    (state2id hmm (cadr tokens))) 0))))))
    (incf (aref (hmm-transitions hmm) (state2id hmm previous-state) (state2id hmm "</s>")))
    (return-from read-corpus hmm)))

;;; TASK 3B ;;;

(defun find-state-count (x hmm)
  (let ((sum 0))
    (dotimes (y (hmm-n hmm))
      (setf sum (+ sum (aref (hmm-transitions hmm) x y))))
    (print sum)
    (return-from find-state-count sum)))

(defun train-hmm (hmm)
  (dotimes (x (hmm-n hmm))
    (dotimes (y (hmm-n hmm))
      (let ((denominator (find-state-count x hmm)))
	(if (not (equal denominator 0))
	    (setf (aref (hmm-transitions hmm) x y)
		  (/ (aref (hmm-transitions hmm) x y) denominator)))))))
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Method for printing out hash keys and values
(defun print-hash-entry (key value)
  (format t "The value associated with the key ~S is ~S~%" key value))

;; For testing purposes:
;;(maphash #'print-hash-entry (hmm-states eisner))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
