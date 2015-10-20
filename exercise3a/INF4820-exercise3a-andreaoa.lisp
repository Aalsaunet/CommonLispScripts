;;;; INF4820 - exercise3a by Andreas Oven Aalsaunet, andreaoa.
;;;; CL environment used: SBCL

;;; TASK 1A
;; I.

;; We can solve this by counting the occurrance of tags and grouping them (i.e we
;; got 4 RB, 1 NNP, 1 POS, 1 NN, 1 VBZ, 1 VBG, 1 VBN, 2 ',' and 1 '.'. For each of the
;; group the denominator will we the occurence count and the numinator will be the
;; number of times the word is preceeded with a RB tags. This gives us:

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


			      
									  

									  
