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


			      
									  

									  
