;;; Task 2A

(defstruct (vs)
  (matrix (make-hash-table :test #'equal))
  similarity-fn)

(defparameter vs-instance (make-vs)) 

(defun read-words-to-hash (words)
  (let ((file-stream (open words)))
    (loop
       for line = (read-line file-stream nil)
       while line
       do (setf (gethash line (vs-matrix vs-instance)) (make-hash-table :test #'equal))))
  )

(defun read-corpus-to-vs (words)
  (read-words-to-hash words)
  )
