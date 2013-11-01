(defun my-entropy (values)
  "Calculates shannon entropy for a list of values."
  (let ((frequency-table (make-hash-table :test 'equal))
	(entropy 0)
	(n 0)
	(current nil))

    (loop for char in values do ;essentially for(char c : input) in lisp
	 (setf current (gethash char frequency-table))
	 (if current
	     (setf (gethash char frequency-table) (+ 1 current))
	     (setf (gethash char frequency-table) 1))
	 (incf n))
    
    (loop for key being the hash-keys of frequency-table do
	 (let ((p (/ (gethash key frequency-table) n)))
	   (incf entropy (* p (log p 2)))))
    (abs entropy)))

(defun valuelist-from-hash-of-hashes (hash value)
  (let ((accumulator '()))
    (loop for member being the hash-values of hash do
	 (setf accumulator (cons (gethash value member) accumulator)))
    accumulator))

(defparameter *sample-hash* nil)
(defun reset-sample-hash ()
  (setf *sample-hash* (make-hash-table :test 'equal))
  (loop for i from 1 to 20 do
       (let ((baby (make-hash-table :test 'equal)))
	 (setf (gethash "value" baby) (* i 7))
	 (setf (gethash i *sample-hash*) baby)))
  (loop for i in '(3 5 7 9 11 13 17 19) do
       (setf (gethash "value" (gethash i *sample-hash*)) 1000))
  *sample-hash*)

