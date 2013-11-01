(load "shannon.lisp") ;this is bad
(load "/home/haakon/programmering/lisp/rtscraper/scraper.lisp") ; this is worse

(defun classification-entropy (filter)
  "Given a dataset (hashmap of hashmaps) of movies and a target function,
This will calculate the classification entropy of the dataset for the id3 algorithm
Currently it just uses *movie-data* though, so only movies in the current set will be evaluated."
  (let ((p (probability filter)))
   (- 
    (* (- p)   (log p 2))
    (* (- 1 p) (log (- 1 p) 2)))))

(defun information-gain (attribute classification previous movies)
  "Given an attribute, it will calculate the information gain of knowing that attribute
Everything here is done in the most stupid way possible, and I mean most stupid way possible."
  (let ((values (make-hash-table :test 'equal)))

    ;; Just grab the values first
    (loop for movie in movies do
	 (setf (gethash (funcall attribute movie) values) '(0 0)))

    values))
    ;; Then count the occurences of each value, and the probability the classification holding under that value
    (loop for movie in movies do
	 (let* ((my-attribute (apply attribute (list movie)))
		(my-attribute-count (cadr  (gethash my-attribute values)))
		(my-attribute-matches (car (gethash my-attribute values))))
;	   (format 't "~a (~a ~a)~%" my-attribute my-attribute-matches my-attribute-count)
	   (if (apply classification (list movie))
	       (incf my-attribute-matches))
	   (incf my-attribute-count)
	   (setf (gethash my-attribute values) (list my-attribute-matches
						     my-attribute-count))))
;    (print-hashmap values)

    ;; Then we do the actual calculation
    (let ((gain previous))
      (loop for value being the hash-keys of values do 
;	   (format 't "~a~%" value)
	   (let* ((yes (car (gethash value values)))
		  (num (cadr (gethash value values)))
		  (no (- num yes))
		  (probability  (/ (car  (gethash value values))
				   (length movies)))
		  (nprobability (- 1 probability))
		  (information (cond ((or (= yes 0) (= no 0)) 0)
				     ((= yes no) 1)
				     ('otherwise (- (* (- nprobability)
						       (log nprobability 2))
						    (* probability
						       (log probability 2)))))))
	     (setf gain
		   (- gain (* probability information)))))
      (cons gain values))))

	 
(defun build-id3-tree (legal-attributes target-attribute)
  "This is a *really* stupid way of doing things
For one, the tree should be a struct or list, depending."
  (let ((tree (make-hash-table :test 'equal))
	(c-entropy (classification-entropy target-attribute))
	(attribute-pairs ()))
    (loop for attribute in legal-attributes do
	 (format 't "~a~%" attribute)
	 (format 't "~a~%" (information-gain attribute target-attribute c-entropy (get-all-movies))))
    attribute-pairs))


(defun classify (classifier movie)
  (funcall classifier movie))

(defun runtime-classifier (movie)
  (let ((runtime (gethash "runtime" movie)))
    (cond ((< runtime 60) "SHORT")
	  ((< runtime 120) "MEDIUM")
	  ((> runtime 120) "LONG"))))

(defun year-classifier (movie)
  (gethash "year" movie))

(defun critic-rating-classifier (movie)
  (let ((rating (gethash "critics_score" (gethash "ratings" movie))))
    (cond ((<= rating 20) "00-20")
	  ((<= rating 40) "21-40")
	  ((<= rating 60) "41-60")
	  ((<= rating 80) "61-80")
	  ('otherwise     "81-100"))))
  
