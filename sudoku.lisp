(defpackage #:sudoku
  (:use #:cl #:cover))

(in-package :sudoku)


;; 
;; Columns:
;; 
;;   Fields :                81 columns
;;   has nr in constraint:   9 * (9 + 9 + 9) = 243    
;;   total:              324
;; 

(defun name-for-fow (nr x y)
  (format nil "~A at ~A,~A" nr x y))

(defun list-of-columns (nr x y)
  "Create a list of columns for this 
"
  (list 
   (format nil "~A,~A" x y)
   (format nil "~A in R~A" nr y)
   (format nil "~A in C~A" nr x)
   (format nil "~A in B~A~A" nr (floor (1- x) 3) (floor (1- y) 3))))

(defun create-empty-sudoku ()
  (let ((sudoku (cover:create-problem)))
    (loop for nr from 1 upto 9 do 
	 (loop for x from 1 upto 9 do
	      (loop for y from 1 upto 9 do
		   (add-specified-row sudoku
				      (name-for-fow nr x y)
				      (list nr x y)
				      (list-of-columns nr x y)))))
    sudoku))


(defun set-value (sudoku value x y)
  (cover::place-row sudoku (cover::next-column (cover::find-row sudoku (name-for-fow value x y)))))


(defun print-solution (solution &optional start-values)
  (let ((sorted-solution
	 (stable-sort 
	  (sort
	   (append start-values
		   (mapcar #'cover::extra-data solution))
	   #'<
	   :key #'third)
	  #'<
	  :key #'second)))
	   (format t "~% -----------------------~%")
    (loop for y from 1 upto 9
	 do
	 (format t "~&|")
	 (loop for x from 1 upto 9
	      do
	      (let ((value (find (list x y) 
				 sorted-solution 
				 :key #'(lambda (x) (list (second x) (third x)))
				 :test #'equal)))
		(if value
		    (format t " ~A" (first value))
		    (format t " .")))
	      (when (= (mod x 3) 0)  (format t " |")))
	 (when (= (mod y 3) 0) 
	   (format t "~% -----------------------~%")))))
			       
     

(defmethod create-sudoku (lines)
  (let ((sudoku (create-empty-sudoku)))
    (loop 
       for y from 1 upto 9
       for line in lines
       do 
	 (loop
	    for x from 1 upto 9
	    for c across line 
	    do
	      (let ((nr (digit-char-p c)))
		(when nr
		  (set-value sudoku nr x y)))))
    sudoku))
    

(defun create-sudoku-from-line (line)
  (let ((sudoku (create-empty-sudoku))
	(filled-in-values (list)))
    (loop for i from 0 upto 80
       for c across line
	 do
	 (let ((nr (digit-char-p c))
	       (x (1+ (rem i 9)))
	       (y (1+ (floor i 9))))
	   (when (and nr (> nr 0))
	     (push (list nr x y) filled-in-values)
	     (set-value sudoku nr x y))))
    (values sudoku filled-in-values)))
		 
	   
	 

0  1  2  3  4  5  6  7  8
9 10 11 12 13 14 15 16 17
...




