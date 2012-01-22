(defpackage #:nonogram
  (:use #:cl #:cover))

(in-package #:nonogram)


(defun col-name-for-color-cell (x y colour row-p)
  (format nil "C~A,~A-Colour-~A-~A"
	  x y colour (if row-p "ROW" "COL")))

(defun col-name-for-cell (x y)
  (format nil "C~A,~A" x y))

(defun col-name-for-hint (x-or-y row-p)
  (format nil "~A-~A" (if row-p "ROW" "COL") x-or-y))


(defun add-cell-color-rows (width height colours problem)
  (loop :for x :from 1 :upto width
     :do
     (loop :for y :from 1 :upto height
	:do
	(loop :for colour :in colours :do
	   (cover:add-specified-row problem (format nil "C~A,~A-Colour-~A" x y colour) (list x y colour)
				    (cons (col-name-for-cell x y)
					  (loop :for c :in colours
					     :unless (eq c colour) :collect (col-name-for-color-cell x y c t)
					     :unless (eq c colour) :collect (col-name-for-color-cell x y c nil))))))))


(defun expand-hint (hint max-length)
  "Returns a list of expansions, each expansion is list of
pairs:  (location . (color . count))"
  (labels ((expand-hint (sub-hint start)
	     (if sub-hint
		 (loop 
		    :with ssub-hint = (cdr sub-hint)
		    :with current-hint = (car sub-hint)
		    :with eq-next = (and ssub-hint (eq (car current-hint) (caar ssub-hint)))
		    :with current-length = (+ (cdr current-hint) (if eq-next
								     1 0))
		    :for index :from start :upto (1+ (- max-length current-length))
		    :unless ssub-hint :collect (list (cons start (cons 'E (- index start))) 
						     (cons index current-hint)
						     (cons (+ index (cdr current-hint)) 
							   (cons 'E (1+ (- max-length index (cdr current-hint))))))
		    :when ssub-hint :append
		    (loop :for sr :in (expand-hint ssub-hint 
						   (+ index current-length))
		       :collect 
		       (cons (cons start (cons 'E (- index start)))
			     (cons (cons index current-hint) 
				   (cons (cons (+ index (cdr current-hint)) (cons 'E (if eq-next 1 0))) sr))))))))
    (expand-hint hint 1)))


(defun add-row-rows-for-hint (hint width row-nr problem)
  (loop :for row :in (expand-hint hint width)
     :for index :from 1
     :for cover-row = (cover:add-row problem (format nil "ROW-~A-~A" row-nr index) nil)
     :do
     (cover:add-cell problem cover-row 
		     (cover:find-or-insert-column problem (col-name-for-hint row-nr t)))
     (loop :for (location colour . length) :in row
	:do
	(loop :for x :from location :repeat length :do
	   (cover:add-cell problem cover-row 
			   (cover:find-or-insert-column problem (col-name-for-color-cell x row-nr colour t)))))))

(defun add-col-rows-for-hint (hint width col-nr problem)
  (loop :for row :in (expand-hint hint width)
     :for index :from 1
     :for cover-row = (cover:add-row problem (format nil "COL-~A-~A" col-nr index) nil)
     :do
     (cover:add-cell problem cover-row 
		     (cover:find-or-insert-column problem (col-name-for-hint col-nr nil)))
     (loop :for (location colour . length) :in row
	:do
	(loop :for y :from location :repeat length :do
	   (cover:add-cell problem cover-row
			   (cover:find-or-insert-column problem (col-name-for-color-cell col-nr y colour nil)))))))

  
(defun make-nonogram-problem (row-hints col-hints width height colours)
  (let ((problem (cover:create-problem)))
    (add-cell-color-rows width height colours problem)
    (loop :for row-hint :in row-hints 
       :for row-nr :from 1
       :do
       (add-row-rows-for-hint row-hint width row-nr problem))
    (loop :for col-hint :in col-hints
       :for col-nr :from 1
       :do
       (add-col-rows-for-hint col-hint height col-nr problem))
    problem))


(defun make-test-problem ()
  (make-nonogram-problem
   '(((X . 1) (X . 2) (Y . 2) (Y . 1))
     ((Y . 1) (Y . 1) (X . 1) (Z . 1))
     ((Y . 1) (Z . 4)))
   '(((X . 1) (Y . 2))
     ()
     ((Z . 1))
     ((X . 1) (Y . 1) (Z . 1))
     ( (X . 2) (Z . 1))
     ((Y . 1) (Z . 2))
     ((Y . 1))
     ()
     ((Y . 1))) 9 3 '(E X Y Z)))

(defun make-test-problem-2 ()
  (make-nonogram-problem
   '(((X . 1))
     ((Y . 1)))
   '(((X . 1))
     ((Y . 1))) 2 2 '(E X Y)))

(defun make-test-problem-3 ()
  (make-nonogram-problem
   '(((X . 1)))
   '(((X . 1))) 1 1 '(E X)))


(defun make-test-problem-4 ()
  (make-nonogram-problem
   '(((X . 2))
     (( X . 4)( X . 2))
     (( X . 1)( X . 1)( X . 4))
     (( X . 1)( X . 1)( X . 1)( X . 1))
     (( X . 1)( X . 1)( X . 1)( X . 1))
     (( X . 1)( X . 1)( X . 1)( X . 1))
     (( X . 1)( X . 1)( X . 1)( X . 1))
     (( X . 1)( X . 1)( X . 1)( X . 1))
     (( X . 1)( X . 2)( X . 2)( X . 1))
     (( X . 1)( X . 3)( X . 1))
     (( X . 2)( X . 1))
     (( X . 1)( X . 1)( X . 1)( X . 2))
     (( X . 2)( X . 1)( X . 1)( X . 1))
     (( X . 1)( X . 2))
     (( X . 1)( X . 2)( X . 1)))
   '((( X . 3))
     (( X . 3))
     (( X . 10))
     (( X . 2))
     (( X . 2))
     (( X . 8)( X . 2))
     (( X . 2))
     (( X . 1)( X . 2)( X . 1))
     (( X . 2)( X . 1))
     (( X . 7))
     (( X . 2))
     (( X . 2))
     (( X . 10))
     (( X . 3))
     (( X . 2))) 15 15 '(X E)))