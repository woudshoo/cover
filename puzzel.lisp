(defpackage #:puzzel
  (:use #:cl #:cover))
  
(in-package :puzzel)

;;;
;;;
;;; PUZZLE PIECES

;;;   #####
;;;
;;;   ####       #
;;;   #          ####
;;;
;;;   ####        #
;;;    #         ####
;;;
;;;   #
;;;   #
;;;   ###
;;;
;;;    #
;;;    #
;;;   ###
;;;
;;;   ##      ###    
;;;   ###     ##
;;;
;;;   
;;;   # #
;;;   ###
;;;
;;;   #         #
;;;   ###     ###
;;;    #       #
;;;
;;;  #         #
;;;  ###     ###
;;;    #     #
;;;
;;;    #  
;;;   ###
;;;    #
;;;
;;;  #
;;;  ##
;;;   ##
;;;
;;; 11x5 == 55
;;;
;;; Total:  16  stukjes, 80 plaatsen in 8x10?
;;;
;;; 
;;;
;;; syntax:
;;;
;;;  (create-bord 8 10)
;;;
;;;  (add-piece "
;;;  

(defclass board ()
  ((width :accessor width :initarg :width)
   (height :accessor height :initarg :height)))

(defclass point ()
  ((x :accessor x :initarg :x)
   (y :accessor y :initarg :y)))
  
(defclass piece ()
  ((name :accessor name :initarg :name)
   (squares :accessor squares :initarg :squares)))
  
(defgeneric name (obj))
(defgeneric width (obj))
(defgeneric height (obj))
(defgeneric translate (obj vect))
(defgeneric upper-left (obj))
(defgeneric reflect-around-origin (obj))
(defgeneric move-to-origin (obj))

(defmethod reflect-around-origin ((point point))
  (make-instance 'point :x (- (x point)) :y (- (y point))))

(defmethod move-to-origin ((obj t))
  (translate obj (reflect-around-origin (upper-left obj))))

(defmethod name ((point point))
  (format nil "~A,~A" (x point) (y point)))

(defmethod max-x ((piece piece))
  (loop for point in (squares piece)
       maximize (x point) into max-x
       finally (return max-x)))

(defmethod max-x ((list list))
  (loop for obj in list
       maximize (max-x obj) into max-x
       finally (return max-x)))
(defmethod min-x ((list list))
  (loop for obj in list
       minimize (min-x obj) into min-x
       finally (return min-x)))

(defmethod max-y ((list list))
  (loop for obj in list
       maximize (max-y obj) into max-y
       finally (return max-y)))

(defmethod min-y ((list list))
  (loop for obj in list
       minimize (min-y obj) into min-y
       finally (return min-y)))

(defmethod max-y ((piece piece))
  (loop for point in (squares piece)
       maximize (y point) into max-y
       finally (return max-y)))

(defmethod min-x ((piece piece))
  (loop for point in (squares piece)
       minimize (x point) into min-x
       finally (return min-x)))

(defmethod min-y ((piece piece))
  (loop for point in (squares piece)
       minimize (y point) into min-y
       finally (return min-y)))


(defmethod width ((piece piece))
  (loop for point in (squares piece)
       maximize (x point) into max-x
       minimize (x point) into min-x
       finally (return (1+ (- max-x min-x)))))

(defmethod height ((piece piece))
  (loop for point in (squares piece)
       maximize (y point) into max-y
       minimize (y point) into min-y
       finally (return (1+ (- max-y min-y)))))

(defmethod translate ((point point) (vect point))
  (make-instance 'point 
		 :x  (+ (x point) (x vect))
		 :y  (+ (y point) (y vect))))

(defmethod translate ((point point) (vect list))
  (make-instance 'point 
		 :x (+ (x point) (first vect))
		 :y (+ (y point) (second vect))))

(defmethod translate ((piece piece) (vect t))
  (make-instance 'piece 
		 :name (name piece)
		 :squares (mapcar #'(lambda (p) (translate p vect)) (squares piece))))

(defmethod upper-left ((piece piece))
  (loop for point in (squares piece)
       minimize (x point) into min-x
       minimize (y point) into min-y
       finally (return (make-instance 'point :x min-x :y min-y))))
  

(defmethod contains-point ((point-a point) (point-b point))
  (and (= (x point-a) (x point-b)) (= (y point-a) (y point-b))))

(defmethod contains-point ((point-a point) (point-b list))
  (and (= (x point-a) (first point-b)) (= (y point-a) (second point-b))))

(defmethod contains-point ((piece piece) (point t))
  (loop for p in (squares piece)
     when (contains-point p point) return t))



(defun create-squares-from-strings (&rest rest)
  (loop 
     for row in rest 
     for y from 0
     append
       (loop 
	  for c across row
	  for x from 0
	  when (char= c #\#) collect (make-instance 'point :x x :y y))))
     
(defun print-piece (piece)
  (format t "Name:    ~A~%Squares: ~A~%" (name piece) (mapcar #'name (squares piece))))

(defun print-piece-2 (piece)
  (print-piece piece)
  (let ((origin-piece (move-to-origin piece)))
    (loop for y from 0 upto (height piece)
       do
	 (loop for x from 0 upto (width piece)
	      do
	      (format t (if (contains-point piece (list x y)) "#" " ")))
	 (format t "~%"))))

(defun create-piece-with-name (name &rest rest)
  (make-instance 'piece :name name :squares (apply #'create-squares-from-strings rest)))


(defun add-board (problem board)
  (loop for x from 0 below (width board)
       do
       (loop for y from 0 below (height board)
	  do
	    (cover:add-column problem (name (make-instance 'point :x x :y y))))))


(defun add-piece (problem piece count)
  (add-specified-row problem (format nil "~A~A" (name piece) count) piece (cons (name piece) (mapcar #'name (squares piece)))))


(defmethod transform ((point point) (transform-matrix list))
  (make-instance 'point 
		 :x (+ (* (first transform-matrix) (x point))
		       (* (second transform-matrix) (y point)))
		 :y (+ (* (third transform-matrix) (x point))
		       (* (fourth transform-matrix) (y point)))))

(defmethod transform ((piece piece) (transform-matrix list))
  (make-instance 'piece :name (name piece)
		 :squares (mapcar #'(lambda (x) (transform x transform-matrix)) (squares piece))))

(defun all-rotations-and-mirrors-for-piece (piece)
  (let ((pieces 
	 (mapcar #'(lambda (transform) (transform piece transform)) 
		 '((1 0 0 1)  ;; identit
		   (0 1 -1 0)  ;; rotation 90 clockwise
		   (0 -1 1 0) ;; rotation 90 counter clockwise
		   (-1 0 0 -1) ;; roation 180 degrees
		   
		   (-1 0 0 1) ;; flip horizontal axis  
		   (1 0 0 -1) ;; flip vertical axis
		   (0 -1 -1 0) ;; 
		   (0 1 1 0)))))
    (setf pieces (mapcar #'move-to-origin pieces))
    pieces))
    
(defun pieces-are-same (piece-a piece-b)
  "Return true if the two pieces are the same"
  (let ((squares-a (squares piece-a))
	(squares-b (squares piece-b)))
    (and (= (length squares-a) (length squares-b))
	 (reduce #'(lambda (x y) (and x y)) squares-b :key (lambda (x)  (contains-point piece-a x)) :initial-value t))))

(defun rotations-and-mirrors-for-piece-reduced (piece)
  (let ((pieces (all-rotations-and-mirrors-for-piece piece))
	(result (list)))
    (loop for p in pieces
       do
	 (unless (member p result :test #'pieces-are-same)
	   (push p result)))
    result))

  

(defun add-piece-for-board (problem piece board)
  (let ((pieces-list (rotations-and-mirrors-for-piece-reduced piece)))
    (loop for p in pieces-list
       do
       (add-piece-for-board-with-translations problem p board))))


(defun add-piece-for-board-with-translations (problem piece board)
  (let ((origin-piece (move-to-origin piece))
	(count 0))
    (loop for x from 0 upto (- (width board) (width piece))
       do
	 (loop for y from 0 upto (- (height board) (height piece))
	    do
	      (incf count)
	      (add-piece problem (translate origin-piece (list x y)) count)))))

(setf u-piece
      (create-piece-with-name "U"
			      "#.#"
			      "###"))
(setf l-piece
      (create-piece-with-name "L" "##" "#."))


(setf p-piece
      (create-piece-with-name "P" "#"))


(defun pieces-from-solution (solution)
  (mapcar (lambda (x) (cover::extra-data (cover::row x))) solution))

(defun piece-containing (x y list-of-pieces)
  (find-if (lambda (piece) (contains-point piece (list x y))) list-of-pieces))


(defun print-solution (vect)
  (let* ((list-of-pieces (pieces-from-solution vect))
	 (start-x (min-x list-of-pieces))
	 (end-x (max-x list-of-pieces))
	 (start-y (min-y list-of-pieces))
	 (end-y (max-y list-of-pieces)))
    (loop for y from start-y upto (1+ end-y)
       do
	 (loop for x from start-x upto (1+ end-x)
	    do
	      ;; print top row of piece
	      (format t 
		      (if (eq (piece-containing x y list-of-pieces) 
			      (piece-containing x (1- y) list-of-pieces))
			  "   " "---")))
	 (format t "~%")
	 (loop for x from start-x upto (1+ end-x)
	    do
	      (format t
		      (if (eq (piece-containing x y list-of-pieces)
			      (piece-containing (1- x) y list-of-pieces))
			  "   " "|  ")))
	      (format t "~%")
	      ;; print left hand side of piece
	 )))
	      

    

(defun create-test-puzzel ()
  (let ((puzzel (cover:create-problem))
	(u-piece       (create-piece-with-name "U"
			      "#.#"
			      "###"))
	(t-piece       (create-piece-with-name "T"
					       "###"
					       ".#."
					       ".#."))
	(l-piece       (create-piece-with-name "L" 
					       "##" 
					       "#."))
	(p-piece       (create-piece-with-name "P" "#"))
	(I-piece       (create-piece-with-name "I" "###"))
	(board         (make-instance 'board :width 3 :height 4)))
    (add-piece-for-board puzzel u-piece board)
    (add-piece-for-board puzzel l-piece board)
    (add-piece-for-board puzzel p-piece board)
    (add-piece-for-board puzzel I-piece board)
    puzzel))
    


;; Pentomino pieces

(defvar F-piece (create-piece-with-name "F" " ##" "##" " #"))
(defvar I-piece (create-piece-with-name "I" "#####"))
(defvar L-piece (create-piece-with-name "L" "####" "#"))
(defvar N-piece (create-piece-with-name "N" " ###" "##"))
(defvar P-piece (create-piece-with-name "P" "###" "##"))
(defvar T-piece (create-piece-with-name "T" "###" " #" " #"))
(defvar U-piece (create-piece-with-name "U" "# #" "###"))
(defvar V-piece (create-piece-with-name "V" "###" "#" "#"))
(defvar W-piece (create-piece-with-name "W" "#" "##" " ##"))
(defvar X-piece (create-piece-with-name "X" " #" "###" " #"))
(defvar Y-piece (create-piece-with-name "Y" "#" "##" "#" "#"))
(defvar Z-piece (create-piece-with-name "Z" " #" "##" " #" " ##"))


(defun create-pentomino-puzel (length)
  (let ((puzzel (cover:create-problem))
	(board (make-instance 'board :width length :height (/ 60 length))))
    (add-piece-for-board puzzel F-piece board)
    (add-piece-for-board puzzel I-piece board)
    (add-piece-for-board puzzel L-piece board)
    (add-piece-for-board puzzel N-piece board)
    (add-piece-for-board puzzel P-piece board)
    (add-piece-for-board puzzel T-piece board)
    (add-piece-for-board puzzel U-piece board)
    (add-piece-for-board puzzel V-piece board)
    (add-piece-for-board puzzel W-piece board)
    (add-piece-for-board puzzel X-piece board)
    (add-piece-for-board puzzel Y-piece board)
    (add-piece-for-board puzzel Z-piece board)
    puzzel))

(setq F-piece (create-piece-with-name "F" " ##" "##" " #"))
(setq I-piece (create-piece-with-name "I" "#####"))
(setq L-piece (create-piece-with-name "L" "####" "#"))
(setq N-piece (create-piece-with-name "N" " ###" "##"))
(setq P-piece (create-piece-with-name "P" "###" "##"))
(setq T-piece (create-piece-with-name "T" "###" " #" " #"))
(setq U-piece (create-piece-with-name "U" "# #" "###"))
(setq V-piece (create-piece-with-name "V" "###" "#" "#"))
(setq W-piece (create-piece-with-name "W" "#" "##" " ##"))
(setq X-piece (create-piece-with-name "X" " #" "###" " #"))
(setq Y-piece (create-piece-with-name "Y" "#" "##" "#" "#"))
(setq Z-piece (create-piece-with-name "Z" " #" "##" " #" " ##"))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(defun create-test-2-puzzel ()
;  (let ((puzzel (cover:create-problem))
;	(board  (make-instance 'board :width :height)))

