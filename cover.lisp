(defpackage #:cover
  (:use #:cl)
  (:export #:cover
	   #:problem
	   #:cell
	   #:add-row
	   #:add-column
	   #:add-cell
	   #:create-problem
	   #:add-specified-row
	   #:print-problem
	   #:create-test-problem))

(in-package :cover)


(defmacro do-linked-list-not-first (loop-var start next-function &body body)
  `(do ((,loop-var (,next-function ,start) (,next-function ,loop-var)))
       ((eq ,loop-var ,start) nil)
     ,@body))

(defmacro do-linked-list-all (loop-var start next-function &body body)
  `(progn 
     (let ((,loop-var ,start))
       ,@body)
     (do-linked-list-not-first ,loop-var ,start ,next-function ,@body)))

(defclass cell ()
  ((next-column :accessor next-column)
   (previous-column :accessor previous-column)
   (next-row :accessor next-row)
   (previous-row :accessor previous-row)
   (column-header :accessor column-header))
  (:documentation "Cell in matrix, conaining links to cells to its four existing neighbours and a link up to the columns"))

(defclass header (cell)
  ((name :accessor name))
  (:documentation "Generic header cell for the problem matrix."))

(defclass row-header (header)
  ((extra-data :accessor extra-data)))
  
(defclass column-header (header)
  ((nr-rows :accessor nr-rows :initform 0)))

(defclass problem (cell) 
  ((min-nr-rows :accessor min-nr-rows :initform 9999)))   ;; this is bad, using arbitrary number!!

(define-condition cell-is-not-in-column (error) ())
(define-condition cell-is-not-in-row (error) ())
(define-condition cant-find-row () ())
(define-condition cant-find-column () ())

(defgeneric initialize (thing) 
  (:documentation 
   "Initializes basic slot values, this method should go away but use default values instead 
or use the after method."))
(defgeneric add-row (problem row-name extra-data)  (:documentation "Add an empty row to the problem"))
(defgeneric add-column (problem column-name) (:documentation "Add an empty column to the problem"))
(defgeneric add-cell (problem row col) (:documentation "Adds a cell to the matrix.  This should update the column count!"))
(defgeneric find-column (start-point column-name) (:documentation "Try to find a column starting at start-point"))
(defgeneric find-row (start-point column-name) (:documentation "Try to find a row starting at start-point"))
(defgeneric row (cell) (:documentation "Find the row of the argument"))
(defgeneric column (cell) (:documentation "Find the column of the argument"))
(defgeneric remove-column (problem col) (:documentation "Remove column from problem"))
(defgeneric remove-row (problem col) (:documentation "Remove row from problem"))

(defmethod initialize ((cell cell))
  (setf (next-column cell) cell)
  (setf (previous-column cell) cell)
  (setf (previous-row cell) cell)
  (setf (next-row cell) cell)
  (setf (column-header cell) nil))

(defun insert-cell-vertically-after (new-cell old-cell)
  (setf (next-row new-cell) (next-row old-cell))
  (setf (previous-row new-cell) old-cell)
  (setf (previous-row (next-row old-cell)) new-cell)
  (setf (next-row old-cell) new-cell))

(defun insert-cell-horizontally-after (new-cell old-cell)
  (setf (next-column new-cell) (next-column old-cell))
  (setf (previous-column new-cell) old-cell)
  (setf (previous-column (next-column old-cell)) new-cell)
  (setf (next-column old-cell) new-cell))

(defun remove-horizontally (cell)
  (setf (next-column (previous-column cell)) (next-column cell))
  (setf (previous-column (next-column cell)) (previous-column cell)))

(defun remove-vertically (cell)
  (setf (next-row (previous-row cell)) (next-row cell))
  (setf (previous-row (next-row cell)) (previous-row cell)))

(defun reinsert-horizontally (cell)
  (setf (previous-column (next-column cell)) cell)
  (setf (next-column (previous-column cell)) cell))

(defun reinsert-vertically (cell)
  (setf (previous-row (next-row cell)) cell)
  (setf (next-row (previous-row cell)) cell))

(defmethod add-row ((problem problem) (row-name string) (extra-data t))
  "Adds a new row to problem.
It is not checked if a row with this name already exists."
  (let ((new-row-header (make-instance 'row-header)))
    (initialize new-row-header)
    (setf (extra-data new-row-header) extra-data)
    (setf (name new-row-header) row-name)
    (insert-cell-vertically-after new-row-header problem)))

(defmethod add-column ((problem problem) (col-name string))
  "Adds a new column to problem.
It is not checked if a column with col-name already exists."
  (let ((new-column-header (make-instance 'column-header)))
    (initialize new-column-header)
    (setf (name new-column-header) col-name)
    (insert-cell-horizontally-after new-column-header problem)))

(defmethod find-column ((problem problem) (column-name string))
  (do ((column (next-column problem) (next-column column)))
      ((eq column problem) nil)
    (when (string= (name column) column-name) (return-from find-column column))))

(defmethod find-column ((cell cell) (column-name string))
  (do ((row (previous-row cell) (previous-row row)))
      ((eq row cell) (signal 'cell-is-not-in-column))
    (when (typep row 'column-header)
      (return-from find-column (when (string= (name row) column-name) row)))))

(defmethod find-row ((problem problem) (row-name string))
  (do-linked-list-not-first row problem next-row 
    (when (string= (name row) row-name) (return-from find-row row))))

(defmethod find-row ((cell cell) (row-name string))
  (do ((column (previous-column cell) (previous-column column)))
      ((eq column cell) (signal 'cell-is-not-in-row))
    (when (typep column 'row-header)
      (return-from find-row (when (string= (name column) row-name) column)))))

(defmethod row ((cell cell))
  (do-linked-list-all c cell next-column
    (when (typep c 'row-header) (return-from row c))))

(defmethod column ((cell cell))
  (do-linked-list-all c cell next-row
    (when (typep c 'column-header) (return-from column c))))
	   

(defmethod add-cell ((problem problem) (row row-header) (col column-header))
  (let ((new-cell (make-instance 'cell)))
    (setf (column-header new-cell) col)
    (incf (nr-rows col))
    (insert-cell-horizontally-after new-cell row)
    (insert-cell-vertically-after new-cell col)
    new-cell))

(defmethod add-cell ((problem problem) (row-name string) (col-name string))
  "Adds a cell to the problem matrix.
row-name and col-name need to indicate 
existing rows otherwise it will sginal cant-find-row or cant-find-column.
Also the cell must not exist yet, otherwise it will be duplicated
and the result will be an ill-posed problem."
  (let ((col (find-column problem col-name))
	(row (find-row problem row-name)))
    (unless row (signal 'cant-find-row))
    (unless col (signal 'cant-find-column))
    (add-cell problem row col)))

(defun find-cell (problem row-name col-name)
  (let ((row (find-row problem row-name)))
    (unless row (return-from find-cell nil))
    (do-linked-list-not-first cell row next-column
      (when (find-column cell col-name) (return-from find-cell cell)))))


(defmethod remove-column ((problem problem) (col column-header))
  (do-linked-list-not-first cell col next-row 
    (remove-horizontally cell))
  col)

(defmethod remove-column ((problem problem) (col-name string))
  (let ((col (find-column problem col-name)))
    (when col (remove-column problem col))
    col))

(defun reinsert-column (problem col)
  (declare (ignore problem))
  (do-linked-list-not-first cell col next-row
    (reinsert-horizontally cell))
  col)

(defun reinsert-row (problem row)
  (declare (ignore problem))
  (do-linked-list-not-first cell row previous-column
    (reinsert-vertically cell)
    (unless (typep cell 'row-header)
      (incf (nr-rows (column-header cell)))))
  row)

(defmethod remove-row ((problem problem) (row cell))
  "Removes the row from the probem.  
This is done by removing all the row cells, except
the argument `row' by calling remove-vertically.  This
operation can be undone by calling reinsert-row with
the same argument `row'."
  (declare (ignore problem))
  (do-linked-list-not-first cell row next-column
    (unless (typep cell 'row-header)
      (decf (nr-rows (column-header cell))))
    (remove-vertically cell))
  row)

(defmethod remove-row ((problem problem) (row-name string))
  (let ((row (find-row problem row-name)))
    (when row (remove-row problem row))
    row))

(defun empty-problem ()
  (let ((problem (make-instance 'problem)))
    (initialize problem)
    problem))

(defun print-problem (p)
  (format t "     ")
  (do-linked-list-not-first c p next-column
    (format t "~5A" (name c)))
  (format t "~%")
  (do-linked-list-not-first r p next-row
    (format t "~%~5A" (name r))
    (do-linked-list-not-first c p next-column
      (format t "~5A" (if (find-cell p (name r) (name c)) "#" ".")))))
    
      


;;; Problem solving algorithme

(defun cover-column (p col)
  "Remove all rows that have a one in this column!
It returns the column that is covered and the column
still has all the links intact to the rows that are removed.
"
  (do-linked-list-not-first cell col next-row
    (if (typep cell 'column-header)
	(remove-horizontally cell)
	(remove-row p cell)))
  col)
    
(defun uncover-column (p col)
  (do-linked-list-not-first cell col previous-row
    (if (typep cell 'column-header)
	(reinsert-horizontally cell)
	(reinsert-row p cell))))

(defun place-row (p row)
  (do-linked-list-not-first cell row next-column
    (unless (typep cell 'row-header)
      (cover-column p cell)))
  (cover-column p row)
  (remove-row p row)
  row)
    

(defun unplace-row (p row)
  (reinsert-row p row)
  (uncover-column p row)
  (do-linked-list-not-first cell row previous-column
    (unless (typep cell 'row-header)
      (uncover-column p cell)))
  row)


(defun cover-complete-p (p)
  (eq p (next-column p)))

(defun first-available-column (p)
  (next-column p))

(defun minimum-column-available (p)
  (let ((min 9999)
	(result nil))
    (do-linked-list-not-first col p next-column
      (when (< (nr-rows col) min)
	(setf min (nr-rows col))
	(setf result col)
	(when (= min 1) (return-from minimum-column-available result))
	(when (= min 0) (signal 'cant-find-column))))
    result))
    

(defun no-rows-p (p)
  (eq p (next-row p)))

(defun no-solution-possible-p (p)
  (when (no-rows-p p) (return-from no-solution-possible-p t))
  (do-linked-list-not-first col p next-column
    (when (= (nr-rows col) 0)
      (return-from no-solution-possible-p t)))
  nil)
    
;;

(defun find-or-insert-column (problem col-name)
  (or (find-column problem col-name) (add-column problem col-name)))


;;; Public handy methods

(defun create-problem ()
  "Create an empty problem"
  (let ((new-problem (make-instance 'problem)))
    (initialize new-problem)
    new-problem))


(defun add-specified-row (problem row-name extra-data list-col-names)
  (let ((row (add-row problem row-name extra-data)))
    (loop for col-name in list-col-names
	 do
	 (add-cell problem row (find-or-insert-column problem col-name)))
    row))


(defun cover (p &optional &key
	      (column-selection #'first-available-column) 
	      (max-nr-of-solutions 50)
	      (solution-so-far (list))
	      (solution-printer nil)
	      (solutions (list)))
  (do-linked-list-not-first row-cell (funcall column-selection p) next-row
    (place-row p row-cell)
    (push row-cell solution-so-far)
    (cond
      ((cover-complete-p p)
       (push solution-so-far solutions)
       (when solution-printer
	 (funcall solution-printer solution-so-far))
       (when (> (length solutions) max-nr-of-solutions)
	 (return-from cover solutions)))
      ((no-solution-possible-p p) )
;       (when solution-printer (funcall solution-printer solution-so-far))) ; continue
      (t 
       (setf solutions (cover p :solution-so-far solution-so-far 
			      :column-selection column-selection
			      :max-nr-of-solutions max-nr-of-solutions
			      :solution-printer solution-printer
			      :solutions solutions))))
    (unplace-row p (pop solution-so-far))
    (when (> (length solutions) max-nr-of-solutions)
      (return-from cover solutions)))
  solutions)
  
;(defun cover (problem &optional (result-list (list)))
;  (do-linked-list-not-first row-cell (next-column problem) next-row 

(defun create-test-problem ()
  (let ((problem (make-instance 'problem)))
    (initialize problem)
    (add-column problem "C1")
    (add-column problem "C2")
    (add-column problem "C3")
    (add-row problem "R1" nil)
    (add-row problem "R2" nil)
    (add-row problem "R3" nil)
    (add-row problem "R4" nil)
    (add-row problem "R5" nil)
    (add-cell problem "R1" "C2")
    (add-cell problem "R2" "C1")
    (add-cell problem "R1" "C3")
    (add-cell problem "R2" "C3")
    (add-cell problem "R3" "C2")
    (add-cell problem "R4" "C3")
    (add-cell problem "R5" "C1")
    problem))
