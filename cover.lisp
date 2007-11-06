; Package preamble
(defpackage #:cover
  (:use #:cl)
  (:export #:cover
	   #:problem
	   #:cell
	   #:row
	   #:extra-data
	   #:add-row
	   #:add-column
	   #:add-cell
	   #:create-problem
	   #:add-specified-row
	   #:print-problem
	   #:create-test-problem))

(in-package :cover)

;; 
;; Utility macros
;;
(defmacro do-linked-list-not-first (loop-var start next-function &body body)
  `(do ((,loop-var (,next-function ,start) (,next-function ,loop-var)))
       ((eq ,loop-var ,start) nil)
     ,@body))

(defmacro do-linked-list-all (loop-var start next-function &body body)
  `(progn 
     (let ((,loop-var ,start))
       ,@body)
     (do-linked-list-not-first ,loop-var ,start ,next-function ,@body)))

;;
;; Class definitions
;;
(defclass cell ()
  ((next-column :accessor next-column)
   (previous-column :accessor previous-column)
   (next-row :accessor next-row)
   (previous-row :accessor previous-row)
   (column-header :accessor column-header :initarg :column))
  (:documentation 
   "Cell in matrix.
Representing a cell in the sparse problem matrix.  Will contain links to its horizontal and vertical
neighbours.  Also includes a pointer up to the column header."))

(defclass header (cell)
  ((name :accessor name :initarg :name))
  (:documentation "Generic header cell.
Allows naming of the cell.  Specially usefull for the headers in the matrix."))

(defclass row-header (header)
  ((extra-data :accessor extra-data :initarg :extra-data))
  (:documentation "Represents the head of a row.  
Extra data is not used by this algorithm, but can be convenient for interpretng the solution."))
  
(defclass column-header (header)
  ((nr-rows :accessor nr-rows :initform 0))
  (:documentation "Represents the head of a column.
The nr-rows property keep track of the number of rows intersecting this column."))

(defclass problem (cell) 
  ()
					;  ((min-nr-rows :accessor min-nr-rows :initform 9999))
  (:documentation "Representng the problem."))


(define-condition cell-is-not-in-column (error) ())
(define-condition cell-is-not-in-row (error) ())
(define-condition cant-find-row () ())
(define-condition cant-find-column () ())

(defgeneric add-row (problem row-name extra-data)  (:documentation "Add an empty row to the problem"))
(defgeneric add-column (problem column-name) (:documentation "Add an empty column to the problem"))
(defgeneric add-cell (problem row col) (:documentation "Adds a cell to the matrix.  This should update the column count!"))
(defgeneric find-column (start-point column-name) (:documentation "Try to find a column starting at start-point"))
(defgeneric find-row (start-point column-name) (:documentation "Try to find a row starting at start-point"))
(defgeneric remove-row (problem col) (:documentation "Remove row from problem"))

(defmethod initialize-instance :after ((cell cell) &rest ignored)
  (declare (ignore ignored))
  (setf (next-column cell) cell)
  (setf (previous-column cell) cell)
  (setf (previous-row cell) cell)
  (setf (next-row cell) cell))

(defmacro insert-after-double-linked-list (new-cell old-cell next-cell previous-cell)
  "Insert new element in double linked list.
Situation before:
                                             next-cell
      old cell <-------------------------------> other-cell
                    previous-cell

Situation after:

      old cell <--------> new-cell <--------> other cell

Returns the new-cell
"  
  `(progn 
     (setf (,next-cell ,new-cell) (,next-cell ,old-cell))
     (setf (,previous-cell ,new-cell) ,old-cell)
     (setf (,previous-cell (,next-cell ,old-cell)) ,new-cell)
     (setf (,next-cell ,old-cell) ,new-cell)))

(defun insert-cell-vertically-after (new-cell old-cell)
  "Insert new cell in the linked list describing a column.
Returns the `new-cell'."
  (insert-after-double-linked-list new-cell old-cell next-row previous-row))

(defun insert-cell-horizontally-after (new-cell old-cell)
  "Insert new cell in the linked list describing a row.
Returns the `new-cell'."
  (insert-after-double-linked-list new-cell old-cell next-column previous-column))

(defun remove-horizontally (cell)
  "Remove the cell from row.
This will break the links that makes this cell part of a row.
The operation is reversible with the function `reinsert-horizontally'.
Note that it will not update in any way the links that keep a cell in a column."
  (setf (next-column (previous-column cell)) (next-column cell))
  (setf (previous-column (next-column cell)) (previous-column cell)))

(defun remove-vertically (cell)
  "Remove the cell from a column.
This will break the links that makes this cell part of a column.
The operation is reversible with the function `reinsert-vertically'.
Note that it will not update the links that keep a cell in a row."
  (setf (next-row (previous-row cell)) (next-row cell))
  (setf (previous-row (next-row cell)) (previous-row cell)))

(defun reinsert-horizontally (cell)
  "Reinsert cell in problem.
If a cell is removed by `remove-horzontally' and the problem 
matrix this cell has been removed from is in the same state as it was
just after removal by `remove-horizonally' the effect of `remove-horizonally'
can be undone by calling this function.

Returns `cell'."
  (setf (previous-column (next-column cell)) cell)
  (setf (next-column (previous-column cell)) cell))

(defun reinsert-vertically (cell)
  "Reinsert cell in problem.
If a cell is removed by `remove-vertically' and the problem 
matrix this cell has been removed from is in the same state as it was
just after removal by `remove-vertically' the effect of `remove-vetically'
can be undone by calling this function.

Returns `cell'."
  (setf (previous-row (next-row cell)) cell)
  (setf (next-row (previous-row cell)) cell))

(defmethod add-row ((problem problem) (row-name string) (extra-data t))
  "Adds a new row to problem.
It is not checked if a row with this name already exists.
The row will be empty and have no cells in it.

Returns the new row."
  (insert-cell-vertically-after (make-instance 'row-header 
					       :name row-name 
					       :extra-data extra-data)
				problem))

(defmethod add-column ((problem problem) (col-name string))
  "Adds a new column to problem.
It is not checked if a column with col-name already exists.
The column will be empty and have no cells in it.

Returns the new column."
  (insert-cell-horizontally-after (make-instance 'column-header :name col-name) problem))

(defmethod find-column ((problem problem) (column-name string))
  (do-linked-list-not-first column problem next-column
    (when (string= (name column) column-name) (return-from find-column column))))

(defmethod find-row ((problem problem) (row-name string))
  (do-linked-list-not-first row problem next-row 
    (when (string= (name row) row-name) (return-from find-row row))))

(defmethod add-cell ((problem problem) (row row-header) (col column-header))
  "Creates and adds a new cell to problem.
It assumes the `row' and `col' parameters are from the same problem.
Returns the new cell."
  (declare (ignore problem))
  (let ((new-cell (make-instance 'cell :column col)))
    (incf (nr-rows col))
    (insert-cell-horizontally-after new-cell row)
    (insert-cell-vertically-after new-cell col)
    new-cell))

(defmethod add-cell ((problem problem) (row-name string) (col-name string))
  "Adds a cell to the problem matrix.
row-name and col-name need to indicate 
existing rows otherwise it will sginal cant-find-row or cant-find-column.
Also the cell must not exist yet, otherwise it will be duplicated
and the result will be an ill-posed problem.

Returns the new cell."
  (let ((col (find-column problem col-name))
	(row (find-row problem row-name)))
    (unless row (signal 'cant-find-row))
    (unless col (signal 'cant-find-column))
    (add-cell problem row col)))

(defun find-cell (problem row-name col-name)
  "Finds cell by 'row-name' and 'col-name'.
Used for printing the matrix."
  (let ((row (find-row problem row-name)))
    (unless row (return-from find-cell nil))
    (do-linked-list-not-first cell row next-column
      (when (string= (name (column-header cell)) col-name) (return-from find-cell cell)))))

(defun row (cell)
  "Find a row for cell"
  (do-linked-list-all x cell next-column
    (when (typep x 'row-header) (return-from row x))))

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
    new-problem))


(defun add-specified-row (problem row-name extra-data list-col-names)
  "Add new row to problem.
The row will be constructed with name `row-name' and the additional data of the row
will be set to `extra-data'.  
In addition `list-col-names' is a list of strings each of which indicate
the column for which this row will have a cell.  If a column does not exist yet
it will be created and added to the problem as well.

Returns the new row."
  (let ((row (add-row problem row-name extra-data)))
    (loop for col-name in list-col-names
	 do
	 (add-cell problem row (find-or-insert-column problem col-name)))
    row))


(defun cover (p &optional &key
	      (column-selection #'minimum-column-available) 
	      (max-nr-of-solutions 50)
	      (solution-printer nil))
  "Solves cover problem given by p.
The optional keyword arguments modify search tree traversal behaviour.

Returns a list of cells that describe the solution.
"
  (let ((solution-so-far (list))
	(solutions (list)))
    (labels ((cover-aux () 
	       "Runs the actual algorithm."
	       (do-linked-list-not-first row-cell (funcall column-selection p) next-row
		 ;; Try row and add to partial solution
		 (place-row p row-cell)
		 (push row-cell solution-so-far)
		 ;; check the state solution sofar
		 (cond
		   ;; Found solution
		   ((cover-complete-p p)         
		    (push solution-so-far solutions)
		    (when solution-printer
		      (funcall solution-printer solution-so-far)))
		   ;; Dead end, do nothing
		   ((no-solution-possible-p p))
		   ;; normal case, go deeper in tree
		   (t 
		    (cover-aux)))
		 ;; Restore situation
		 (unplace-row p (pop solution-so-far))
		 (when (>= (length solutions) max-nr-of-solutions)
		   (return-from cover-aux)))))
    (cover-aux))
    solutions))

(defun create-test-problem ()
  (let ((problem (make-instance 'problem)))
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
