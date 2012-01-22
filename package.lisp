;;; Copyright (c) 2007,2011 by Willem Rein Oudshoorn
;;;
;;;

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
	   #:create-test-problem
	   #:place-row
	   #:unplace-row
	   #:find-column
	   #:next-column
	   #:previous-column
	   #:next-row
	   #:previous-row
	   #:column-header
	   #:find-row
	   #:remove-row
	   #:find-or-insert-column))
