(defpackage igo.viterbi-node
  (:use :common-lisp)
  (:export new
	   new-bos/eos
	   cost
	   prev
	   word-id
	   left-id
	   right-id
	   start
	   end
	   space?))
(in-package :igo.viterbi-node)

(defstruct (viterbi-node (:constructor new (word-id start end left-id right-id space?))
			 (:conc-name ""))
  (cost 0     :type fixnum)
  (prev nil   :type (or null viterbi-node))
  (word-id 0  :type fixnum)
  (left-id 0  :type fixnum)
  (right-id 0 :type fixnum)
  (start 0    :type fixnum)
  (end 0      :type fixnum)
  (space? nil :type boolean))

(defun new-bos/eos () 
  (new 0 0 0 0 0 nil))