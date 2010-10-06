;;; -*- mode:lisp; package:reut -*-

;;; ppcre-compatible API for xyzzy lisp.
;;; (Port of cl-ppcre/api.lisp)


;;; Copyright (c) 2009-2010, Yousuke Ushiki. All rights reserved.
;;; Copyright (c) 2002-2008, Dr. Edmund Weitz. All rights reserved.

;;; Redistribution and use in source and binary forms, with or without
;;; modification, are permitted provided that the following conditions
;;; are met:

;;;   * Redistributions of source code must retain the above copyright
;;;     notice, this list of conditions and the following disclaimer.

;;;   * Redistributions in binary form must reproduce the above
;;;     copyright notice, this list of conditions and the following
;;;     disclaimer in the documentation and/or other materials
;;;     provided with the distribution.

;;; THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;;; OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;;; WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;; ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;;; DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;;; DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;;; GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(eval-when (:compile :load-toplevel :execute)
  (require "reut/pkg"))

(in-package :reut)

(export '(scan scan-to-strings
	  do-scans do-matches do-matches-as-strings
	  all-matches all-matches-as-strings split
	  regex-replace regex-replace-all
	  register-groups-bind do-register-groups))

;;; utilities macros and functions
;;; from cl-ppcre/util.lisp

(defmacro with-unique-names ((&rest bindings) &body body)
  `(let ,(mapcar
	  #'(lambda (binding)
	      (check-type binding (or cons symbol))
	      (if (consp binding)
		  (let ((var (car binding))
			(x (cadr binding)))
		    (check-type var symbol)
		    `(,var (gensym ,(format nil "~A" x))))
		`(,binding (gensym ,(symbol-name binding)))))
	  bindings)
     ,@body))

(defmacro with-rebinding (bindings &body body)
  (loop for binding in bindings
    for var = (if (consp binding) (car binding) binding)
    for name = (gensym)
    collect `(,name ,var) into renames
    collect ``(,,var ,,name) into temps
    finally (return `(let ,renames
		       (with-unique-names ,bindings
			 `(let (,,@temps)
			    ,,@body))))))

(defun nsubseq (sequence start &optional (end (length sequence)))
  "Returns a subsequence by pointing to location in original sequence."
  (make-vector (- end start)
              :element-type (array-element-type sequence)
              :displaced-to sequence
              :displaced-index-offset start))

(defun normalize-var-list (var-list)
  "Utility function for REGISTER-GROUPS-BIND and DO-REGISTER-GROUPS.
Creates the long form \(a list of \(FUNCTION VAR) entries) out of the
short form of VAR-LIST."
  (loop for element in var-list
    if (consp element)
      nconc (loop for var in (rest element)
	      collect (list (first element) var))
    else
      collect (list '(function identity) element)))

(defun string-list-to-simple-string (string-list)
  "Concatenates a list of strings to one simple-string."
  ;; this function provided by JP Massar; note that we can't use APPLY
  ;; with CONCATENATE here because of CALL-ARGUMENTS-LIMIT
  (let ((total-size 0))
    (declare (fixnum total-size))
    (dolist (string string-list)
      #-:genera (declare (string string))
      (incf total-size (length string)))
    (let ((result-string (make-sequence 'simple-string total-size))
	  (curr-pos 0))
      (declare (fixnum curr-pos))
      (dolist (string string-list)
	#-:genera (declare (string string))
	(replace result-string string :start1 curr-pos)
	(incf curr-pos (length string)))
      result-string)))

(defun set-register-values (starts ends)
  (setf (fill-pointer starts) 0
	(fill-pointer ends) 0)
  (dotimes (i 9)
    (let* ((idx (1+ i))
	   (beg (match-beginning idx))
	   (end (match-end idx)))
      (setf (aref starts i) beg
	    (aref ends i) end)
      (when beg
	(setf (fill-pointer starts) idx
	      (fill-pointer ends) idx)))))


;; create-scanner (string string) &key case-insensitive-mode multi-line-mode single-line-mode extended-mode destructive => scanner, register-names
;; create-scanner (parse-tree t) &key case-insensitive-mode multi-line-mode single-line-mode extended-mode destructive => scanner, register-names
;; create-scanner (function function) &key case-insensitive-mode multi-line-mode single-line-mode extended-mode destructive => scanner


(defun scan (regex target-string &key start end)
  (let ((rstarts #1=(make-vector 9 :fill-pointer 0))
	(rends #1#))
    (string-match regex target-string (or start 0) (or end (length target-string)))
    (set-register-values rstarts rends)
    (values (match-beginning 0) (match-end 0)
	    rstarts rends)))


(defun scan-to-strings (regex target-string
			      &key (start 0)
			      (end (length target-string))
			      sharedp)
  "Like SCAN but returns substrings of TARGET-STRING instead of
positions, i.e. this function returns two values on success: the whole
match as a string plus an array of substrings (or NILs) corresponding
to the matched registers.  If SHAREDP is true, the substrings may
share structure with TARGET-STRING."
  (multiple-value-bind (match-start match-end reg-starts reg-ends)
      (scan regex target-string :start start :end end)
    (unless match-start
      (return-from scan-to-strings nil))
    (let ((substr-fn (if sharedp #'nsubseq #'subseq)))
      (values (funcall substr-fn
		       target-string match-start match-end)
	      (map 'vector
		   (lambda (reg-start reg-end)
		     (if reg-start
			 (funcall substr-fn
				  target-string reg-start reg-end)
		       nil))
		   reg-starts
		   reg-ends)))))


(defmacro register-groups-bind (var-list (regex target-string
						&key start end sharedp)
					 &body body)
  "Executes BODY with the variables in VAR-LIST bound to the
corresponding register groups after TARGET-STRING has been matched
against REGEX, i.e. each variable is either bound to a string or to
NIL.  If there is no match, BODY is _not_ executed. For each element
of VAR-LIST which is NIL there's no binding to the corresponding
register group.  The number of variables in VAR-LIST must not be
greater than the number of register groups.  If SHAREDP is true, the
substrings may share structure with TARGET-STRING."
  (with-rebinding (target-string)
    (with-unique-names (match-start match-end reg-starts reg-ends
				    start-index substr-fn)
      `(multiple-value-bind (,match-start ,match-end ,reg-starts ,reg-ends)
	   (scan ,regex ,target-string :start (or ,start 0)
		 :end (or ,end (length ,target-string)))
	 (declare (ignore ,match-end))
	 (when ,match-start
	   (let* ,(cons
		   `(,substr-fn (if ,sharedp
				    #'nsubseq
				  #'subseq))
		   (loop for (function var) in (normalize-var-list var-list)
		     for counter from 0
		     when var
		     collect `(,var (let ((,start-index
					   (aref ,reg-starts ,counter)))
				      (if ,start-index
					  (funcall ,function
						   (funcall ,substr-fn
							    ,target-string
							    ,start-index
							    (aref ,reg-ends ,counter)))
					nil)))))
	     ,@body))))))


(defmacro do-scans ((match-start match-end reg-starts reg-ends
				 regex target-string
				 &optional result-form
				 &key start end)
		    &body body)
  (with-rebinding (target-string)
    (with-unique-names (re %start %end rs re i)
      `(block nil
	 (let ((,re (compile-regexp ,regex))
	       (,%start (or ,start 0))
	       (,%end (or ,end (length ,target-string)))
	       (,reg-starts #1=(make-vector 9 :fill-pointer 0))
	       (,reg-ends #1#))
	   (loop
	     (string-match ,re ,target-string ,%start ,end)
	     (let ((,match-start (match-beginning 0))
		   (,match-end (match-end 0)))
	       (unless ,match-start
		 (return ,result-form))
	       (set-register-values ,reg-starts ,reg-ends)
	       ,@body
	       (setq ,%start (if (= ,match-start ,match-end)
				 (1+ ,match-end)
			       ,match-end)))))))))


(defmacro do-matches ((match-start match-end
				   regex target-string
				   &optional result-form
				   &key start end)
		      &body body)
  (with-rebinding (target-string)
    (with-unique-names (re %start %end)
      `(block nil
	 (let ((,re (compile-regexp ,regex))
	       (,%start (or ,start 0))
	       (,%end (or ,end (length ,target-string))))
	   (loop
	     (string-match ,re ,target-string ,%start ,end)
	     (let ((,match-start (match-beginning 0))
		   (,match-end (match-end 0)))
	       (unless ,match-start
		 (return ,result-form))
	       ,@body
	       (setq ,%start (if (= ,match-start ,match-end)
				 (1+ ,match-end)
			       ,match-end)))))))))


(defmacro do-matches-as-strings ((match-var regex target-string
					   &optional result-form
					   &key start end sharedp)
				&body body)
  "Iterates over TARGET-STRING and tries to match REGEX as often as
possible evaluating BODY with MATCH-VAR bound to the substring of
TARGET-STRING corresponding to each match in turn.  After the last
match, returns RESULT-FORM if provided or NIL otherwise.  An implicit
block named NIL surrounds DO-MATCHES-AS-STRINGS; RETURN may be used to
terminate the loop immediately.  If REGEX matches an empty string the
scan is continued one position behind this match.  If SHAREDP is true,
the substrings may share structure with TARGET-STRING.  BODY may start
with declarations."
  (with-rebinding (target-string)
    (with-unique-names (match-start match-end substr-fn)
      `(let ((,substr-fn (if ,sharedp #'nsubseq #'subseq)))
        (do-matches (,match-start ,match-end ,regex ,target-string
                     ,result-form :start ,start :end ,end)
          (let ((,match-var
                  (funcall ,substr-fn
                           ,target-string ,match-start ,match-end)))
            ,@body))))))


(defmacro do-register-groups (var-list (regex target-string
					      &optional result-form
					      &key start end sharedp)
				       &body body)
  "Iterates over TARGET-STRING and tries to match REGEX as often as
possible evaluating BODY with the variables in VAR-LIST bound to the
corresponding register groups for each match in turn, i.e. each
variable is either bound to a string or to NIL.  For each element of
VAR-LIST which is NIL there's no binding to the corresponding register
group. The number of variables in VAR-LIST must not be greater than
the number of register groups.  After the last match, returns
RESULT-FORM if provided or NIL otherwise.  An implicit block named NIL
surrounds DO-REGISTER-GROUPS; RETURN may be used to terminate the loop
immediately. If REGEX matches an empty string the scan is continued
one position behind this match.  If SHAREDP is true, the substrings
may share structure with TARGET-STRING.  BODY may start with
declarations."
  (with-rebinding (target-string)
    (with-unique-names (substr-fn match-start match-end
				  reg-starts reg-ends start-index)
      `(let ((,substr-fn (if ,sharedp
			     #'nsubseq
			   #'subseq)))
	 (do-scans (,match-start ,match-end ,reg-starts ,reg-ends
		    ,regex ,target-string
		    ,result-form :start ,start :end ,end)
	   (let ,(loop for (function var) in (normalize-var-list var-list)
		   for counter from 0
		   when var
		   collect `(,var (let ((,start-index
					 (aref ,reg-starts ,counter)))
				    (if ,start-index
					(funcall ,function
						 (funcall ,substr-fn
							  ,target-string
							  ,start-index
							  (aref ,reg-ends ,counter)))
				      nil))))
	     ,@body))))))


(defun all-matches (regex target-string
			  &key (start 0)
			  (end (length target-string)))
  (let (result-list)
    (do-matches (match-start match-end regex target-string
			     (nreverse result-list)
			     :start start :end end)
      (push match-start result-list)
      (push match-end result-list))))


(defun all-matches-as-strings (regex target-string
				     &key
				     (start 0)
				     (end (length target-string))
				     sharedp)
  (let (result-list)
    (do-matches-as-strings (match regex target-string (nreverse result-list)
				  :start start :end end :sharedp sharedp)
      (push match result-list))))


(defun split (regex target-string
		    &key (start 0)
		    (end (length target-string))
		    limit
		    with-registers-p
		    omit-unmatched-p
		    sharedp)
  "Matches REGEX against TARGET-STRING as often as possible and
returns a list of the substrings between the matches.  If
WITH-REGISTERS-P is true, substrings corresponding to matched
registers are inserted into the list as well.  If OMIT-UNMATCHED-P is
true, unmatched registers will simply be left out, otherwise they will
show up as NIL.  LIMIT limits the number of elements returned -
registers aren't counted.  If LIMIT is NIL \(or 0 which is
equivalent), trailing empty strings are removed from the result list.
If REGEX matches an empty string the scan is continued one position
behind this match.  If SHAREDP is true, the substrings may share
structure with TARGET-STRING."
  ;; initialize list of positions POS-LIST to extract substrings with
  ;; START so that the start of the next match will mark the end of
  ;; the first substring
  (let ((pos-list (list start))
	(counter 0))
    ;; how would Larry Wall do it?
    (when (eql limit 0)
      (setq limit nil))
    (do-scans (match-start match-end
			   reg-starts reg-ends
			   regex target-string nil
			   :start start :end end)
      (unless (and (= match-start match-end)
		   (= match-start (car pos-list)))
	;; push start of match on list unless this would be an empty
	;; string adjacent to the last element pushed onto the list
	(when (and limit
		   (>= (incf counter) limit))
	  (return))
	(push match-start pos-list)
	(when with-registers-p
	  ;; optionally insert matched registers
	  (loop for reg-start across reg-starts
	    for reg-end across reg-ends
	    if reg-start
	    ;; but only if they've matched
	    do (push reg-start pos-list)
	    (push reg-end pos-list)
	    else unless omit-unmatched-p
	    ;; or if we're allowed to insert NIL instead
	    do (push nil pos-list)
	    (push nil pos-list)))
	;; now end of match
	(push match-end pos-list)))
    ;; end of whole string
    (push end pos-list)
    ;; now collect substrings
    (nreverse
     (loop with substr-fn = (if sharedp #'nsubseq #'subseq)
       with string-seen = nil
       for (this-end this-start) on pos-list by #'cddr
       ;; skip empty strings from end of list
       if (or limit
	      (setq string-seen
		    (or string-seen
			(and this-start
			     (> this-end this-start)))))
       collect (if this-start
		   (funcall substr-fn
			    target-string this-start this-end)
		 nil)))))


(defun regex-replace-all (regexp string replacement
			  &key (start 0) end count case-fold)
  (cond
   ((functionp replacement)
    (let ((c 0) (count (or count -1)))
      (values
       (with-output-to-string (os)
	 (when (> start 0)
	   (princ (subseq string 0 start) os))
	 (do ((matchfunc (if case-fold #'string-matchp #'string-match))
	      (pos start (match-end 0)))
	     ((or (= c count) (not (funcall matchfunc regexp string pos end)))
	      (princ (subseq string pos) os))
	   (incf c)
	   (format os "~A~A" (subseq string pos (match-beginning 0))
	     (funcall replacement (match-string 0)))))
       c)))
   ((stringp replacement)
    (substitute-string string regexp replacement
		       :case-fold case-fold :start start :end end))
   (t (error (make-condition 'type-error
			     :datum replacement
			     :expected-type '(or string function))))))

(defun regex-replace (regexp string replacement
		      &key case-fold (start 0) end)
  (regex-replace-all regexp string replacement
		     :case-fold :case-fold
		     :start start :end end :count 1))


(provide "reut/ppcre-compat")


