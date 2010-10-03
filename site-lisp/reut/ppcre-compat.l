;;; -*- mode:lisp; package:reut -*-
;;; Port of cl-ppcre/api.lisp


;;; The external API for creating and using scanners.

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
	  regex-replace
	  regex-replace-all
	  register-groups-bind do-register-groups
	  with-loop-split
	  do-replace))

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


;;; apis
;;; from cl-ppcre/api.lisp

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



(defun regex-replace (string regexp replacement
	     &key (case-fold nil) (start 0) end (skip 0) (count nil))
  (cond
   ((functionp replacement)
    (with-output-to-string (os)
      (when (> start 0)
	(princ (subseq string 0 start) os))
      (do ((matchfunc (if case-fold #'string-matchp #'string-match))
	   (pos start (match-end 0))
	   (count (or count -1) (1- count)))
	  ((or (= count 0) (not (funcall matchfunc regexp string pos end)))
	   (princ (subseq string pos) os))
	(format os "~A~A" (subseq string pos (match-beginning 0))
		(funcall replacement (match-string 0))))))
   ((stringp replacement)
    (substitute-string string regexp replacement
		       :case-fold case-fold :start start :end end
		       :skip skip :count count))
   (t (error (make-condition 'type-error
			     :datum replacement
			     :expected-type 'string)))))

(defmacro do-replace ((string regexp
		       &key (case-fold nil) (start 0) end (skip 0) (count nil))
		      &body form)
  (let ((binds (loop for i from 0 to 9
		 collect (list (intern (format nil "$~D" i))
			       `(match-string ,i)))))
    (with-unique-names (gs gmf gpos gcount)
      `(with-output-to-string (,gs)
	 (when (> ,start 0)
	   (format ,gs "~A" (subseq ,string 0 ,start)))
	 (do ((,gmf (if ,case-fold #'string-matchp #'string-match))
	      (,gpos ,start (match-end 0))
	      (,gcount (or ,count -1) (1- ,gcount)))
	     ((or (= ,gcount 0) (not (funcall ,gmf ,regexp ,string ,gpos ,end)))
	      (format ,gs "~A" (subseq ,string ,gpos)))
	   (format ,gs "~A~A" (subseq ,string ,gpos (match-beginning 0))
		   (let ,binds ,@form)))))))


#|
(defun replace-aux (target-string replacement pos-list reg-list start end
                                  preserve-case simple-calls element-type)
  "Auxiliary function used by REGEX-REPLACE and REGEX-REPLACE-ALL.
POS-LIST contains a list with the start and end positions of all
matches while REG-LIST contains a list of arrays representing the
corresponding register start and end positions."
  (declare #.*standard-optimize-settings*)
  ;; build the template once before we start the loop
  (let ((replacement-template (build-replacement-template replacement)))
    (with-output-to-string (s nil :element-type element-type)
      ;; loop through all matches and take the start and end of the
      ;; whole string into account
      (loop for (from to) on (append (list start) pos-list (list end))
            ;; alternate between replacement and no replacement
            for replace = nil then (and (not replace) to)
            for reg-starts = (if replace (pop reg-list) nil)
            for reg-ends = (if replace (pop reg-list) nil)
            for curr-replacement = (if replace
                                     ;; build the replacement string
                                     (build-replacement replacement-template
                                                        target-string
                                                        start end
                                                        from to
                                                        reg-starts reg-ends
                                                        simple-calls
                                                        element-type)
                                     nil)
            while to
            if replace
              do (write-string (if preserve-case
                                 ;; modify the case of the replacement
                                 ;; string if necessary
                                 (funcall (string-case-modifier target-string
                                                                from to
                                                                start end)
                                          curr-replacement)
                                 curr-replacement)
                               s)
            else
              ;; no replacement
              do (write-string target-string s :start from :end to)))))

(defun regex-replace (regex target-string replacement &key
                            (start 0)
                            (end (length target-string))
                            preserve-case
                            simple-calls
                            (element-type #+:lispworks 'lw:simple-char #-:lispworks 'character))
  "Try to match TARGET-STRING between START and END against REGEX and
replace the first match with REPLACEMENT.  Two values are returned;
the modified string, and T if REGEX matched or NIL otherwise.

  REPLACEMENT can be a string which may contain the special substrings
\"\\&\" for the whole match, \"\\`\" for the part of TARGET-STRING
before the match, \"\\'\" for the part of TARGET-STRING after the
match, \"\\N\" or \"\\{N}\" for the Nth register where N is a positive
integer.

  REPLACEMENT can also be a function designator in which case the
match will be replaced with the result of calling the function
designated by REPLACEMENT with the arguments TARGET-STRING, START,
END, MATCH-START, MATCH-END, REG-STARTS, and REG-ENDS. (REG-STARTS and
REG-ENDS are arrays holding the start and end positions of matched
registers or NIL - the meaning of the other arguments should be
obvious.)

  Finally, REPLACEMENT can be a list where each element is a string,
one of the symbols :MATCH, :BEFORE-MATCH, or :AFTER-MATCH -
corresponding to \"\\&\", \"\\`\", and \"\\'\" above -, an integer N -
representing register (1+ N) -, or a function designator.

  If PRESERVE-CASE is true, the replacement will try to preserve the
case (all upper case, all lower case, or capitalized) of the
match. The result will always be a fresh string, even if REGEX doesn't
match.

  ELEMENT-TYPE is the element type of the resulting string."
;  (declare #.*standard-optimize-settings*)
  (multiple-value-bind (match-start match-end reg-starts reg-ends)
      (scan regex target-string :start start :end end)
    (if match-start
      (values (replace-aux target-string replacement
                           (list match-start match-end)
                           (list reg-starts reg-ends)
                           start end preserve-case
                           simple-calls element-type)
              t)
      (values (subseq target-string start end)
              nil))))

(defun regex-replace-all (regex target-string replacement &key
                                (start 0)
                                (end (length target-string))
                                preserve-case
                                simple-calls
                                (element-type #+:lispworks 'lw:simple-char #-:lispworks 'character))
  "Try to match TARGET-STRING between START and END against REGEX and
replace all matches with REPLACEMENT.  Two values are returned; the
modified string, and T if REGEX matched or NIL otherwise.

  REPLACEMENT can be a string which may contain the special substrings
\"\\&\" for the whole match, \"\\`\" for the part of TARGET-STRING
before the match, \"\\'\" for the part of TARGET-STRING after the
match, \"\\N\" or \"\\{N}\" for the Nth register where N is a positive
integer.

  REPLACEMENT can also be a function designator in which case the
match will be replaced with the result of calling the function
designated by REPLACEMENT with the arguments TARGET-STRING, START,
END, MATCH-START, MATCH-END, REG-STARTS, and REG-ENDS. (REG-STARTS and
REG-ENDS are arrays holding the start and end positions of matched
registers or NIL - the meaning of the other arguments should be
obvious.)

  Finally, REPLACEMENT can be a list where each element is a string,
one of the symbols :MATCH, :BEFORE-MATCH, or :AFTER-MATCH -
corresponding to \"\\&\", \"\\`\", and \"\\'\" above -, an integer N -
representing register (1+ N) -, or a function designator.

  If PRESERVE-CASE is true, the replacement will try to preserve the
case (all upper case, all lower case, or capitalized) of the
match. The result will always be a fresh string, even if REGEX doesn't
match.

  ELEMENT-TYPE is the element type of the resulting string."
  (declare #.*standard-optimize-settings*)
  (let ((pos-list '())
        (reg-list '()))
    (do-scans (match-start match-end reg-starts reg-ends regex target-string
                           nil
                           :start start :end end)
      (push match-start pos-list)
      (push match-end pos-list)
      (push reg-starts reg-list)
      (push reg-ends reg-list))
    (if pos-list
      (values (replace-aux target-string replacement
                           (nreverse pos-list)
                           (nreverse reg-list)
                           start end preserve-case
                           simple-calls element-type)
              t)
      (values (subseq target-string start end)
	      nil))))
|#

;;; from clisp/regexp

(defmacro with-loop-split ((var stream regex &key start end) &body body)
  (with-unique-names (line)
    `(do ((,line #1=(read-line ,stream nil nil) #1#))
	 ((null ,line))
       (let ((,var (split ,regex ,line
			  :start (or ,start 0)
			  :end (or ,end (length ,line)))))
	 ,@body))))



(provide "reut/api")

