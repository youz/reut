;;; -*- mode:Lisp; package:reut -*-

;;; Perl Style Regexp Reader for xyzzy

;;  #/(a+)b/
;;    -> "\\(a+\\)b"
;;  #/(a|b|xyz){2,3}/
;;    -> "\\(a\\|b\\|xyz\\)\\{2,3\\}"
;;   #/[^\[\-\]\d]+/)
;;    -> "[^][0-9-]+"

(eval-when (:compile :load-toplevel :execute)
  (require "reut/pkg"))

(in-package "reut")
(export '(convert-from-psre))


(defun psre-reader (stream subchar n)
  (with-output-to-string (os)
    (labels
	((start ()
	   (do ((c #1=(read-char stream nil nil) #1#))
	       ((eql c subchar))
	     (case c
	       ((#\( #\) #\{ #\} #\|) (format os "\\~A" c))
	       (#\\ (escape #1#))
	       (#\[ (char-class))
	       (t (princ c os)))))
	 (print-code (s &rest code)
	   (let ((code (parse-integer (format nil "~{~A~}" code) :radix 16)))
	     (princ (code-char code) s)))
	 (escape (c)
	   (unless c (error 'end-of-file))
	   (case c
	     ((#\( #\) #\{ #\} #\| #\/) (princ c os))
	     (#\t (princ #\TAB os))
	     (#\n (princ #\LFD os))
	     (#\r (princ #\RET os))
	     (#\f (princ #\C-l os))
	     (#\d (princ "[0-9]" os))
	     (#\D (princ "[^0-9]" os))
	     (#\s (princ "[ \t\n\r\f]" os))
	     (#\S (princ "[^ \t\n\r\f]" os))
	     (#\x (print-code os #1# #1#))
	     (#\X (print-code os #1# #1# #1# #1#))
	     (t (format os "\\~A" c))))
	 (char-class ()
	   (princ #\[ os)
	   (when (char= #\^ (peek-char nil stream))
	     (princ #1# os))
	   (format os "~A]"
	     (with-output-to-string (cs)
	       (do ((c #1# #1#)
		    (hyphen ""))
		   ((eql c #\]) (princ hyphen cs))
		 (unless c
		   #2=(error "Åe[ÅfÇ…ëŒâûÇ∑ÇÈÅe]ÅfÇ™Ç†ÇËÇ‹ÇπÇÒ" 'simple-error))
		 (case c
		   (#\\
		    (let ((e #1#))
		      (case e
			(#\- (setq hyphen "-"))
			(#\] (princ #\] os))
			(#\w (princ "0-9A-Za-z_" cs))
			(#\d (princ "0-9" cs))
			(#\s (princ " \t\n\r\f" cs))
			(#\t (princ #\TAB cs))
			(#\n (princ #\LFD cs))
			(#\r (princ #\RET cs))
			(#\f (princ #\C-l cs))
			(#\x (print-code cs #1# #1#))
			(#\X (print-code cs #1# #1# #1# #1#))
			(t (princ e cs)))))
		   (subchar #2#)
		   (t (princ c cs))))))))
      (start))))

(set-dispatch-macro-character #\# #\/ #'psre-reader)

(defun convert-from-psre (psre)
  (with-input-from-string (is psre)
    (reut::psre-reader is nil nil)))


(in-package "editor")
(defun psre-reader-colorize ()
  (unless (local-variable-p 'regexp-keyword-list)
    (make-local-variable 'regexp-keyword-list))
  (setf regexp-keyword-list
	(append regexp-keyword-list
		(compile-regexp-keyword-list
		 '(("#/.*?[^\\]/" t (:color 9 0)))))))

(add-hook '*lisp-mode-hook* 'psre-reader-colorize)
(add-hook '*lisp-interaction-mode-hook* 'psre-reader-colorize)


(provide "reut/psre-reader")
