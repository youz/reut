;;; -*- mode:lisp; package:editor -*-

(eval-when (:compile :load-toplevel :execute)
  (require "reut/pkg")
  (require "reut/psre-reader"))

(in-package "editor")

(export '(*psre-search* psre-search-dialog psre-replace-dialog))
(defvar *psre-search* t)


(defun user::qrr (pattern replacement)
  (interactive "sReplace: \nswith: ")
  (query-replace-regexp (reut:convert-from-psre pattern) replacement))


(defvar *psre-search-dialog-template*
  '(dialog 0 0 242 95
    (:caption "������̌���")
    (:font 9 "MS UI Gothic")
    (:control
     (:static nil "����(&S):" #x50020000 7 10 28 8)
     (:combobox search nil #x50210842 39 8 140 96)
     (:button case-fold "�啶������������ʂ���(&C)" #x50010006 39 27 96 10)
     (:button word "�P��P�ʂŌ�������(&W)" #x50010003 39 40 83 10)
     (:button regexp "���K�\��(&E)" #x50010003 39 53 52 10)
     (:button psre "Perl��(&P)" #x50010003 95 53 52 10)
     (:button escseq "�G�X�P�[�v�V�[�P���X�𗝉�����(&Y)" #x50010003 39 66 107 10)
     (:button wrap "������Ȃ���Ζ߂��Č���(&V)" #x50010003 39 79 101 10)
     (:button backward "�㌟��(&P)" #x50010000 185 7 50 14)
     (:button forward "������(&N)" #x50010001 185 24 50 14)
     (:button IDCANCEL "�L�����Z��" #x50010000 185 41 50 14))))

(defun psre-search-dialog ()
  (interactive)
  (multiple-value-bind (result data)
      (let ((string (or (selection-start-end (start end)
			  (buffer-substring start end))
			(if *regexp-search*
			    *last-search-regexp*
			  *last-search-string*))))
	(dialog-box *psre-search-dialog-template*
		    (list (cons 'search *minibuffer-search-string-history*)
			  (cons 'case-fold (cfs2dialog *case-fold-search*))
			  (cons 'word *word-search*)
			  (cons 'regexp *regexp-search*)
			  (cons 'psre *psre-search*)
			  (cons 'search (if (eq (cdr *last-search-string-pair*)
						string)
					    (car *last-search-string-pair*)
					  string))
			  (cons 'escseq *understand-escape-sequences*)
			  (cons 'wrap *wrap-search*))
		    '((word :disable (regexp))
		      (regexp :non-null t :enable (psre))
		      (search :non-null "�������������͂���" :enable (forward backward)))))
    (when result
      (let ((string (cdr (assoc 'search data))))
	(when string
	  (add-history string '*minibuffer-search-string-history*)
	  (setq *case-fold-search* (dialog2cfs (cdr (assoc 'case-fold data))))
	  (setq *word-search* (cdr (assoc 'word data)))
	  (setq *regexp-search* (cdr (assoc 'regexp data)))
	  (setq *psre-search* (cdr (assoc 'regexp data)))
	  (setq *wrap-search* (cdr (assoc 'wrap data)))
	  (setq *understand-escape-sequences* (cdr (assoc 'escseq data)))
	  (if *understand-escape-sequences*
	      (setq *last-search-string-pair*
		    (cons string
			  (setq string (decode-escape-sequence
					string *regexp-search*))))
	    (setq *last-search-string-pair* nil))
	  (when (and *regexp-search* *psre-search*)
	    (setq string (reut:convert-from-psre string)))
	  (search-command string (eq result 'backward)
			  (and (null *word-search*) *regexp-search*)
			  t nil))))))


(defvar *psre-replace-dialog-template*
  '(dialog 0 0 242 128
    (:caption "������̒u��")
    (:font 9 "MS UI Gothic")
    (:control
     (:static nil "����(&S):" #x50020000 7 10 32 8)
     (:combobox search nil #x50210842 39 7 140 96)
     (:static nil "�u��(&R):" #x50020000 7 29 32 8)
     (:combobox replace nil #x50210842 39 26 140 96)
     (:button case-fold "�啶������������ʂ���(&C)" #x50010006 39 46 113 10)
     (:button word "�P��P�ʂŌ�������(&W)" #x50010003 39 59 97 10)
     (:button regexp "���K�\��(&E)" #x50010003 39 72 63 10)
     (:button psre "Perl��(&P)" #x50010003 95 72 63 10)
     (:button escseq "�G�X�P�[�v�V�[�P���X�𗝉�����(&Y)" #x50010003 39 85 113 10)
     (:button whole "�o�b�t�@�̐擪����(&O)" #x50010003 39 98 89 10)
     (:button query "�m�F����(&Q)" #x50010001 185 7 50 14)
     (:button all "�S�Ēu��(&A)" #x50010000 185 24 50 14)
     (:button IDCANCEL "�L�����Z��" #x50010000 185 41 50 14))))

(defun psre-replace-dialog ()
  (interactive)
  (multiple-value-bind (result data)
      (let ((search (or (selection-start-end (start end)
			  (buffer-substring start end))
			(if *regexp-search*
			    *last-search-regexp*
			  *last-search-string*)))
	    (replace (if *regexp-search*
			 *last-replace-regexp*
		       *last-replace-string*)))
	(dialog-box *psre-replace-dialog-template*
		    (list (cons 'search *minibuffer-search-string-history*)
			  (cons 'replace *minibuffer-search-string-history*)
			  (cons 'case-fold (cfs2dialog *case-fold-search*))
			  (cons 'word *word-search*)
			  (cons 'regexp *regexp-search*)
			  (cons 'psre *psre-search*)
			  (cons 'whole *replace-whole-buffer*)
			  (cons 'search
				(if (eq (cdr *last-search-string-pair*)
					search)
				    (car *last-search-string-pair*)
				  search))
			  (cons 'replace
				(if (eq (cdr *last-replace-string-pair*)
					replace)
				    (car *last-replace-string-pair*)
				  replace))
			  (cons 'escseq *understand-escape-sequences*))
		    '((word :disable (regexp))
		      (regexp :non-null t :enable (psre))
		      (search :non-null "�������������͂���" :enable (query all)))))
    (when result
      (let ((search (cdr (assoc 'search data)))
	    (replace (cdr (assoc 'replace data))))
	(when search
	  (add-history search '*minibuffer-search-string-history*)
	  (add-history replace '*minibuffer-search-string-history*)
	  (setq *case-fold-search* (dialog2cfs (cdr (assoc 'case-fold data))))
	  (setq *word-search* (cdr (assoc 'word data)))
	  (setq *regexp-search* (cdr (assoc 'regexp data)))
	  (setq *psre-search* (cdr (assoc 'psre data)))
	  (setq *understand-escape-sequences* (cdr (assoc 'escseq data)))
	  (if *understand-escape-sequences*
	      (progn
		(setq *last-search-string-pair*
		      (cons search
			    (setq search (decode-escape-sequence
					  search *regexp-search*))))
		(setq *last-replace-string-pair*
		      (cons replace
			    (setq replace (decode-escape-sequence
					   replace *regexp-search*)))))
	    (progn
	      (setq *last-search-string-pair* nil)
	      (setq *last-replace-string-pair* nil)))
	  (setq *replace-whole-buffer* (cdr (assoc 'whole data)))
	  (when *replace-whole-buffer*
	    (goto-char (point-min)))
	  (when (and *regexp-search* *psre-search*)
	    (setq search (reut:convert-from-psre search)))
	  (perform-replace search replace (eq result 'query)
			   (and (null *word-search*) *regexp-search*)
			   t nil))))))

(provide "reut/psre-replace")