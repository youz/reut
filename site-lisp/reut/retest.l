;;; -*- mode: lisp; -*-

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "reut"))

(defpackage :regexp-test
  (:use :lisp :editor :reut))


(equalp (multiple-value-list (scan #/(a)*b/ "xaaabd"))
        (list 1 5 #(3) #(4)))

(equalp (multiple-value-list (scan #/(a)*b/ "xaaabd" :start 1))
        (list 1 5 #(3) #(4)))

(equalp (multiple-value-list (scan #/(a)*b/ "xaaabd" :start 2))
        (list 2 5 #(3) #(4)))

(null (scan #/(a)*b/ "xaaabd" :end 4))

(equalp (multiple-value-list (scan #/b{0,}/ "bbbc")) ; '(:greedy-repetition 0 nil #\b)
        (list 0 3 #() #()))

(null (scan #/b{4,6}/ "bbbc")) ; '(:greedy-repetition 4 6 #\b)

(let ((s #/(([a-c])+)x/))
  (equalp (multiple-value-list (scan s "abcxy"))
          (list 0 4 #(0 2) #(3 3))))

(equalp (multiple-value-list (scan-to-strings #/[^b]*b/ "aaabd"))
        (list "aaab" #()))

(equalp (multiple-value-list (scan-to-strings #/([^b])*b/ "aaabd"))
        (list "aaab" #("a")))

(equalp (multiple-value-list (scan-to-strings #/(([^b])*)b/ "aaabd"))
        (list "aaab" #("aaa" "a")))

(equalp (register-groups-bind (first second third fourth)
            (#/((a)|(b)|(c))+/ "abababc" :sharedp t)
          (list first second third fourth))
        (list "c" "a" "b" "c"))

(equalp (register-groups-bind (nil second third fourth)
            (#/((a)|(b)|(c))()+/ "abababc" :start 6)
          (list second third fourth))
        (list nil nil "c"))

(null (register-groups-bind (first)
          (#/(a|b)+/ "accc" :start 1)
        first))

(equalp (register-groups-bind (fname lname (#'parse-integer date month year))
	    ( #/(\w+)\s+(\w+)\s+(\d{1,2})\.(\d{1,2})\.(\d{4})/ "Frank Zappa 21.12.1940" )
          (list fname lname (encode-universal-time 0 0 0 date month year 0)))
        (list "Frank" "Zappa" 1292889600))

(flet ((foo (regex target-string &key (start 0) (end (length target-string)))
         (let ((sum 0))
           (do-matches (s e regex target-string nil :start start :end end)
             (incf sum (- e s)))
           (/ sum (- end start)))))
  (and (= 1/3 (foo #/a/ "abcabcabc"))
       (= 5/9 (foo #/aa|b/ "aacabcbbc"))))

(labels ((crossfoot (target-string &key (start 0) (end (length target-string)))
           (let ((sum 0))
             (do-matches-as-strings (m #/\d/
                                       target-string nil
                                       :start start :end end)
               (incf sum (parse-integer m)))
             (if (< sum 10)
               sum
               (crossfoot (format nil "~A" sum))))))
  (and (zerop (crossfoot "bar"))
       (= 3 (crossfoot "a3x"))
       (= 6 (crossfoot "12345"))))

(let (result)
  (do-register-groups (first second third fourth)
      (#/((a)|(b)|(c))/ "abababc" nil :start 2 :sharedp t)
    (push (list first second third fourth) result))
  (equal (nreverse result)
         '(("a" "a" nil nil) 
           ("b" nil "b" nil) 
           ("a" "a" nil nil) 
           ("b" nil "b" nil) 
           ("c" nil nil "c"))))

(let (result)
  (do-register-groups ((#'parse-integer n) (#'intern sign) whitespace)
      (#/(\d+)|(\+|-|\*|\/)|(\s+)/ "12*15 - 42/3")
    (unless whitespace
      (push (or n sign) result)))
  (equal (nreverse result)
         '(12 * 15 - 42 / 3)))

(equal (all-matches #/a/ "foo bar baz")
       (list 5 6 9 10))

(equal (all-matches #/\w*/ "foo bar baz")
       (list 0 3 3 3 4 7 7 7 8 11 11 11))

(equal (all-matches-as-strings #/a/ "foo bar baz")
       (list "a" "a"))

(equal (all-matches-as-strings #/\w*/ "foo bar baz")
       (list "foo" "" "bar" "" "baz" ""))

(equal (split #/\s+/ "foo   bar baz
frob")
       '("foo" "bar" "baz" "frob"))

(equal (split #/\s*/ "foo bar   baz")
       '("f" "o" "o" "b" "a" "r" "b" "a" "z"))

(equal (split #/(\s+)/ "foo bar   baz")
       '("foo" "bar" "baz"))

(equal (split #/(\s+)/ "foo bar   baz" :with-registers-p t)
       '("foo" " " "bar" "   " "baz"))

(equal (split #/(\s)(\s*)/ "foo bar   baz" :with-registers-p t)
       '("foo" " " "" "bar" " " "  " "baz"))

(equal (split #/(,)|(;)/ "foo,bar;baz" :with-registers-p t)
       '("foo" "," nil "bar" nil ";" "baz"))

(equal (split #/(,)|(;)/ "foo,bar;baz" :with-registers-p t :omit-unmatched-p t)
       '("foo" "," "bar" ";" "baz"))

(equal (split #/:/ "a:b:c:d:e:f:g::")
       '("a" "b" "c" "d" "e" "f" "g"))

(equal (split #/:/ "a:b:c:d:e:f:g::" :limit 1)
       '("a:b:c:d:e:f:g::"))

(equal (split #/:/ "a:b:c:d:e:f:g::" :limit 2)
       '("a" "b:c:d:e:f:g::"))

(equal (split #/:/ "a:b:c:d:e:f:g::" :limit 3)
       '("a" "b" "c:d:e:f:g::"))

(equal (split #/:/ "a:b:c:d:e:f:g::" :limit 1000)
       '("a" "b" "c" "d" "e" "f" "g" "" ""))

#|
(equal (multiple-value-list (regex-replace-all "foo bar" #/fo+/ "frob"))
       (list "frob bar" 1))

(equal (multiple-value-list (regex-replace-all "FOO bar" #/fo+/ "frob"))
       (list "FOO bar" 0))

(equal (multiple-value-list (regex-replace-all "FOO bar" #/fo+/ "frob" :preserve-case nil)) ; (?i)fo+
       (list "frob bar" 1))

(equal (multiple-value-list (regex-replace-all "FOO bar" #/(?i)fo+/ "frob" :preserve-case t))
       (list "FROB bar" 1))

(equal (multiple-value-list (regex-replace-all "Foo bar" #/(?i)fo+/ "frob" :preserve-case t))
       (list "Frob bar" 1))

(equal (multiple-value-list (regex-replace-all "foo bar baz" #/bar/ "[frob (was '\\&' between '\\`' and '\\'')]"))
       (list "foo [frob (was 'bar' between 'foo ' and ' baz')] baz" 1))

(equal (multiple-value-list
        (regex-replace "bar" "foo bar baz"
                       '("[frob (was '" :match "' between '" :before-match "' and '" :after-match "')]")))
       (list "foo [frob (was 'bar' between 'foo ' and ' baz')] baz" t))

(equal (multiple-value-list (regex-replace "(be)(nev)(o)(lent)"
                                           "benevolent: adj. generous, kind"
                                           (lambda (match &rest registers)
                                             (format nil "~A [~{~A~^.~}]" match registers))
                                           :simple-calls t))
       (list "benevolent [be.nev.o.lent]: adj. generous, kind" t))

(equal (multiple-value-list (regex-replace-all "(?i)fo+" "foo Fooo FOOOO bar" "frob" :preserve-case t))
       (list "frob Frob FROB bar" t))

(string= (regex-replace-all "(?i)f(o+)" "foo Fooo FOOOO bar" "fr\\1b" :preserve-case t)
         "froob Frooob FROOOOB bar")

(let ((qp-regex #/[\x80-\xff]/))
  (flet ((encode-quoted-printable (string)
           "Converts 8-bit string to quoted-printable representation."
           ;; won't work for Corman Lisp because non-ASCII characters aren't 8-bit there
	   (flet ((convert (matched-string)
		    (format nil "=~2,'0x" (char-code (char matched-string 0)))))
             (regex-replace-all qp-regex string #'convert))))
    (string= #?=(encode-quoted-printable "F�te S�rensen na�ve H�hner Stra�e")
	     "F=EAte S=F8rensen na=EFve H=FChner Stra=DFe")))

(let ((url-regex #/[^a-zA-Z0-9_\\-.]/))
  (flet ((url-encode (string)
           "URL-encodes a string."
           ;; won't work for Corman Lisp because non-ASCII characters aren't 8-bit there
           (flet ((convert (target-string)
		    (format nil "%~2,'0x" (char-code (char target-string 0)))))
             (regex-replace-all url-regex string #'convert))))
    (string= (url-encode "F�te S�rensen na�ve H�hner Stra�e")
             "F%EAte%20S%F8rensen%20na%EFve%20H%FChner%20Stra%DFe")))

(flet ((how-many (target-string start end match-start match-end reg-starts reg-ends)
         (declare (ignore target-string start end match-start match-end))
         (format nil "~A" (- (svref reg-ends 0)
                             (svref reg-starts 0)))))
  (string= (regex-replace-all "{(.+?)}"
                              "foo{...}bar{.....}{..}baz{....}frob"
                              (list "[" #'how-many " dots]"))
           "foo[3 dots]bar[5 dots][2 dots]baz[4 dots]frob"))

(let ((qp-regex #/[\\x80-\\xff]/))
  (flet ((encode-quoted-printable (string)
           "Converts 8-bit string to quoted-printable representation.
Version using SIMPLE-CALLS keyword argument."
           ;; ;; won't work for Corman Lisp because non-ASCII characters aren't 8-bit there
           (flet ((convert (match)
                    (format nil "=~2,'0x" (char-code (char match 0)))))
             (regex-replace-all qp-regex string #'convert
                                :simple-calls t))))
    (string= (encode-quoted-printable "F�te S�rensen na�ve H�hner Stra�e")
             "F=EAte S=F8rensen na=EFve H=FChner Stra=DFe")))

(flet ((how-many (match first-register)
         (declare (ignore match))
         (format nil "~A" (length first-register))))
  (string= (regex-replace-all "{(.+?)}"
                              "foo{...}bar{.....}{..}baz{....}frob"
                              (list "[" #'how-many " dots]")
                              :simple-calls t)
           "foo[3 dots]bar[5 dots][2 dots]baz[4 dots]frob"))

(flet ((my-repetition (char min)
         `(:greedy-repetition ,min nil ,char)))
  (setf (parse-tree-synonym 'a*) (my-repetition #\a 0)
        (parse-tree-synonym 'b+) (my-repetition #\b 1))
  (unwind-protect
      (let ((scanner (create-scanner '(:sequence a* b+))))
        (equal (mapcar (lambda (target)
                         (scan scanner target))
                       '("ab" "b" "aab" "a" "x"))
               (list 0 0 0 nil nil)))
    (setf (parse-tree-synonym 'a*) nil
          (parse-tree-synonym 'b+) nil)))

(null (scan #/^a+$/ "a+"))

(let ((*allow-quoting* t))
  ;;we use CREATE-SCANNER because of Lisps like SBCL that don't have an interpreter
  (equalp (multiple-value-list (scan #/^\\Qa+\\E$/ "a+"))
          (list 0 2 #() #())))

(string= (parse-string "\\k<reg>") "k<reg>")

(let ((*allow-named-registers* t))
  (equal (nth-value 1 #/((?<small>[a-z]*)(?<big>[A-Z]*))/)
         (list nil "small" "big")))

(let ((*allow-named-registers* t))
  (equal (nth-value 1 (create-scanner '(:register
                                        (:sequence
                                         (:named-register "small"
                                          (:greedy-repetition 0 nil (:char-class (:range #\a #\z))))
                                         (:named-register "big"
                                          (:greedy-repetition 0 nil (:char-class (:range #\a #\z))))))))
         (list nil "small" "big")))

(let ((*allow-named-registers* t))
  (equalp (multiple-value-list (scan #/((?<small>[a-z]*)(?<big>[A-Z]*))/ "aaaBBB"))
          (list 0 6 #(0 0 3) #(6 3 6))))

(let ((*allow-named-registers* t))
  ;; multiple-choice back-reference
  (equalp (multiple-value-list (scan #/^(?<reg>[ab])(?<reg>[12])\\k<reg>\\k<reg>$/ "a1aa"))
          (list 0 4 #(0 1) #(1 2))))

(let ((*allow-named-registers* t))
  (equalp (multiple-value-list (scan #/^(?<reg>[ab])(?<reg>[12])\\k<reg>\\k<reg>$/ "a22a"))
          (list 0 4 #(0 1) #(1 2))))

(let ((*allow-named-registers* t))
  ;; demonstrating most-recently-seen-register-first property of back-reference;
  ;; "greedy" regex (analogous to "aa?")
  (equalp (multiple-value-list (scan #/^(?<reg>)(?<reg>a)(\\k<reg>)/ "a"))
          (list 0 1 #(0 0 1) #(0 1 1))))

(let ((*allow-named-registers* t))
  (equalp (multiple-value-list (scan #/^(?<reg>)(?<reg>a)(\\k<reg>)/ "aa"))
          (list 0 2 #(0 0 1) #(0 1 2))))

(let ((*allow-named-registers* t))
  ;; switched groups
  ;; "lazy" regex (analogous to "aa??")
  (equalp (multiple-value-list (scan #/^(?<reg>a)(?<reg>)(\\k<reg>)/ "a"))
          (list 0 1 #(0 1 1) #(1 1 1))))

(let ((*allow-named-registers* t))
  ;; scanner ignores the second "a"
  (equalp (multiple-value-list (scan #/^(?<reg>a)(?<reg>)(\\k<reg>)/ "aa"))
          (list 0 1 #(0 1 1) #(1 1 1))))

(let ((*allow-named-registers* t))
  ;; "aa" will be matched only when forced by adding "$" at the end
  (equalp (multiple-value-list (scan #/^(?<reg>a)(?<reg>)(\\k<reg>)$/ "aa"))
          (list 0 2 #(0 1 1) #(1 1 2))))

(string= (quote-meta-chars "[a-z]*") "\\[a\\-z\\]\\*")

(string= (handler-case
             #/foo**x/
           (ppcre-syntax-error (condition)
             (format nil "Houston, we've got a problem with the string ~S:  Looks like something went wrong at position ~A.  The last message we received was \"~?\"."
                     (ppcre-syntax-error-string condition)
                     (ppcre-syntax-error-pos condition)
                     (simple-condition-format-control condition)
                     (simple-condition-format-arguments condition))))
         "Houston, we've got a problem with the string \"foo**x\":  Looks like something went wrong at position 4.  The last message we received was \"Quantifier '*' not allowed.\".")

(flet ((my-weird-filter (pos)
         "Only match at this point if either pos is odd and the
character we're looking at is lowercase or if pos is even and the next
two characters we're looking at are uppercase. Consume these
characters if there's a match."
         (cond ((and (oddp pos)
                     (< pos cl-ppcre::*end-pos*)
                     (lower-case-p (char cl-ppcre::*string* pos)))
                (1+ pos))
               ((and (evenp pos)
                     (< (1+ pos) cl-ppcre::*end-pos*)
                     (upper-case-p (char cl-ppcre::*string* pos))
                     (upper-case-p (char cl-ppcre::*string* (1+ pos))))
                (+ pos 2))
               (t nil))))
  (let ((weird-regex `(:sequence "+" (:filter ,#'my-weird-filter) "+")))
    (equalp (multiple-value-list (scan weird-regex "+A++a+AA+"))
            (list 5 9 #() #()))))
|#

(equalp (multiple-value-list (scan #/(?:\\w\*){2}/ "\\w*\\w*"))
          (list 0 6 #() #()))

(let ((a "\\E*"))
  (equalp (multiple-value-list (scan `(:greedy-repetition 2 2 ,a) "\\E*\\E*"))
          (list 0 6 #() #())))

(loop for *optimize-char-classes* in '(:hash-table :hash-table* :charset :charset* :charmap)
      for s = #/(([a-c])+)x/
      always (equalp (multiple-value-list (scan s "abcxy"))
                     (list 0 4 #(0 2) #(3 3))))
