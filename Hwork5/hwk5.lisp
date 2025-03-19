(defun Ifn (str)
  "Parse nonterminal I: I → i E S I′"
  (print '(in Ifn))
  (print str)
  (cond ((eql (car str) 'i)
         (let ((rest (cdr str)))
           (setf rest (Efn rest))
           (setf rest (Sfn rest))
           (setf rest (Iprimefn rest))
           rest))
        (t (append (cdr str) (list 'err)))))  ; error case

(defun Iprimefn (str)
  "Parse nonterminal I′: I′ → e S | ε"
  (print '(in Iprimefn))
  (print str)
  (cond ((eql (car str) 'e)
         (let ((rest (cdr str)))
           (setf rest (Sfn rest))
           rest))
        (t str)))  ; ε production returns the input unchanged

(defun Efn (str)
  "Parse nonterminal E: E → G E′"
  (print '(in Efn))
  (print str)
  (let ((rest (Gfn str)))
    (setf rest (Eprimefn rest))
    rest))

(defun Eprimefn (str)
  "Parse nonterminal E′: E′ → o G E′ | ε"
  (print '(in Eprimefn))
  (print str)
  (cond ((eql (car str) 'o)
         (let ((rest (cdr str)))
           (setf rest (Gfn rest))
           (setf rest (Eprimefn rest))
           rest))
        (t str)))  ; ε production

(defun Gfn (str)
  "Parse nonterminal G: G → x | y | z | w"
  (print '(in Gfn))
  (print str)
  (cond ((or (eql (car str) 'x)
             (eql (car str) 'y)
             (eql (car str) 'z)
             (eql (car str) 'w))
         (cdr str))
        (t (append (cdr str) (list 'err)))))

(defun Sfn (str)
  "Parse nonterminal S: S → s | d L b"
  (print '(in Sfn))
  (print str)
  (cond ((eql (car str) 's)
         (cdr str))
        ((eql (car str) 'd)
         (let ((rest (cdr str)))
           (setf rest (Lfn rest))
           (if (and rest (eql (car rest) 'b))
               (cdr rest)
               (append (cdr rest) (list 'err)))))
        (t (append (cdr str) (list 'err)))))

(defun Lfn (str)
  "Parse nonterminal L: L → s L′"
  (print '(in Lfn))
  (print str)
  (cond ((eql (car str) 's)
         (let ((rest (cdr str)))
           (setf rest (Lprimefn rest))
           rest))
        (t (append (cdr str) (list 'err)))))

(defun Lprimefn (str)
  "Parse nonterminal L′: L′ → s L′ | ε"
  (print '(in Lprimefn))
  (print str)
  (cond ((eql (car str) 's)
         (let ((rest (cdr str)))
           (setf rest (Lprimefn rest))
           rest))
        (t str)))  ; ε production

(defun parse (str)
  "Top-level parse function: start from nonterminal I."
  (print '(in parse))
  (print str)
  (let ((result (Ifn str)))
    (if (or (null result) (and (listp result) (null result)))
        (print "Parse successful!")
        (if (member 'err result)
            (print "Parse error!")
            (print (format nil "Parse finished with leftover tokens: ~A" result))))))


