;;; nalist-test.el --- Tests for nalist

(require 'ert)
(require 'f)
(require 's)

(load (f-expand "nalist.el" default-directory))

(defun setup-alist ()
  (setq al '((a . b) (c . d)))
  (setq al-eql '((1 . a) (1.0 . b)))
  (setq al-equal '(("spam" . 3) ((a (b c)) . d))))

(ert-deftest test-nalist-subset-p ()
  (should (eq (nalist-subset-p '((a . 1)) '((b . 2) (a . 1)))
              t)))

(ert-deftest test-nalist-equal ()
  (should (eq (nalist-set-equal '((a . 1) (b . 2))
                                      '((b . 2) (a . 1)))
              t)))

(ert-deftest test-nalist-clear ()
  (setup-alist)
  (nalist-clear al)
  (should (eq al nil)))

(ert-deftest test-nalist-get ()
  (setup-alist)
  (should (eq (nalist-get 'a al)
              'b))
  (should (eq (nalist-get 'b al)
              nil))
  (should (eq (nalist-get 'c al)
              'd))
  (should (eq (nalist-get 'a al :testfn 'eq)
              'b))
  (should (eq (nalist-get 'b al :testfn 'eq)
              nil))
  (should (eq (nalist-get 1 al-eql)
              'a))
  (should (eq (nalist-get 1 al-eql :testfn 'eql)
              'a))
  (should (eq (nalist-get 1.0 al-eql)
              nil))
  (should (eq (nalist-get 1.0 al-eql :testfn 'eql)
              'b))
  (should (eq (nalist-get "spam" al-equal :testfn 'equal)
              3))
  (should (eq (nalist-get '(a (b c)) al-equal :testfn 'equal)
              'd))
  (should (eq (nalist-get "Hi" al-equal :testfn #'(lambda (x y) (= (length x) (length y))))
              'd))
  (should (eq (nalist-get 'a al :default 'no-value)
              'b))
  (should (eq (nalist-get 'f al :default 'no-value)
              'no-value)))

(ert-deftest test-nalist-set ()
  (setup-alist)
  (nalist-set 'e 'f al)
  (should (nalist-set-equal al '((a . b) (c . d) (e . f))))
  (nalist-set 'c 'g al)
  (should (nalist-set-equal al '((a . b) (c . g) (e . f))))
  )

(ert-deftest test-nalist-remove ()
  (setup-alist)
  (nalist-remove 'a al)
  (should (nalist-set-equal al '((c . d))))
  )

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

;;; nalist-test.el ends here
