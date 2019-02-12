;;; nalist-test.el --- Tests for nalist

(require 'seq)
(require 'ert)
(require 'f)
(require 's)

(load (f-expand "nalist.el" default-directory))

(defmacro with-nalist-fixture (&rest body)
  `(unwind-protect
       (progn
         ;; setup
         (nalist-init nal '((a . b) (c . d)))
         (nalist-init nal-eql (copy-alist '((1 . a) (1.0 . b))))
         (nalist-init nal-equal (copy-alist '(("spam" . 3) ((a (b c)) . d))))
         (nalist-init nal-4 '((a . b) (c . d) (e . f) (g . h)))
         ,@body)
     ;; teardown
     ))

(ert-deftest test-nalist-map ()
  (with-nalist-fixture
   (let ((res nil))
     (nalist-map #'(lambda (k v) (push k res)) nal-4)
     (should (seq-set-equal-p res '(a c e g)))
     )))

(ert-deftest test-nalist-pop ()
  (with-nalist-fixture
   (should (eq (nalist-pop 'e nal-4)
               'f))
   (should (nalist-set-equal nal-4
                             '((a . b) (c . d) (g . h))))))

(ert-deftest test-nalist-poppair ()
  (with-nalist-fixture
   (should (equal (nalist-poppair nal-4)
                  '(a . b)))
   (should (nalist-set-equal nal-4
                             '((c . d) (e . f) (g . h))))))

(ert-deftest test-nalist-copy ()
  (with-nalist-fixture
   (nalist-copy nal nal-new)
   (should (not (eq nal nal-new)))
   (should (equal nal nal-new))
   ))

(ert-deftest test-nalist-values ()
  (with-nalist-fixture
   (should (seq-set-equal-p (nalist-values nal)
                            '(b d)))))

(ert-deftest test-nalist-pairs ()
  (with-nalist-fixture
   (should (nalist-set-equal (nalist-pairs nal)
                             '((a . b) (c . d))))
   (should (not (eq (nalist-pairs nal)
                    nal)))))

(ert-deftest test-nalist-keys ()
  (with-nalist-fixture
   (should (seq-set-equal-p (nalist-keys nal)
                            '(a c)))))

(ert-deftest test-nalist-subset-p ()
  (should (eq (nalist-subset-p '((a . 1)) '((b . 2) (a . 1)))
              t)))

(ert-deftest test-nalist-equal ()
  (should (eq (nalist-set-equal '((a . 1) (b . 2))
                                      '((b . 2) (a . 1)))
              t)))

(ert-deftest test-nalist-clear ()
  (with-nalist-fixture
   (nalist-clear nal)
   (should (eq nal nil))))

(ert-deftest test-nalist-get ()
  (with-nalist-fixture
   (should (eq (nalist-get 'a nal)
               'b))
   (should (eq (nalist-get 'b nal)
               nil))
   (should (eq (nalist-get 'c nal)
               'd))
   (should (eq (nalist-get 'a nal :testfn 'eq)
               'b))
   (should (eq (nalist-get 'b nal :testfn 'eq)
               nil))
   (should (eq (nalist-get 1 nal-eql)
               'a))
   (should (eq (nalist-get 1 nal-eql :testfn 'eql)
               'a))
   (should (eq (nalist-get 1.0 nal-eql)
               nil))
   (should (eq (nalist-get 1.0 nal-eql :testfn 'eql)
               'b))
   (should (eq (nalist-get "spam" nal-equal :testfn 'equal)
               3))
   (should (eq (nalist-get '(a (b c)) nal-equal :testfn 'equal)
               'd))
   (should (eq (nalist-get "Hi" nal-equal :testfn #'(lambda (x y) (= (length x) (length y))))
               'd))
   (should (eq (nalist-get 'a nal :default 'no-value)
               'b))
   (should (eq (nalist-get 'f nal :default 'no-value)
               'no-value))))

(ert-deftest test-nalist-set ()
  (with-nalist-fixture
   (nalist-set 'e 'f nal)
   (should (nalist-set-equal nal '((a . b) (c . d) (e . f))))
   (nalist-set 'c 'g nal)
   (should (nalist-set-equal nal '((a . b) (c . g) (e . f))))))

(ert-deftest test-nalist-remove ()
  (with-nalist-fixture
   (nalist-remove 'a nal)
   (should (nalist-set-equal nal '((c . d))))))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

;;; nalist-test.el ends here
