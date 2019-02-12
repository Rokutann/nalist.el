;;; nalist-test.el --- Tests for nalist.  -*- lexical-binding: t; -*-

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
         (nalist-init nal-eql '((1 . a) (1.0 . b)))
         (nalist-init nal-equal '(("foo" . 3) ((a (b c)) . d)))
         (nalist-init nal-4 '((a . b) (c . d) (e . f) (g . h)))
         ,@body)
     ;; teardown
     ))

(ert-deftest test-nalist-pairp ()
  (should (eq (nalist-pairp nil)
              nil))
  (should (eq (nalist-pairp 'a)
              nil))
  (should (eq (nalist-pairp '(a))
              t))
  (should (eq (nalist-pairp '(nil . a))
              t))
  (should (eq (nalist-pairp '(a . b))
              t))
  (should (eq (nalist-pairp '(a b))
              t)))

(ert-deftest test-nalist-mappable-list-p ()
  (should (eq (nalist-mappable-list-p nil)
              t))
  (should (eq (nalist-mappable-list-p '(a))
              t))
  (should (eq (nalist-mappable-list-p '(a . b))
              nil))
  (should (eq (nalist-mappable-list-p '(a  b))
              t))
  (should (eq (nalist-mappable-list-p '((a . b) . c))
              nil))
  (should (eq (nalist-mappable-list-p '(a  b . c))
              nil))
  (should (eq (nalist-mappable-list-p '((a . b)  c))
              t))
  (should (eq (nalist-mappable-list-p '((a . b) . (c . d)))
              nil))
  (should (eq (nalist-mappable-list-p '((a . b)  (c . d)))
              t)))

(ert-deftest test-nalist-nalist-p ()
  (should (eq (nalist-nalist-p nil)
              t))
  (should (eq (nalist-nalist-p '(a))
              nil))
  (should (eq (nalist-nalist-p '(a . b))
              nil))
  (should (eq (nalist-nalist-p '(a  b))
              nil))
  (should (eq (nalist-nalist-p '((a . b)))
              t))
  (should (eq (nalist-nalist-p '(nil . (a . b)))
              nil))
  (should (eq (nalist-nalist-p '((a . b) . (c . d)))
              nil))
  (should (eq (nalist-nalist-p '((a . b) (c . d)))
              t)))

(ert-deftest test-nalist-init ()
  (setq alist '((x . y) (z . a)))
  (nalist-init na-shallow alist :shallow t)
  (nalist-init na-deep alist)
  (nalist-init na-nil nil)
  (should (eq alist na-shallow))
  (should (not (eq alist na-deep)))
  (should (equal alist na-deep))
  (should (eq na-nil nil))
  (should (eq (nalist-init na-1 '((a . b) (c . d)))
              'na-1))
  (should-error (nalist-init na-2 'a)))

(ert-deftest test-nalist-equal ()
  (should (nalist-equal nal
                        '((a . b) (c . d))))
  (should (not (nalist-equal nal
                             '((c . d) (a . b))))))

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
   (nalist-copy nal nal-deep)
   (nalist-copy nal nal-shallow :shallow t)
   (should (eq nal nal-shallow))
   (should (not (eq nal nal-deep)))
   (should (equal nal nal-deep))
   (setq not-nalist 'a)
   (should-error (nalist-copy not-nalist nal-new))))

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
   (should (eq (nalist-get "foo" nal-equal :testfn 'equal)
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

(ert-deftest test-lexical-binding ()
  (setq closure nil)
  (nalist-init nal '((v . w) (x . y)))
  (let ((nal nil))
    (nalist-init nal '((a . b) (c . d)))
    (setq closure #'(lambda (key value)
                      (nalist-set key value nal)
                      nal)))
  (should (nalist-set-equal nal
                            '((v . w) (x . y))))
  (should (nalist-set-equal (funcall closure 'a 'e)
                            '((a . e) (c . d))))
  (should (nalist-set-equal nal
                            '((v . w) (x . y)))))

(ert-deftest test-nalist-remove ()
  (with-nalist-fixture
   (nalist-remove 'a nal)
   (should (nalist-set-equal nal '((c . d))))))

;; Local Variables:
;; flycheck-disabled-checkers: (emacs-lisp-checkdoc)
;; End:

;;; nalist-test.el ends here
