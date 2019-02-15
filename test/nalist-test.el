;;; nalist-test.el --- Nalist: Tests.  -*- lexical-binding: t; -*-
;; Copyright (C) 2019  Cyriakus "Mukuge" Hill

;; Author: Cyriakus "Mukuge" Hill <cyriakus.h@gmail.com>
;; Keywords: Lisp, tools
;; URL: https://github.com/mukuge/nalist.el

;; This file is NOT part of GNU Emacs.

;;; License:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(ert-deftest nalist-pairp-test/nil ()
  (should-not(nalist-pairp nil)))

(ert-deftest nalist-pairp-test/symbol ()
  (should-not (nalist-pairp 'a)))

(ert-deftest nalist-pairp-test/string-as-non-trivial-atom ()
  (should-not (nalist-pairp "foo")))

(ert-deftest nalist-pairp-test/cons ()
  (should (nalist-pairp '(a . b))))

(ert-deftest nalist-pairp-test/list ()
  (should (nalist-pairp '(a b))))

(ert-deftest nalist-proper-list-p-test/nil ()
  (should (nalist-proper-list-p nil)))

(ert-deftest nalist-proper-list-p-test/symbol ()
  (should-not (nalist-proper-list-p 'a)))

(ert-deftest nalist-proper-list-p-test/string-as-non-trivial-atom ()
  (should-not (nalist-proper-list-p "foo")))

(ert-deftest nalist-proper-list-p-test/cons ()
  (should-not (nalist-proper-list-p '(a . b))))

(ert-deftest nalist-proper-list-p-test/list ()
  (should (nalist-proper-list-p '(a b))))

(ert-deftest nalist-proper-list-p-test/long-cons ()
  (should-not (nalist-proper-list-p '(a b c d e f g h i j k l m n . o))))

(ert-deftest nalist-proper-list-p-test/long-list ()
  (should (nalist-proper-list-p '(a b c d e f g h i j k l m n o))))

(ert-deftest nalist-proper-list-p-test/deep-cons ()
  (should-not (nalist-proper-list-p '(e (((c ((a) ((b)) )) . d)) (((b . f) j)) . o))))

(ert-deftest nalist-proper-list-p-test/deep-list ()
  (should (nalist-proper-list-p '(((i j ((e))) (f) (g h) a) (b (c d))))))

(ert-deftest nalist-proper-list-p-test/alist-one ()
  (should (nalist-proper-list-p '((a . b)))))

(ert-deftest nalist-proper-list-p-test/alist-long ()
  (should (nalist-proper-list-p '((a . b) (c . d) (e . f) (g . h) (i . j) (k . l) (m . n) (o . p)))))

(ert-deftest nalist-proper-list-p-test/circular-1-periodic ()
  (should-not (nalist-proper-list-p '#1=(a . #1#))))

(ert-deftest nalist-proper-list-p-test/circular-2-periodic ()
  (should-not (nalist-proper-list-p '#1=(a b . #1#))))

(ert-deftest nalist-proper-list-p-test/circular-3-periodic ()
  (should-not (nalist-proper-list-p '#1=(a b c . #1#))))

(ert-deftest nalist-proper-list-p-test/circular-4-periodic ()
  (should-not (nalist-proper-list-p '#1=(a b c d . #1#))))

(ert-deftest nalist-proper-list-p-test/circular-5-periodic ()
  (should-not (nalist-proper-list-p '#1=(a b c d e . #1#))))

(ert-deftest nalist-proper-list-p-test/circular-just-sharing-1-step ()
  (should (nalist-proper-list-p '#1=(#1#))))

(ert-deftest nalist-proper-list-p-test/circular-just-sharing-2-steps ()
  (should (nalist-proper-list-p '#1=(a #1#))))

(ert-deftest nalist-proper-list-p-test/circular-just-sharing-3-steps ()
  (should (nalist-proper-list-p '#1=(a b #1#))))

(ert-deftest nalist-proper-list-p-test/circular-just-sharing-4-steps ()
  (should (nalist-proper-list-p '#1=(a b c #1#))))

(ert-deftest nalist-proper-list-p-test/circular-just-sharing-5-steps ()
  (should (nalist-proper-list-p '#1=(a b c d #1#))))

(ert-deftest nalist-proper-list-p-test/circular-misc ()
  (should-not (nalist-proper-list-p '#1=(#2=(#1# . #2#) . #1#)))
  (should (nalist-proper-list-p '#1=(#2=(#1# . #4=#1#) . #3=(#4# . #2#))))
  (should (nalist-proper-list-p '(#1=(a) . #1#)))
  (should (nalist-proper-list-p '#1=(a  #1#)))
  (should (nalist-proper-list-p '(#1=(a) b #1#)))
  (should-not (nalist-proper-list-p '#1=(#2=(a) (#2#) . #1#)))
  (should-not (nalist-proper-list-p '#1=(#2=(#3=(a . #1#) . #2#) . #3#))))

(ert-deftest nalist-nalist-p-test/nil ()
  (should (nalist-nalist-p nil)))

(ert-deftest nalist-nalist-p-test/symbol ()
  (should-not (nalist-nalist-p 'a)))

(ert-deftest nalist-nalist-p-test/string-as-non-trivial-atom ()
  (should-not (nalist-nalist-p "foo")))

(ert-deftest nalist-nalist-p-test/pair ()
  (should-not (nalist-nalist-p '(a . b))))

(ert-deftest nalist-nalist-p-test/proper-list ()
  (should-not (nalist-nalist-p '(a b))))

(ert-deftest nalist-nalist-p-test/circular-list ()
  (should-not (nalist-nalist-p '#1=(a #1#))))

(ert-deftest nalist-nalist-p-test/alist-1-pair ()
  (should (nalist-nalist-p '((a . b)))))

(ert-deftest nalist-nalist-p-test/alist-2-pairs ()
  (should (nalist-nalist-p '((a . b) (c . d)))))

(ert-deftest nalist-nalist-p-test/alist-3-pairs ()
  (should (nalist-nalist-p '((a . b) (nil) (c . d)))))

(ert-deftest nalist-nalist-p-test/broken-alist-1 ()
  (should-not (nalist-nalist-p '((a . b) nil (c . d)))))

(ert-deftest nalist-nalist-p-test/broken-alist-2 ()
  (should-not (nalist-nalist-p '((a . b) c (d  e)))))

(ert-deftest nalist-nalist-p-test/broken-alist-3 ()
  (should-not (nalist-nalist-p '((a . b) (c . d) . (e . f)))))

(ert-deftest nalist-init-test/literal ()
  (with-unbound-symbols (na)
    (nalist-init na '((a . b) (c . d)))
    (should (seq-set-equal-p na '((a . b) (c . d))))))

(ert-deftest nalist-init-test/variable-deep ()
  (with-unbound-symbols (alist na)
    (setq alist (copy-alist '((a . b) (c . d))))
    (nalist-init na alist)
    (should (seq-set-equal-p na '((a . b) (c . d))))
    (setcar na '(x . y))
    (should (seq-set-equal-p alist '((a . b) (c . d))))))

(ert-deftest nalist-init-test/variable-shallow ()
  (with-unbound-symbols (alist na)
    (setq alist (copy-alist '((a . b) (c . d))))
    (nalist-init na alist :shallow t)
    (should (seq-set-equal-p na '((a . b) (c . d))))
    (setcar na '(x . y))
    (should (seq-set-equal-p alist '((x . y) (c . d))))))

(ert-deftest nalist-init-test/not-an-alist ()
  (with-unbound-symbols (na)
    (should-error (nalist-init na 'a))))

(ert-deftest nalist-equal-test/nil-nil ()
  (should (nalist-equal nil nil)))

(ert-deftest nalist-equal-test/nil-atom ()
  (should-assertion-error (nalist-equal nil "foo")))

(ert-deftest nalist-equal-test/atom-nil ()
  (should-assertion-error (nalist-equal "foo" nil)))

(ert-deftest nalist-equal-test/nil-symbol ()
  (should-assertion-error (nalist-equal nil 'foo)))

(ert-deftest nalist-equal-test/symbol-nil ()
  (should-assertion-error (nalist-equal 'foo nil)))

(ert-deftest nalist-equal-test/cons-nil ()
  (should-assertion-error (nalist-equal '(a . b) nil)))

(ert-deftest nalist-equal-test/nil-cons ()
  (should-assertion-error (nalist-equal nil '(a . b))))

(ert-deftest nalist-equal-test/cons-cons-t ()
  (should-assertion-error (nalist-equal '(a . b) '(a . b))))

(ert-deftest nalist-equal-test/alist-alist-t ()
  (should (nalist-equal '((a . b) (c . d)) '((a . b) (c . d)))))

(ert-deftest nalist-equal-test/alist-alist-nil ()
  (should-not (nalist-equal '((a . b) (c . d)) '((c . d) (a . b)))))

(ert-deftest nalist-set-equal-test/nil-nil ()
  (should (nalist-set-equal nil nil)))

(ert-deftest nalist-set-equal-test/nil-atom ()
  (should-assertion-error (nalist-set-equal nil "foo")))

(ert-deftest nalist-set-equal-test/atom-nil ()
  (should-assertion-error (nalist-set-equal "foo" nil)))

(ert-deftest nalist-set-equal-test/nil-symbol ()
  (should-assertion-error (nalist-set-equal nil 'foo)))

(ert-deftest nalist-set-equal-test/symbol-nil ()
  (should-assertion-error (nalist-set-equal 'foo nil)))

(ert-deftest nalist-set-equal-test/cons-nil ()
  (should-assertion-error (nalist-set-equal '(a . b) nil)))

(ert-deftest nalist-set-equal-test/nil-cons ()
  (should-assertion-error (nalist-set-equal nil '(a . b))))

(ert-deftest nalist-set-equal-test/cons-cons ()
  (should-assertion-error (nalist-set-equal '(a . b) '(a . b))))

(ert-deftest nalist-set-equal-test/alist-alist-nil ()
  (should-not (nalist-set-equal '((a . b) (c . d)) '((a . b) (c . x)))))

(ert-deftest nalist-set-equal-test/alist-alist-t ()
  (should (nalist-set-equal '((a . b) (c . d)) '((a . b) (c . d)))))

(ert-deftest nalist-set-equal-test/alist-alist-nil ()
  (should (nalist-set-equal '((a . b) (c . d)) '((c . d) (a . b)))))

(ert-deftest nalist-map-test/not-a-nalist ()
  (with-unbound-symbols (na)
    (setq na (make-hash-table)))
  (let ((res nil))
    (should-assertion-error (nalist-map #'(lambda (k v) (push k res)) na))))

(ert-deftest nalist-map-test/nil ()
  (with-unbound-symbols (na)
    (setq na nil)
    (let ((res nil))
      (nalist-map #'(lambda (k v) (push k res)) na)
      (should (eq res nil)))))

(ert-deftest nalist-map-test/nalist-one ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b))))
    (let ((res nil))
      (nalist-map #'(lambda (k v) (push k res)) na)
      (should (seq-set-equal-p res '(a))))))

(ert-deftest nalist-map-test/nalist-two ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b) (c . d))))
    (let ((res nil))
      (nalist-map #'(lambda (k v) (push k res)) na)
      (should (seq-set-equal-p res '(a c))))))

(ert-deftest nalist-map-test/nalist-many ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b) (c . d) (e . f) (g . h)
                           (j . k) (l . m) (n . o) (p . q)
                           (r . s) (t . u) (v . w) (x . y)
                           (z . 0) (1 . 2) (3 . 4) (5 . 6))))
    (let ((res nil))
      (nalist-map #'(lambda (k v) (push k res)) na)
      (should (seq-set-equal-p res '(a c e g j l n p r t v x z 1 3 5))))))

(ert-deftest nalist-pop-test/nil ()
  (with-unbound-symbols (na)
    (setq na nil)
    (should (eq (nalist-pop 'k na) nil))))

(ert-deftest nalist-pop-test/non-existent-key ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b) (c . d) (e . f) (g . h))))
    (should (eq (nalist-pop 'k na) nil))
    (should (seq-set-equal-p na '((a . b) (c . d) (e . f) (g . h))))))

(ert-deftest nalist-pop-test/existent-key-one ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b))))
    (should (eq (nalist-pop 'a na) 'b))
    (should (eq na nil))))

(ert-deftest nalist-pop-test/existent-key-two ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b) (c . d))))
    (should (eq (nalist-pop 'a na) 'b))
    (should (seq-set-equal-p na '((c . d))))))

(ert-deftest nalist-pop-test/existent-key-many ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b) (c . d) (e . f) (g . h)
                           (j . k) (l . m) (n . o) (p . q)
                           (r . s) (t . u) (v . w) (x . y)
                           (z . 0) (1 . 2) (3 . 4) (5 . 6))))
    (should (eq (nalist-pop 'e na) 'f))
    (should (seq-set-equal-p na '((a . b) (c . d)         (g . h)
                                  (j . k) (l . m) (n . o) (p . q)
                                  (r . s) (t . u) (v . w) (x . y)
                                  (z . 0) (1 . 2) (3 . 4) (5 . 6))))))

(ert-deftest nalist-poppair-test/nil ()
  (with-unbound-symbols (na)
    (setq na nil)
    (should (eq (nalist-poppair na) nil))))

(ert-deftest nalist-poppair-test-one ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b))))
    (should (equal (nalist-poppair na) '(a . b)))
    (should (eq na nil))))

(ert-deftest nalist-poppair-test-many ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b) (c . d) (e . f) (g . h)
                           (j . k) (l . m) (n . o) (p . q)
                           (r . s) (t . u) (v . w) (x . y)
                           (z . 0) (1 . 2) (3 . 4) (5 . 6))))
    (should (equal (nalist-poppair na) '(a . b)))
    (should (seq-set-equal-p na '((c . d) (e . f) (g . h)
                                  (j . k) (l . m) (n . o) (p . q)
                                  (r . s) (t . u) (v . w) (x . y)
                                  (z . 0) (1 . 2) (3 . 4) (5 . 6))))))

(ert-deftest nalist-copy-test/deep-nil ()
  (with-unbound-symbols (na nb)
    (setq na nil)
    (nalist-copy na nb)
    (should (eq nb nil))))

(ert-deftest nalist-copy-test/shallow-nil ()
  (with-unbound-symbols (na nb)
    (setq na nil)
    (nalist-copy na nb :shallow t)
    (should (eq nb nil))))

(ert-deftest nalist-copy-test/deep-one ()
  (with-unbound-symbols (na nb)
    (setq na nil)
    (nalist-copy na nb)
    (should (eq nb nil))))

(ert-deftest nalist-copy-test/not-a-nalsit ()
  (with-unbound-symbols (na nb)
    (setq na '(a b c))
    (should-assertion-error (nalist-copy na nb))))

(ert-deftest nalist-copy-test ()
  (with-nalist-fixture
   (nalist-copy nal nal-deep)
   (nalist-copy nal nal-shallow :shallow t)
   (should (eq nal nal-shallow))
   (should-not (eq nal nal-deep))
   (should (equal nal nal-deep))
   (setq not-nalist 'a)
   (should-assertion-error (nalist-copy not-nalist nal-new))))

(ert-deftest nalist-values-test ()
  (with-nalist-fixture
   (should (seq-set-equal-p (nalist-values nal) '(b d)))))

(ert-deftest nalist-pairs-test ()
  (with-nalist-fixture
   (should (nalist-set-equal (nalist-pairs nal) '((a . b) (c . d))))
   (should-not (eq (nalist-pairs nal) nal))))

(ert-deftest nalist-keys-test ()
  (with-nalist-fixture
   (should (seq-set-equal-p (nalist-keys nal) '(a c)))))

(ert-deftest nalist-subset-p-test ()
  (should (nalist-subset-p '((a . 1)) '((b . 2) (a . 1)))))

(ert-deftest nalist-equal-test ()
  (should (nalist-set-equal '((a . 1) (b . 2)) '((b . 2) (a . 1)))))

(ert-deftest nalist-clear-test ()
  (with-nalist-fixture
   (nalist-clear nal)
   (should (eq nal nil))))

(ert-deftest nalist-get-test ()
  (with-nalist-fixture
   (should (eq (nalist-get 'a nal) 'b))
   (should (eq (nalist-get 'b nal) nil))
   (should (eq (nalist-get 'c nal) 'd))
   (should (eq (nalist-get 'a nal :testfn 'eq) 'b))
   (should (eq (nalist-get 'b nal :testfn 'eq) nil))
   (should (eq (nalist-get 1 nal-eql) 'a))
   (should (eq (nalist-get 1 nal-eql :testfn 'eql) 'a))
   (should (eq (nalist-get 1.0 nal-eql) nil))
   (should (eq (nalist-get 1.0 nal-eql :testfn 'eql) 'b))
   (should (eq (nalist-get "foo" nal-equal :testfn 'equal) 3))
   (should (eq (nalist-get '(a (b c)) nal-equal :testfn 'equal) 'd))
   (should (eq (nalist-get "Hi" nal-equal :testfn #'(lambda (x y) (= (length x) (length y)))) 'd))
   (should (eq (nalist-get 'a nal :default 'no-value) 'b))
   (should (eq (nalist-get 'f nal :default 'no-value) 'no-value))))

(ert-deftest nalist-set-test ()
  (with-nalist-fixture
   (nalist-set 'e 'f nal)
   (should (nalist-set-equal nal '((a . b) (c . d) (e . f))))
   (nalist-set 'c 'g nal)
   (should (nalist-set-equal nal '((a . b) (c . g) (e . f))))))

(ert-deftest lexical-binding-test ()
  (setq closure nil)
  (nalist-init nal '((v . w) (x . y)))
  (let ((nal nil))
    (nalist-init nal '((a . b) (c . d)))
    (setq closure #'(lambda (key value)
                      (nalist-set key value nal)
                      nal)))
  (should (nalist-set-equal nal '((v . w) (x . y))))
  (should (nalist-set-equal (funcall closure 'a 'e) '((a . e) (c . d))))
  (should (nalist-set-equal nal '((v . w) (x . y)))))

(ert-deftest nalist-remove-test ()
  (with-nalist-fixture
   (nalist-remove 'a nal)
   (should (nalist-set-equal nal '((c . d))))))

;;; nalist-test.el ends here
