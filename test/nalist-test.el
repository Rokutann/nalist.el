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

;;; nalist-pairp

(ert-deftest nalist-pairp-test/nil ()
  (should-not (nalist-pairp nil)))

(ert-deftest nalist-pairp-test/symbol ()
  (should-not (nalist-pairp 'a)))

(ert-deftest nalist-pairp-test/string-as-non-trivial-atom ()
  (should-not (nalist-pairp "foo")))

(ert-deftest nalist-pairp-test/cons ()
  (should (nalist-pairp '(a . b))))

(ert-deftest nalist-pairp-test/list ()
  (should (nalist-pairp '(a b))))

;;; nalist-proper-list-p

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

;;; nalist-nalist-p

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

;;; nalist-init

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

(ert-deftest nalist-init-test/let-scope-hygienic ()
  (with-unbound-symbols (alist blist na)
    (setq alist (copy-alist '((a . b) (c . d))))
    (setq blist (copy-alist '((x . y) (v . w))))
    (nalist-init na alist)
    (should (seq-set-equal-p na '((a . b) (c . d))))
    (let ((na nil))
      (nalist-init na blist)
      (should (seq-set-equal-p na '((x . y) (v . w)))))
    (should (seq-set-equal-p na '((a . b) (c . d))))))

(ert-deftest nalist-init-test/lexical-binding-hygienic ()
  (with-unbound-symbols (alist blist na closure)
    (setq alist (copy-alist '((a . b) (c . d))))
    (nalist-init na alist)
    (let ((na nil)
          (blist (copy-alist '((x . y) (v . w)))))
      (setq closure #'(lambda ()
                        (nalist-init na blist)
                        na)))
    (should (seq-set-equal-p (funcall closure) '((x . y) (v . w))))))

(ert-deftest nalist-init-test/buffer-local-hygienic ()
  (with-unbound-symbols (alist blist na closure)
    (with-temp-buffers (buf)
      (setq alist (copy-alist '((a . b) (c . d))))
      (nalist-init na alist)
      (with-current-buffer buf
        (nalist-make-local-variable na)
        (setq blist (copy-alist '((v . w) (x . y))))
        (nalist-init na blist))
      (should (seq-set-equal-p na '((a . b) (c . d)))))))

;;; nalist-make-local-variable

(ert-deftest nalist-make-local-variable-test ()
  (with-unbound-symbols (na)
    (with-temp-buffers (buf)
      (setq na (copy-alist '((a . b))))
      (should-not (local-variable-if-set-p 'na))
      (with-current-buffer buf
        (nalist-make-local-variable na)
        (should (local-variable-p 'na))
        (should (equal na '((a . b)))))
      (should-not (local-variable-p 'nal)))))

;;; nalist-make-variable-buffer-local

(ert-deftest nalit-make-variable-buffer-local-test/2 ()
  (cl-macrolet ((test-with-temp-variable
                 ()
                 (let ((na (cl-gentemp "na")))
                   `(with-unbound-symbols (,na)
                      (with-temp-buffers (buf buf2)
                        (setq ,na (copy-alist '((a . b))))
                        (should-not (local-variable-if-set-p ',na))
                        (with-current-buffer buf
                          (nalist-make-variable-buffer-local ,na)
                          (should (local-variable-if-set-p ',na))
                          (should (seq-set-equal-p ,na '((a . b))))
                          (setq ,na (copy-alist '((x . y)))))
                        (with-current-buffer buf2
                          (should (eq ,na nil))))))))
    (test-with-temp-variable)))

;;; nalist-equal

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

;;; nalist-set-equal

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

;;; nalist-map

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

(ert-deftest nalist-map-test/let-scope-hygienic ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b))))
    (let ((res nil))
      (nalist-map #'(lambda (k v) (push k res)) na)
      (should (seq-set-equal-p res '(a)))
      (let ((na (copy-alist '((v . w))))
            (res nil))
        (nalist-map #'(lambda (k v) (push k res)) na)
        (should (seq-set-equal-p res '(v)))))))

;;; nalist-pop

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

(ert-deftest nalist-pop-test/let-scope-hygienic ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b) (c . d))))
    (let ((na (copy-alist '((v . w) (x . y)))))
      (should (eq (nalist-pop 'v na) 'w))
      (should (seq-set-equal-p na '((x . y)))))
    (should (seq-set-equal-p na '((a . b) (c . d))))))

(ert-deftest nalist-pop-test/lexical-binding-hygienic ()
  (with-unbound-symbols (na closure)
    (setq na (copy-alist '((a . b) (c . d))))
    (let ((na (copy-alist '((v . w) (x . y)))))
      (setq closure #'(lambda (key)
                        (nalist-pop key na))))
    (should (eq (funcall closure 'v) 'w))
    (should (seq-set-equal-p na '((a . b) (c . d))))))

(ert-deftest nalist-pop-test/buffer-local-hygienic ()
  (with-unbound-symbols (na)
    (with-temp-buffers (buf)
      (setq na (copy-alist '((a . b) (c . d))))
      (with-current-buffer buf
        (nalist-make-local-variable na)
        (setq na (copy-alist '((v . w) (x . y))))
        (should (eq (nalist-pop 'v na) 'w))
        (should (seq-set-equal-p na '((x . y)))))
      (should (seq-set-equal-p na '((a . b) (c . d)))))))

;;; nalist-poppair

(ert-deftest nalist-poppair-test/not-a-nalist ()
  (with-unbound-symbols (na)
    (setq na '(a b c))
    (should-assertion-error (nalist-poppair na))))

(ert-deftest nalist-poppair-test/nil ()
  (with-unbound-symbols (na)
    (setq na nil)
    (should (eq (nalist-poppair na) nil))))

(ert-deftest nalist-poppair-test/one ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b))))
    (should (equal (nalist-poppair na) '(a . b)))
    (should (eq na nil))))

(ert-deftest nalist-poppair-test/many ()
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

(ert-deftest nalist-poppair-test/let-scope-hygienic ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b) (c . d))))
    (let ((na (copy-alist '((v . w) (x . y)))))
      (should (equal (nalist-poppair na) '(v . w)))
      (should (seq-set-equal-p na '((x . y)))))
    (should (seq-set-equal-p na '((a . b) (c . d))))))

(ert-deftest nalist-poppair-test/lexical-binding-hygienic ()
  (with-unbound-symbols (na closure)
    (setq na (copy-alist '((a . b) (c . d))))
    (let ((na (copy-alist '((v . w) (x . y)))))
      (setq closure #'(lambda ()
                        (nalist-poppair na))))
    (should (equal (funcall closure) '(v . w)))))

(ert-deftest nalist-poppair-test/buffer-local-hygienic ()
  (with-unbound-symbols (na)
    (with-temp-buffers (buf)
      (setq na (copy-alist '((a . b) (c . d))))
      (with-current-buffer buf
        (nalist-make-local-variable na)
        (setq na (copy-alist '((v . w) (x . y))))
        (should (equal (nalist-poppair na) '(v . w)))
        (should (seq-set-equal-p na '((x . y)))))
      (should (seq-set-equal-p na '((a . b) (c . d)))))))

;;; nalist-copy

(ert-deftest nalist-copy-test/not-a-nalsit ()
  (with-unbound-symbols (na nb)
    (setq na '(a b c))
    (should-assertion-error (nalist-copy na nb))))

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
    (setq na (copy-alist '((a . b))))
    (nalist-copy na nb)
    (should (seq-set-equal-p nb '((a . b))))
    (should-not (eq na nb))))

(ert-deftest nalist-copy-test/shallow-one ()
  (with-unbound-symbols (na nb)
    (setq na (copy-alist '((a . b))))
    (nalist-copy na nb :shallow t)
    (should (seq-set-equal-p nb '((a . b))))
    (should (eq na nb))))

(ert-deftest nalist-copy-test/let-scope-hygienic ()
  (with-unbound-symbols (na nb)
    (setq na (copy-alist '((a . b)))
          nb (copy-alist '((x . y))))
    (let ((na (copy-alist '((1 . 2))))
          (nb nil))
      (nalist-copy na nb)
      (should (seq-set-equal-p nb '((1 . 2)))))
    (should (seq-set-equal-p nb '((x . y))))))

(ert-deftest nalist-copy-test/lexical-binding-hygienic ()
  (with-unbound-symbols (na nb closure)
    (setq na (copy-alist '((a . b)))
          nb (copy-alist '((1 . 2))))
    (let ((na (copy-alist '((x . y))))
          (nb nil))
      (setq closure #'(lambda ()
                        (nalist-copy na nb)
                        nb)))
    (should (seq-set-equal-p (funcall closure) '((x . y))))
    (should (seq-set-equal-p nb '((1 . 2))))))

(ert-deftest nalist-copy-test/let-scope-hygienic ()
  (with-unbound-symbols (na nb)
    (with-temp-buffers (buf)
      (setq na (copy-alist '((a . b)))
            nb (copy-alist '((x . y))))
      (with-current-buffer buf
        (nalist-make-local-variable na)
        (nalist-make-local-variable nb)
        (setq na (copy-alist '((1 . 2)))
              nb nil)
        (nalist-copy na nb)
        (should (seq-set-equal-p nb '((1 . 2)))))
      (should (seq-set-equal-p nb '((x . y)))))))

;;; nalist-values

(ert-deftest nalist-values-test/not-a-nalist ()
  (with-unbound-symbols (na)
    (setq na '(a b))
    (should-assertion-error (nalist-values na))))

(ert-deftest nalist-values-test/nil ()
  (with-unbound-symbols (na)
    (setq na nil)
    (should (eq (nalist-values na) nil))))

(ert-deftest nalist-values-test/one ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b))))
    (should (seq-set-equal-p (nalist-values na) '(b)))))

(ert-deftest nalist-values-test/two ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b) (c . d))))
    (should (seq-set-equal-p (nalist-values na) '(b d)))))

(ert-deftest nalist-values-test/many ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b) (c . d) (e . f) (g . h)
                           (j . k) (l . m) (n . o) (p . q)
                           (r . s) (t . u) (v . w) (x . y)
                           (z . 0) (1 . 2) (3 . 4) (5 . 6))))
    (should (seq-set-equal-p (nalist-values na)
                             '(b d f h k m o q s u w y 0 2 4 6)))))

(ert-deftest nalist-values-test/let-scope-hygienic ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b))))
    (let ((na (copy-alist '((a . c)))))
      (should (seq-set-equal-p (nalist-values na) '(c))))))

;;; nalist-pairs

(ert-deftest nalist-pairs-test/not-a-nalist ()
  (with-unbound-symbols (na)
    (setq na '(a b))
    (should-assertion-error (nalist-pairs na))))

(ert-deftest nalist-pairs-test/nil ()
  (with-unbound-symbols (na)
    (setq na nil)
    (should (eq (nalist-pairs na) nil))))

(ert-deftest nalist-pairs-test/one ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b))))
    (should (seq-set-equal-p (nalist-pairs na) '((a . b))))
    (should-not (eq na (nalist-pairs na)))))

(ert-deftest nalist-pairs-test/two ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b) (c . d))))
    (should (seq-set-equal-p (nalist-pairs na) '((a . b) (c . d))))
    (should-not (eq na (nalist-pairs na)))))

(ert-deftest nalist-pairs-test/many ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b) (c . d) (e . f) (g . h)
                           (j . k) (l . m) (n . o) (p . q)
                           (r . s) (t . u) (v . w) (x . y)
                           (z . 0) (1 . 2) (3 . 4) (5 . 6))))
    (should (seq-set-equal-p (nalist-pairs na) na))
    (should-not (eq (nalist-pairs na) na ))))

(ert-deftest nalist-pairs-test/let-scope-hygienic ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b))))
    (let ((na (copy-alist '((x . y)))))
      (should (seq-set-equal-p (nalist-pairs na) '((x . y))))
      )))

;;; nalist-keys

(ert-deftest nalist-keys-test/not-a-nalist ()
  (with-unbound-symbols (na)
    (setq na '(a b))
    (should-assertion-error (nalist-keys na))))

(ert-deftest nalist-keys-test/nil ()
  (with-unbound-symbols (na)
    (setq na nil)
    (should (eq (nalist-keys na) nil))))

(ert-deftest nalist-keys-test/one ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b))))
    (should (seq-set-equal-p (nalist-keys na) '(a)))))

(ert-deftest nalist-keys-test/two ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b) (c . d))))
    (should (seq-set-equal-p (nalist-keys na) '(a c)))))

(ert-deftest nalist-keys-test/many ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b) (c . d) (e . f) (g . h)
                           (j . k) (l . m) (n . o) (p . q)
                           (r . s) (t . u) (v . w) (x . y)
                           (z . 0) (1 . 2) (3 . 4) (5 . 6))))
    (should (seq-set-equal-p (nalist-keys na)
                             '(a c e g j l n p r t v x z 1 3 5)))))

(ert-deftest nalist-keys-test/let-scope-hygienic ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b))))
    (let ((na (copy-alist '((x . y)))))
      (should (seq-set-equal-p (nalist-keys na) '(x))))))

;;; nalist-subset-p

(ert-deftest nalist-subset-p-test/nalist-a-is-not-a-nalist ()
  (with-unbound-symbols (na nb)
    (setq na (copy-alist '(a b)))
    (setq nb (copy-alist '((a . b))))
    (should-assertion-error (nalist-subset-p na nb))))

(ert-deftest nalist-subset-p-test/nalist-b-is-not-a-nalist ()
  (with-unbound-symbols (na nb)
    (setq na (copy-alist '((a . b))))
    (setq nb (copy-alist '(a b)))
    (should-assertion-error (nalist-subset-p na nb))))

(ert-deftest nalist-subset-p-test/nil-nil ()
  (with-unbound-symbols (na nb)
    (setq na nil)
    (setq nb nil)
    (should (nalist-subset-p na nb))))

(ert-deftest nalist-subset-p-test/nil-one ()
  (with-unbound-symbols (na nb)
    (setq na nil)
    (setq nb (copy-alist '((a . b))))
    (should (nalist-subset-p na nb))))

(ert-deftest nalist-subset-p-test/one-nil ()
  (with-unbound-symbols (na nb)
    (setq na (copy-alist '((a . b))))
    (setq nb nil)
    (should-not (nalist-subset-p na nb))))

(ert-deftest nalist-subset-p-test/one-one-t ()
  (with-unbound-symbols (na nb)
    (setq na (copy-alist '((a . b))))
    (setq nb (copy-alist '((a . b))))
    (should (nalist-subset-p na nb))))

(ert-deftest nalist-subset-p-test/one-one-nil ()
  (with-unbound-symbols (na nb)
    (setq na (copy-alist '((a . b))))
    (setq nb (copy-alist '((a . x))))
    (should-not (nalist-subset-p na nb))))

(ert-deftest nalist-subset-p-test/one-two-t ()
  (with-unbound-symbols (na nb)
    (setq na (copy-alist '((a . b))))
    (setq nb (copy-alist '((a . b) (c . d))))
    (should (nalist-subset-p na nb))))

(ert-deftest nalist-subset-p-test/one-two-nil ()
  (with-unbound-symbols (na nb)
    (setq na (copy-alist '((a . b))))
    (setq nb (copy-alist '((a . x) (c . d))))
    (should-not (nalist-subset-p na nb))))

(ert-deftest nalist-subset-p-test/two-one ()
  (with-unbound-symbols (na nb)
    (setq na (copy-alist'((a . b) (c . d))))
    (setq nb (copy-alist '((a . b))))
    (should-not (nalist-subset-p na nb))))

(ert-deftest nalist-subset-p-test/many-many-t ()
  (with-unbound-symbols (na nb)
    (setq na (copy-alist '((c . d) (l . m) (e . f) (3 . 4)
                           (j . k) (v . w) (5 . 6) (p . q)
                           (r . s) (a . b) (x . y) (g . h)
                           (z . 0) (n . o) (1 . 2) )))
    (setq nb (copy-alist '((a . b) (c . d) (e . f) (g . h)
                           (j . k) (l . m) (n . o) (p . q)
                           (r . s) (t . u) (v . w) (x . y)
                           (z . 0) (1 . 2) (3 . 4) (5 . 6))))
    (should (nalist-subset-p na nb))))

(ert-deftest nalist-subset-p-test/many-many-nil ()
  (with-unbound-symbols (na nb)
    (setq na (copy-alist '((c . d) (l . m) (e . f) (3 . 4)
                           (j . k) (v . w) (5 . 6) (p . q)
                           (r . s) (a . b) (x . y) (g . h)
                           (z . 0) (n . o) (1 . 100) )))
    (setq nb (copy-alist '((a . b) (c . d) (e . f) (g . h)
                           (j . k) (l . m) (n . o) (p . q)
                           (r . s) (t . u) (v . w) (x . y)
                           (z . 0) (1 . 2) (3 . 4) (5 . 6))))
    (should-not (nalist-subset-p na nb))))

(ert-deftest nalist-subset-p-test/let-scope-hygienic ()
  (with-unbound-symbols (na nb)
    (setq na (copy-alist '((a . b))))
    (setq nb (copy-alist '((a . b) (c . d))))
    (let ((na (copy-alist '((x . y)))))
      (should-not (nalist-subset-p na nb)))
    (let ((nb (copy-alist '((c . d)))))
      (should-not (nalist-subset-p na nb)))))

;;; nalist-clear

(ert-deftest nalist-clear-test ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b))))
    (nalist-clear na)
    (should (eq na nil))))

(ert-deftest nalist-clear-test/let-scope-hygienic ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b))))
    (let ((na (copy-alist '((c . d)))))
      (nalist-clear na)
      (should (eq na nil)))
    (should (seq-set-equal-p na '((a . b))))))

(ert-deftest nalist-clear-test/lexical-binding-hygienic ()
  (with-unbound-symbols (na closure)
    (setq na (copy-alist '((a . b))))
    (let ((na (copy-alist '((x . y)))))
      (setq closure #'(lambda ()
                        (nalist-clear na)
                        na)))
    (should (eq (funcall closure) nil))
    (should (seq-set-equal-p na '((a . b))))))

(ert-deftest nalist-clear-test/buffer-local-hygienic ()
  (with-unbound-symbols (na)
    (with-temp-buffers (buf)
      (setq na (copy-alist '((a . b))))
      (with-current-buffer buf
        (nalist-make-local-variable na)
        (nalist-clear na)
        (should (eq na nil)))
      (should (seq-set-equal-p na '((a . b)))))))

;;; naslit-get

(ert-deftest nalist-get-test/nil ()
  (with-unbound-symbols (na)
    (setq na nil)
    (should (eq (nalist-get 'a na) nil))))

(ert-deftest nalist-get-test/one ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b))))
    (should (eq (nalist-get 'a na) 'b))))

(ert-deftest nalist-get-test/two ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b) (c . d))))
    (should (eq (nalist-get 'c na) 'd))))

(ert-deftest nalist-get-test/many ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b) (c . d) (e . f) (g . h)
                           (j . k) (l . m) (n . o) (p . q)
                           (r . s) (t . u) (v . w) (x . y)
                           (z . 0) (1 . 2) (3 . 4) (5 . 6))))
    (should (eq (nalist-get 'z na) 0))))

(ert-deftest nalist-get-test/testfn ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '(("a" . b))))
    (should (eq (nalist-get "a" na :testfn 'equal) 'b))))

(ert-deftest nalist-get-test/let-scope-hygienic ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b))))
    (let ((na (copy-alist '((x . y)))))
      (should (eq (nalist-get 'x na) 'y)))))

(ert-deftest nalist-get-test/lexical-binding-hygienic ()
  (with-unbound-symbols (na closure)
    (setq na (copy-alist '((a . b) (c . d) (e . f))))
    (let ((na (copy-alist '((a . b) (c . d) (x . y)))))
      (setq closure #'(lambda (key)
                        (nalist-get key na))))
    (should (eq (funcall closure 'x) 'y))))

(ert-deftest nalist-get-test/buffer-local-hygienic ()
  (with-unbound-symbols (na)
    (with-temp-buffers (buf)
      (setq na (copy-alist '((a . b))))
      (with-current-buffer buf
        (nalist-make-local-variable na)
        (setq na (copy-alist '((x . y))))
        (should (eq (nalist-get 'x na) 'y))))))

;;; nalist-set

(ert-deftest nalist-set-test/nil ()
  (with-unbound-symbols (na)
    (setq na nil)
    (nalist-set 'a 'b na)
    (should (seq-set-equal-p na '((a . b))))))

(ert-deftest nalist-set-test/non-existent-key-one ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b))))
    (nalist-set 'c 'd na)
    (should (seq-set-equal-p na '((a . b) (c . d))))))

(ert-deftest nalist-set-test/non-existent-key-many ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b) (c . d) (e . f) (g . h)
                           (j . k) (l . m) (n . o) (p . q)
                           (r . s) (t . u) (v . w) (x . y)
                           (z . 0) (1 . 2) (3 . 4) (5 . 6))))
    (nalist-set 100 'a na)
    (should (seq-set-equal-p na '((a . b) (c . d) (e . f) (g . h)
                                  (j . k) (l . m) (n . o) (p . q)
                                  (r . s) (t . u) (v . w) (x . y)
                                  (z . 0) (1 . 2) (3 . 4) (5 . 6)
                                  (100 . a))))))

(ert-deftest nalist-set-test/existent-key-one ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b))))
    (nalist-set 'a 'c na)
    (should (seq-set-equal-p na '((a . c))))))

(ert-deftest nalist-set-test/existent-key-many ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b) (c . d) (e . f) (g . h)
                           (j . k) (l . m) (n . o) (p . q)
                           (r . s) (t . u) (v . w) (x . y)
                           (z . 0) (1 . 2) (3 . 4) (5 . 6))))
    (nalist-set 3 100 na)
    (should (seq-set-equal-p na '((a . b) (c . d) (e . f) (g . h)
                                  (j . k) (l . m) (n . o) (p . q)
                                  (r . s) (t . u) (v . w) (x . y)
                                  (z . 0) (1 . 2) (3 . 100) (5 . 6))))))

(ert-deftest nalist-set-test/testfn ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '(("a" . b) ("c" . d))))
    (nalist-set "a" 'c na :testfn 'equal)
    (should (seq-set-equal-p na '(("a" . c) ("c" . d))))))

(ert-deftest nalist-set-test/let-scope-hygienic ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b))))
    (let ((na (copy-alist '((a . b)))))
      (nalist-set 'a 'c na)
      (should (seq-set-equal-p na '((a . c)))))
    (should (seq-set-equal-p na '((a . b))))))

(ert-deftest nalist-set-test/lexical-binding-hygienic ()
  (with-unbound-symbols (na closure)
    (setq na (copy-alist '((a . b) (c . d) (e . f))))
    (let ((na (copy-alist '((a . b) (c . d) (x . y)))))
      (setq closure #'(lambda (key value)
                        (nalist-set key value na)
                        na)))
    (should (seq-set-equal-p (funcall closure 'a 10) '((a . 10) (c . d) (x . y))))
    (should (seq-set-equal-p na '((a . b) (c . d) (e . f))))))

(ert-deftest nalist-set-test/buffer-local-hygienic ()
  (with-unbound-symbols (na)
    (with-temp-buffers (buf)
      (setq na (copy-alist '((a . b))))
      (with-current-buffer buf
        (nalist-make-local-variable na)
        (nalist-set 'a 'c na)
        (should (seq-set-equal-p na '((a . c)))))
      (should (seq-set-equal-p na '((a . b)))))))

;;; nalist-remove

(ert-deftest nalist-remove-test/nil ()
  (with-unbound-symbols (na)
    (setq na nil)
    (nalist-remove 'a na)
    (should (eq na nil))))

(ert-deftest nalist-remove-test/non-existent-key-one ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b))))
    (nalist-remove 'c na)
    (should (seq-set-equal-p na '((a . b))))))

(ert-deftest nalist-remove-test/non-existent-key-many ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b) (c . d) (e . f) (g . h)
                           (j . k) (l . m) (n . o) (p . q)
                           (r . s) (t . u) (v . w) (x . y)
                           (z . 0) (1 . 2) (3 . 4) (5 . 6))))
    (nalist-remove 100 na)
    (should (seq-set-equal-p na '((a . b) (c . d) (e . f) (g . h)
                                  (j . k) (l . m) (n . o) (p . q)
                                  (r . s) (t . u) (v . w) (x . y)
                                  (z . 0) (1 . 2) (3 . 4) (5 . 6))))))

(ert-deftest nalist-remove-test/existent-key-one ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b))))
    (nalist-remove 'a na)
    (should (eq na nil))))

(ert-deftest nalist-remove-test/existent-key-many ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b) (c . d) (e . f) (g . h)
                           (j . k) (l . m) (n . o) (p . q)
                           (r . s) (t . u) (v . w) (x . y)
                           (z . 0) (1 . 2) (3 . 4) (5 . 6))))
    (nalist-remove 3 na)
    (should (seq-set-equal-p na '((a . b) (c . d) (e . f) (g . h)
                                  (j . k) (l . m) (n . o) (p . q)
                                  (r . s) (t . u) (v . w) (x . y)
                                  (z . 0) (1 . 2)         (5 . 6))))))

(ert-deftest nalist-remove-test/testfn ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '(("a" . b) ("c" . d))))
    (nalist-remove "a" na :testfn 'equal)
    (should (seq-set-equal-p na '(("c" . d))))))

(ert-deftest nalist-remove-test/let-scope-hygienic ()
  (with-unbound-symbols (na)
    (setq na (copy-alist '((a . b) (c . d))))
    (let ((na (copy-alist '((a . b) (c . d)))))
      (nalist-remove 'a na)
      (should (seq-set-equal-p na '((c . d)))))
    (should (seq-set-equal-p na '((a . b) (c . d))))))

(ert-deftest nalist-remove-test/lexical-binding-hygienic ()
  (with-unbound-symbols (na closure)
    (setq na (copy-alist '((a . b) (c . d) (e . f))))
    (let ((na (copy-alist '((a . b) (c . d) (x . y)))))
      (setq closure #'(lambda (key)
                        (nalist-remove key na)
                        na)))
    (should (seq-set-equal-p (funcall closure 'a) '((c . d) (x . y))))
    (should (seq-set-equal-p na '((a . b) (c . d) (e . f))))))

(ert-deftest nalist-remove-test/buffer-local-hygienic ()
  (with-unbound-symbols (na)
    (with-temp-buffers (buf)
      (setq na (copy-alist '((a . b) (c . d))))
      (with-current-buffer buf
        (nalist-make-local-variable na)
        (nalist-remove 'a na)
        (should (seq-set-equal-p na '((c . d)))))
      (should (seq-set-equal-p na '((a . b) (c . d)))))))

;;; nalist-test.el ends here
