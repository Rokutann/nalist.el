;;; nalist-test-helper.el --- Nalist: Tests for helper.  -*- lexical-binding: t; -*-
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

(ert-deftest nalist-helper-totally-destruct-cons-test/nil ()
  (setq obj nil)
  (nalist-helper-totally-destruct-cons obj)
  (should (eq obj nil)))

(ert-deftest nalist-helper-totally-destruct-cons-test/atom ()
  (setq obj 'a)
  (nalist-helper-totally-destruct-cons obj)
  (should (eq obj nil)))

(ert-deftest nalist-helper-totally-destruct-cons-test/cons ()
  (setq obj '(a . b))
  (setq ref obj)
  (nalist-helper-totally-destruct-cons obj)
  (should (eq obj nil))
  (should (equal ref '(nil))))

(ert-deftest nalist-helper-totally-destruct-cons-test/list ()
  (setq obj '(a b))
  (setq ref obj)
  (nalist-helper-totally-destruct-cons obj)
  (should (eq obj nil))
  (should (equal ref '(nil))))

(ert-deftest nalist-helper-totally-destruct-cons-test/long-list ()
  (setq obj '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
  (setq ref obj)
  (nalist-helper-totally-destruct-cons obj)
  (should (eq obj nil))
  (should (equal ref '(nil))))

(ert-deftest nalist-helper-totally-destruct-cons-test/deep-list ()
  (setq obj '(((((a) b) c) d) (e (f (g (h (i (j))))))) )
  (setq ref obj)
  (nalist-helper-totally-destruct-cons obj)
  (should (eq obj nil))
  (should (equal ref '(nil))))

(ert-deftest nalist-helper-totally-destruct-cons-test/list-literal ()
  (should (eq (nalist-helper-totally-destruct-cons '(a b)) nil)))

(ert-deftest nalist-helper-with-temp-conses/nil ()
  (with-temp-conses nil
      (setq alist '(a . b)))
  (should (equal alist '(a . b))))

(ert-deftest nalist-helper-with-temp-conses/one-var ()
  (with-temp-conses (alist)
      (setq alist '(a . b)))
  (should (eq alist nil)))

(ert-deftest nalist-helper-with-temp-conses/two-vars ()
  (with-temp-conses (alist blist)
    (setq alist '(a . b))
    (setq blist '(c . d)))
  (should (eq alist nil))
  (should (eq blist nil)))

(ert-deftest nalist-helper-with-unbound-vars ()
  (setq var-1 1)
  (setq var-2 2)
  (with-unbound-symbols (var-1 var-2)
    (should-not (boundp 'var-1))
    (should-not (boundp 'var-2)))
  )

;;; nalist-helper-test.el ends here.
