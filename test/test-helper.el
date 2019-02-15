;;; test-helper.el --- Nalist: Helpers for the tests. -*- lexical-binding: t; -*-
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

(defmacro should-assertion-error (&rest body)
  "Make `cl-assert' raise an error while executing BODY."
  `(let ((debug-on-error nil))
     (should-error
      ,@body)))

(defun nalist-helper-compose-unbound-forms (sym-list)
  "Compose forms from SYM-LIST for `with-unbound-symbols'."
  (mapcar #'(lambda (sym)
              `(makunbound ',sym))
          sym-list))

(defmacro with-unbound-symbols (sym-list &rest body)
  "Unbound all the vars in SYM-LIST before executing BODY."
  (declare (indent 1))
  `(progn
     ,@(nalist-helper-compose-unbound-forms sym-list)
     ,@body))

(defun nalist-helper-compose-forms (var-list)
  "Compose forms from VAR-LIST for `with-temp-conses'."
  (mapcar #'(lambda (var)
              `(nalist-helper-totally-destruct-cons ,var))
          var-list))

(defmacro with-temp-conses (var-list &rest body)
  "Destruct all conses the variables in VAR-LIST bound, after executing BODY."
  (declare (indent 1))
  `(unwind-protect
       (progn
         ,@body)
     ,@(nalist-helper-compose-forms var-list)))

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

(defmacro nalist-helper-totally-destruct-cons (obj)
  "Totally destruct OBJ.

OBJ should be a cons cell or a symbol refers a con cell."
  `(prog1
       (unless (atom ,obj)
         (nalist-helper-chop-all-leaves ,obj))
     (when (symbolp ',obj)
       (setq ,obj nil))
     (garbage-collect)))

(defun nalist-helper-chop-all-leaves (cons-cell)
  "Dereference all cells of CONS-CELL from tips to the root."
  (unless (or (null cons-cell)
              (equal cons-cell '(nil . nil)))
    (when (nalist-helper-car-has-leaves cons-cell)
      (nalist-helper-chop-all-leaves (car cons-cell)))
    (when (nalist-helper-cdr-has-leaves cons-cell)
      (nalist-helper-chop-all-leaves (cdr cons-cell)))
    (nalist-helper-chop-all-leaves cons-cell)))

(defun nalist-helper-cdr-has-leaves (cons-cell)
  "Return t if the cdr of CONS-CELL points more than an atom or '(nil).

If the cdr poinsts nil, just return nil.  If the cdr points a
non-nil atom or '(nil), `setcdr' nil and return nil.  Otherwise,
return t."
  (cond ((null (cdr cons-cell)))
        ((atom (cdr cons-cell))
         (setcdr cons-cell nil))
        ((and (null (cdar cons-cell))
              (null (cddr cons-cell)))
         (setcdr cons-cell nil))
        (t t)))

(defun nalist-helper-car-has-leaves (cons-cell)
  "Return t if the car of CONS-CELL points more than an atom or '(nil).

If the car points nil, just return nil.  If the car points a
non-nil atom or '(nil), `setcar' nil and return nil.  Otherwise,
return t."
  (cond ((null (car cons-cell)))
        ((atom (car cons-cell))
         (setcar cons-cell nil))
        ((and (null (caar cons-cell))
              (null (cadr cons-cell)))
         (setcar cons-cell nil))
        (t t)))

(defun nalist-helper-destruct-cons (cons-cell)
  "Destruct CONS-CELL completely."
  (cond ((null cons-cell))
        ;; Fisrtly, we deal with the cases one of the item in the
        ;; considerdation is a non-nil atom.  Because applying
        ;; cons-related functions to non-nil atoms causes erros.
        ((and (atom (car cons-cell))
              (atom (cdr cons-cell)))
         (setcar cons-cell nil)
         (setcdr cons-cell nil)
         ;; In case one of these is nil, it's redundant to set it to
         ;; nil. However, it's more efficient to allow it than adding
         ;; conditional branched for it since this is an end of the
         ;; iteration.
         )
        ((atom (car cons-cell))
         (setcar cons-cell nil) ;; This is redundant, when car is nil, but not harmful.
         (if (and (null (cdar cons-cell))
                  (null (cddr cons-cell)))
             (setcdr cons-cell nil)
           (nalist-helper-destruct-cons (cdr cons-cell)))
         (nalist-helper-destruct-cons cons-cell))
        ((atom (cdr cons-cell))
         (setcdr cons-cell nil) ;; This is redundant, when car is nil, but not harmful.
         (if (and (null (caar cons-cell))
                  (null (cadr cons-cell)))
             (setcar cons-cell nil)
           (nalist-helper-destruct-cons (car cons-cell)))
         (nalist-helper-destruct-cons cons-cell))
        ;; Now both car and cdr are consp.
        ((and (null (caar cons-cell))
              (null (cadr cons-cell))
              (null (cdar cons-cell))
              (null (cddr cons-cell)))
         (setcar cons-cell nil)
         (setcdr cons-cell nil))
        ((and (null (caar cons-cell))
              (null (cadr cons-cell)))
         (setcar cons-cell nil)
         (nalist-helper-destruct-cons (cdr cons-cell))
         (nalist-helper-destruct-cons cons-cell))
        ((and (null (cdar cons-cell))
              (null (cddr cons-cell)))
         (setcdr cons-cell nil)
         (nalist-helper-destruct-cons (car cons-cell))
         (nalist-helper-destruct-cons cons-cell))
        (t (nalist-helper-destruct-cons (car cons-cell))
           (nalist-helper-destruct-cons (cdr cons-cell))
           (nalist-helper-destruct-cons cons-cell))))

;;; test-helper.el ends here
