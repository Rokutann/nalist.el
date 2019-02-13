;;; nalist.el --- API for named association lists.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Cyriakus "Mukuge" Hill

;; Author: Cyriakus "Mukuge" Hill <cyriakus.h@gmail.com>
;; Keywords: Lisp, tools
;; URL: https://github.com/mukuge/nalist.el
;; Package-Version: 0.0.1
;; Package-Requires: ((emacs "26.1"))

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

;; API for named association lists.

;;; Code:

(require 'cl-lib)
(require 'seq)

(defun nalist-pairp (obj)
  "Return t if OBJ is a pair, otherwise nil.

A pair is a cons cell, regardless of what Lisp objects its `car'
and `cdr' point."
  (consp obj))

(defun nalist-proper-list-p (obj)
  "Return t if OBJ is a proper list, otherwise nil.

A proper list is a non circular cons chain whose last `cdr' points nil."
  (or
   (null obj)
   (and (consp obj)
        (cl-loop
         for nth-cdr = obj then (cdr nth-cdr)
         for 2nth-cdr = (cdr obj) then (cddr 2nth-cdr)
         when (eq nth-cdr 2nth-cdr) return nil
         ;; A circular list consists of finite positions: When we move
         ;; a pointer along its structure using `cdr', it goes back to
         ;; the same reference point in finite steps.  It's periodic.
         ;; Let's assume the period is N steps.  If we move two
         ;; pointers so that one moves one step at a time starting
         ;; from the position 1, the other moves two steps at a time
         ;; starting from the position 2, the former points the nth
         ;; position, the latter points the 2nth position after n
         ;; iterations.  So, after N iterations, the former and the
         ;; latter point the same position on the circular list
         ;; because it's N-periodic.
         ;;
         when (null nth-cdr) return t
         ;; nth-cdr and 2nth-cdr advance different steps at a
         ;; time. That means there are some points where the nth-cdr
         ;; visit before the 2nth-cdr.  So, need to check nullness of
         ;; the nth-cdr.
         ;;
         when (null 2nth-cdr) return t
         when (not (consp 2nth-cdr)) return nil
         ;; Check for non circular lists if 2nth cdr position is a
         ;; terminus.
         ;;
         when (null (cdr 2nth-cdr)) return t
         when (not (consp (cdr 2nth-cdr))) return nil
         ;; Check for non circular lists if (2n+1)th cdr position is a
         ;; terminus.
         ;;
         when (null (cddr 2nth-cdr)) return t
         when (not (consp (cddr 2nth-cdr))) return nil)
        ;; Check for non circular list if (2n+2)th, or the next 2nth,
        ;; cdr position is a terminus.
        ;;
        ;; Given the above three positions are not a terminus,
        ;; iterate over to the next nth and 2nth cdr positions.
        )))

(defun nalist-nalist-p (obj)
  "Return t if OBJ is an alist, otherwise nil.

An alist, or association list, is a proper list of pairs."
  (and (nalist-proper-list-p obj)
       (let ((res t))
         (mapc #'(lambda (pair)
                   (unless (nalist-pairp pair)
                     (setq res nil)))
               obj)
         res)))

(cl-defmacro nalist-init (symbol alist &key shallow)
  "Bind SYMBOL to ALIST if SHALLOW is non-nil, otherwise to a deep-copy of ALIST."
  `(progn
     (unless (nalist-nalist-p ,alist)
       (error "Invalid initial value `%s'" ,alist))
     (if ,shallow
         (setq ,symbol ,alist)
       (setq ,symbol (copy-alist ,alist)))
     ',symbol))

(defmacro nalist-make-local-variable (nalist)
  "Create a buffer-local binding in the current buffer for NALIST.

This macro binds a deep-copy of the content of the original
NALIST to the buffer-local NALIST to avoid their sharing cons
cells."
  (let ((copy (gensym)))
    `(let ((,copy (copy-alist ,nalist)))
       (make-local-variable ',nalist)
       (setq ,nalist ,copy)))
  ',nalist)

(defmacro nalist-make-variable-buffer-local (nalist)
  "Mark NALIST automatically buffer local.

This macro binds a deep-copy of the content of the original
NALIST to the buffer-local NALIST to avoid their sharing cons
cells.

It also sets the default value of NALIST nil to avoid variables
in other buffers share the cons cells through it."
  (let ((copy (gensym)))
    `(let ((,copy ,nalist))
       (setq ,nalist nil)
       (make-variable-buffer-local ',nalist)
       (setq ,nalist ,copy)))
  ',nalist)

(defmacro nalist-clear (nalist)
  "Set NALIST nil."
  `(setq ,nalist nil))

(cl-defmacro nalist-set (key value nalist &key (testfn 'eq))
  "Find a pair with KEY in NALIST with TESTFN, and set its value VALUE.

It destructively changes the value of KEY into VALUE if their a
pair with KEY already exists in NALIST, otherwise creates a new
pair with KEY and VALUE."
  `(setf (alist-get ,key ,nalist nil nil ',testfn) ,value))

(cl-defun nalist-get (key nalist &key default (testfn 'eq))
  "Return the value of KEY in NALIST if found TESTFN-wise, otherwise DEFAULT."
  (alist-get key nalist default nil testfn))

(cl-defmacro nalist-remove (key nalist &key (testfn 'eq))
  "Remove the pair with KEY from NALIST if found TESTFN-wise."
  `(setf (alist-get ,key ,nalist nil t ',testfn) nil))

(defun nalist-pairs (nalist)
  "Return a list consisting all the pairs in NALIST."
  (copy-alist nalist))

(defun nalist-keys (nalist)
  "Return a list consisting all the keys in NALIST."
  (mapcar 'car nalist))

(defun nalist-values (nalist)
  "Return a list consisting all the values in NALIST."
  (mapcar 'cdr nalist))

(cl-defmacro nalist-copy (nalist-old nalist-new &key shallow)
  "Copy and bind the content of NALIST-OLD to NALIST-NEW.

This macro uses shallow-copy if SHALLOW is non-nil, otherwise
uses deep-copy."
  `(progn
     (unless (nalist-nalist-p ,nalist-old)
       (error "Invalid initial value: `%s'" ,nalist-old))
     (if ,shallow
         (setq ,nalist-new ,nalist-old)
       (setq ,nalist-new (copy-alist ,nalist-old)))
     ',nalist-new))

(defmacro nalist-pop (key nalist)
  "Remove the pair with KEY from NALIST, and return the value of the pair."
  (let ((before (gensym))
        (after (gensym))
        (pair (gensym))
        (pair-found (gensym)))
    `(cl-do* ((,before nil)
              (,after ,nalist (cdr ,after))
              (,pair (car ,after) (car ,after))
              (,pair-found nil)
              )
         ((null ,after)
          (setq ,nalist ,before)
          (cdr ,pair-found))
       (if (eq (car ,pair) ,key)
           (setq ,pair-found ,pair)
         (push ,pair ,before)))))

(defmacro nalist-poppair (nalist)
  "Return a pair in NALIST, and remove it from NALIST."
  `(prog1
       (car ,nalist)
     (setq ,nalist (cdr ,nalist))))

(defun nalist-map (function nalist)
  "Call FUNCTION for all pairs in NALIST.

FUNCTION is called with two arguments, KEY and VALUE.
‘nalist-map’ always returns nil."
  (let ((remaining nalist))
    (while remaining
      (let ((pair (car remaining)))
        (funcall function (car pair) (cdr pair))
        (setq remaining (cdr remaining))))
    nil))

(defun nalist-subset-p (nalist-a nalist-b)
  "Return t if NALIST-A is a subset of NALIST-B `equal'-wise, otherwise nil."
  (let ((res t))
    (mapc #'(lambda (pair)
              (unless (member pair nalist-b)
                (setq res nil)))
          nalist-a)
    res))

(defun nalist-equal (nalist-a nalist-b)
  "Return t if NALIST-A nad NALIST-B are identical `equal'-wise, otherwise nil."
  (equal nalist-a nalist-b))

(defun nalist-set-equal (nalist-a nalist-b &optional testfn)
  "Test with TESTFN if NALIST-A and NALIST-B have the same set of pairs.

Return t if so, otherwise nil.  The default TESTFN is `equal'."
  (seq-set-equal-p nalist-a nalist-b testfn))


(provide 'nalist)
;;; nalist.el ends here
