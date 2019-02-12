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
  "Return t if OBJ is a pair, otherwise nil."
  (consp obj))

(defun nalist-mappable-list-p (obj)
  "Return t if OBJ is a list which can be used with `mapc' and such."
  (or (null obj)
      (and (listp obj)
           (nalist-mappable-list-p (cdr obj)))))

(defun nalist-nalist-p (obj)
  "Return t if OBJ is alist, otherwise nil."
  (and (nalist-mappable-list-p obj)
       (let ((res t))
         (mapc #'(lambda (pair)
                   (unless (nalist-pairp pair)
                     (setq res nil)))
               obj)
         res)))

(cl-defmacro nalist-init (symbol alist &key shallow)
  "Bind ALIST to SYMBOL if SHALLOW is t, otherwise a deep copy of ALIST."
  `(progn
     (unless (nalist-nalist-p ,alist)
       (error "Invalid initial value `%s'" ,alist))
     (if ,shallow
         (setq ,symbol ,alist)
       (setq ,symbol (copy-alist ,alist)))
     ',symbol))

(defmacro nalist-clear (nalist)
  "Set NALIST nil."
  `(setq ,nalist nil))

(cl-defmacro nalist-set (key value nalist &key (testfn 'eq))
  "Set a KEY VALUE pair in NALIST with TESTFN."
  `(setf (alist-get ,key ,nalist nil nil ',testfn) ,value))

(cl-defun nalist-get (key nalist &key default (testfn 'eq))
  "Return the value of KEY in NALIST if exists TESTFN wise, otherwise DEFAULT."
  (alist-get key nalist default nil testfn))

(cl-defmacro nalist-remove (key nalist &key (testfn 'eq))
  "Remove the pair with KEY in NALIST with TESTFN."
  `(setf (alist-get ,key ,nalist nil t ',testfn) nil))

(defun nalist-pairs (nalist)
  "Return a list of all pairs in NALIST."
  (copy-alist nalist))

(defun nalist-keys (nalist)
  "Return a list of all keys in NALIST."
  (mapcar 'car nalist))

(defun nalist-values (nalist)
  "Retrun a list of all values in NALIST."
  (mapcar 'cdr nalist))

(cl-defmacro nalist-copy (nalist-old nalist-new &key shallow)
  "Create NALIST-NEW by SHALLOW or deep copying NALIST-OLD.

Shallow-copy the content of NALIST-NEW if SHALLOW is non-nil,
otherwise deep-copy it."
  `(progn
     (unless (nalist-nalist-p ,nalist-old)
       (error "Invalid initial value: `%s'" ,nalist-old))
     (if ,shallow
         (setq ,nalist-new ,nalist-old)
       (setq ,nalist-new (copy-alist ,nalist-old)))
     ',nalist-new))

(defmacro nalist-pop (key nalist)
  "Remove the pair with KEY from NALIST and return it."
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
  "Remove a pair from NALIST and return it."
  `(prog1
       (car ,nalist)
     (setq ,nalist (cdr ,nalist))))

(defun nalist-map (function nalist)
  "Call FUNCTION for all entries in NALIST.

FUNCTION is called with two arguments, KEY and VALUE.
‘nalist-map’ always returns nil."
  (let ((remaining nalist))
    (while remaining
      (let ((pair (car remaining)))
        (funcall function (car pair) (cdr pair))
        (setq remaining (cdr remaining))))
    nil))

(defun nalist-subset-p (nalist-a nalist-b)
  "Return t if NALIST-A is a subset of NALIST-B `equal' wise, otherwise nil."
  (let ((res t))
    (mapc #'(lambda (pair)
              (unless (member pair nalist-b)
                (setq res nil)))
          nalist-a)
    res))

(defun nalist-equal (nalist-a nalist-b)
  "Return t if NALIST-A nad NALIST-B are identical `equal' wise, otherwise nil."
  (equal nalist-a nalist-b))

(defun nalist-set-equal (nalist-a nalist-b &optional testfn)
  "Check with TESTFN if NALIST-A and NALIST-B have same pairs.

Return t if so, otherwise nil.  The default TESTFN is `equal'."
  (seq-set-equal-p nalist-a nalist-b testfn))


(provide 'nalist)
;;; nalist.el ends here
