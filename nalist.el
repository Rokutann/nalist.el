;;; nalist.el --- A named association list library.  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Cyriakus "Mukuge" Hill

;; Author: Cyriakus "Mukuge" Hill <cyriakus.h@gmail.com>
;; Keywords: Lisp, tools
;; URL: https://github.com/mukuge/nalist
;; Package-Version: 0.0.1
;; Package-Requires: ((emacs "26.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A named association list library.

;;; Code:

(require 'cl-lib)
(require 'seq)

(defmacro nalist-init (symbol alist)
  "Bind the value of SYMBOL to a deep copy of ALIST."
  `(setq ,symbol (copy-alist ,alist)))

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

(defmacro nalist-copy (nalist-old nalist-new)
  "Create NALIST-NEW by shallow-copying NALIST-OLD."
  `(setq ,nalist-new (copy-alist ,nalist-old)))

;; FIXME: Need to be rewritten with cl-do or something.
(defmacro nalist-pop (key nalist)
  "Remove the pair with KEY from NALIST and return it."
  `(let ((before nil) ;; FIXME: Need to be gensymed.
         (pair-found nil)
         (after ,nalist))
     (while after
       (let ((pair (car after)))
         (if (eq (car pair) ,key)
             (setq pair-found pair)
           (push pair before)))
       (setq after (cdr after)))
     (setq ,nalist before)
     (cdr pair-found)))

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
  "Return t if NALIST-A is a sbuset of NALIST-B, otherwise nil."
  (let ((res t))
    (mapc #'(lambda (pair)
              (unless (member pair nalist-b)
                (setq res nil)))
          nalist-a)
    res))

(defun nalist-set-equal (nalist-a nalist-b)
  "Return t if NALIST-A and NALIST-B are identical setwise, otherwise nil."
  (and (nalist-subset-p nalist-a nalist-b)
       (nalist-subset-p nalist-b nalist-a)))

(defun nalist-equal (nalist-a nalist-b)
  "Return t if NALIST-A nad NALIST-B are identical `equal' wise, otherwise nil."
  (equal nalist-a nalist-b))

(defun nalist-seq-eaula (nalist-a nalist-b)
  "Return t if NALIST-A and NALIST-B are identical sequence wise, otherwise nil."
  (seq-set-equal-p nalist-a nalist-b))

(provide 'nalist)
;;; nalist.el ends here
