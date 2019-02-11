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

(defmacro nalist-clear (alist)
  "Set ALIST nil."
  `(setq ,alist nil))

(cl-defmacro nalist-set (key value alist &key (testfn 'eq))
  "Set a KEY VALUE pair in ALIST with TESTFN."
  `(setf (alist-get ,key ,alist nil nil ',testfn) ,value))

(cl-defmacro nalist-remove (key alist &key (testfn 'eq))
  "Remove the pair with KEY in ALIST with TESTFN."
  `(setf (alist-get ,key ,alist nil t ',testfn) nil))


(cl-defun nalist-get (key alist &key default (testfn 'eq))
  "Return the value of KEY in ALIST if exists TESTFN wise, otherwise DEFAULT."
  (alist-get key alist default nil testfn))

(defun nalist-subset-p (alist-a alist-b)
  "Return t is ALIST-A is a sbuset of ALIST-B, otherwise nil."
  (let ((res t))
    (mapc #'(lambda (pair)
              (unless (member pair alist-b)
                (setq res nil)))
          alist-a)
    res))

(defun nalist-set-equal (alist-a alist-b)
  "Return t if ALIST-A and ALIST-B are identical setwise, otherwise nil."
  (and (nalist-subset-p alist-a alist-b)
       (nalist-subset-p alist-b alist-a)))


(provide 'nalist)
;;; nalist.el ends here
