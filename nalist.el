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

(defmacro nalist-clear (nalist)
  "Set NALIST nil."
  `(setq ,nalist nil))

(cl-defmacro nalist-set (key value nalist &key (testfn 'eq))
  "Set a KEY VALUE pair in NALIST with TESTFN."
  `(setf (alist-get ,key ,nalist nil nil ',testfn) ,value))

(cl-defmacro nalist-remove (key nalist &key (testfn 'eq))
  "Remove the pair with KEY in NALIST with TESTFN."
  `(setf (alist-get ,key ,nalist nil t ',testfn) nil))


(cl-defun nalist-get (key nalist &key default (testfn 'eq))
  "Return the value of KEY in NALIST if exists TESTFN wise, otherwise DEFAULT."
  (alist-get key nalist default nil testfn))

(defun nalist-subset-p (nalist-a nalist-b)
  "Return t is NALIST-A is a sbuset of NALIST-B, otherwise nil."
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


(provide 'nalist)
;;; nalist.el ends here
