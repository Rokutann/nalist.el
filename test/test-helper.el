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

;;; test-helper.el ends here
