;;; This script is based on docs.el in f.el.
;;; See: https://github.com/rejeep/f.el/blob/master/bin/docs.el

(require 'dash)
(require 's)
(require 'f)

(defvar nalist-root-path
  (expand-file-name ".." (file-name-directory load-file-name)))

(defvar nalist-lib-file
  (expand-file-name "nalist.el" nalist-root-path))

(defvar nalist-readme-file
  (expand-file-name "README.md" nalist-root-path))

(defvar nalist-readme-template
  (expand-file-name "README.md.tpl" nalist-root-path))

(defvar nalist-fn-doc-mapping (make-hash-table :test 'equal))

(require 'nalist nalist-lib-file)

(-map
 (lambda (lib)
   (when (equal (car lib) nalist-lib-file)
     (-select
      (lambda (alist)
        (when (and
               (listp alist)
               (equal (car alist) 'defun)
               (s-matches? "^nalist-[^-][a-z-]+\\??$" (symbol-name (cdr alist))))
          (puthash (symbol-name (cdr alist)) (documentation (cdr alist)) nalist-fn-doc-mapping)))
      (cdr lib))))
 load-history)

(let ((content (f-read nalist-readme-template)))
  (maphash
   (lambda (fn doc)
     (setq content (s-replace (concat "{{" fn "}}") doc content)))
   nalist-fn-doc-mapping)
  (f-write content 'utf-8 nalist-readme-file))
