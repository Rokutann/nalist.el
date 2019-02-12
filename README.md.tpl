# nalist.el

API to deal with association lists with names, or named alists. It's safe with buffer-local and lexical-binding.

## Installation

Place `nalist.el` on a directory in your `load-path', and add this to
your Emacs config:

```el
(require 'nalist)
```

## Documentation and Examples

### nalist-init `(symbol alist &key shallow)`

{{nalist-init}}

```lisp
(nalist-init nal-1 '((a . b) (c . d))) ;; => nal-1
(nalist-init nal-2 '((1 . a) (1.0 . b))) ;; => nal-2
(nalist-init nal-3 '(("foo" . 3) ((a (b c)) . d))) ;; => nal-3

```

### nalist-get `(key nalist &key default (testfn 'eq))`

{{nalist-get}}

```lisp
(nalist-get 'c nal-1) ;; => 'd
(nalist-get 1.0 nal-2) ;; => nil
(nalist-get 1.0 nal-2 :testfn 'eql) ;; => 'b
(nalist-get "foo" nal-3 :testfn 'equal) ;; => 3
```

### nalist-set `(key value nalist &key (testfn 'eq))`

{{nalist-set}}

```lisp
(nalist-set 'c 'g nal-1) ;; => 'g
;; The content of nal-1 is changed to '((a . b) (c . g))
```

### nalist-remove `(key nalist &key (testfn 'eq))`

{{nalist-remove}}

```lisp
(nalist-remove 'c nal-1) ;; => nil
;; The content of nal-1 is changed to '((a . b))
```

### nalist-pop `(key nalist)`

{{nalist-pop}}

```lisp
(nalist-pop 'c nal-1) ;; => 'd
;; The content of nal-1 is changed to '((a . b))
```

### nalist-poppair `(nalist)`

{{nalist-poppair}}

```lisp
(nalist-poppair nal-1) ;; => '(a . b)
;; The content of nal-1 is changed to '((c . d))
```

### nalist-pairs `(nalist)`

{{nalist-pairs}}

```lisp
(nalist-pairs nal-1) ;; => '((a . b) (c . d))
```

### nalist-keys `(nalist)`

{{nalist-keys}}

```lisp
(nalist-keys nal) ;; => '(a c)
```

### nalist-values `(nalist)`

{{nalist-values}}

```lisp
(nalist-values nal) ;; => '(b d)
```

### nalist-copy `(nalist-old nalist-new &key shallow)`

{{nalist-copy}}

```lisp
(nalist-copy nal-1 nal-deep) ;; => 'nal-deep
(nalist-copy nal-1 nal-shallow :shallow t) ;; => 'nal-shallow
```

### nalist-clear `(nalist)`

{{nalist-clear}}

```lisp
(nalist-clear nal) ;; => nil
```

### nalist-map `(function nalist)`

{{nalist-map}}

```lisp
(let ((res nil))
  (nalist-map #'(lambda (k v) (push k res)) nal-1)
  res) ;; => '(a c)
```

### nalist-pairp `(obj)`

{{nalist-pairp}}

```lisp
(nalist-pairp 'a) ;; => nil
(nalist-pairp '(a . b)) ;; => t
```

### nalist-mappable-list-p `(obj)`

{{nalist-mappable-list-p}}

```lisp
(nalist-mappable-list-p nil) ;; => t
(nalist-mappable-list-p '(a)) ;; => t
(nalist-mappable-list-p '(a . b)) ;; => nil

```

### nalist-nalist-p `(obj)`

{{nalist-nalist-p}}

```lisp
(nalist-nalist-p nil) ;; => t
(nalist-nalist-p '(a . b)) ;; => nil
(nalist-nalist-p '((a . b) . c)) ;; => nil
(nalist-nalist-p '((a . b) (c . d))) ;; => t
```

### nalist-subset-p `(nalist-a nalist-b)`

{{nalist-subset-p}}

```lisp
(nalist-subset-p '((a . b)) '((a . b) (c . d))) ;; => t
(nalist-subset-p '((a . b) (c . d)) '((a . b))) ;; => nil
```

### nalist-equal `(nalist-a nalist-b)`

{{nalist-equal}}

```lisp
(nalist-equal nal-1 '((a . b) (c . d)))) ;; => t
(nalist-equal nal-1 '((c . d) (a . b)))) ;; => nil
```

### nalist-set-equal `(nalist-a nalist-b &optional testfn)`

{{nalist-set-equal}}

```lisp
(nalist-set-equal nal-1 '((a . b) (c . d)))) ;; => t
(nalist-set-equal nal-1 '((c . d) (a . b)))) ;; => t
```
