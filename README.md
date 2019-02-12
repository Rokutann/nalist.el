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

Bind ALIST to SYMBOL if SHALLOW is t, otherwise a deep copy of ALIST.

(fn SYMBOL ALIST &key SHALLOW)

```lisp
(nalist-init nal-1 '((a . b) (c . d))) ;; => nal-1
(nalist-init nal-2 '((1 . a) (1.0 . b))) ;; => nal-2
(nalist-init nal-3 '(("foo" . 3) ((a (b c)) . d))) ;; => nal-3

```

### nalist-get `(key nalist &key default (testfn 'eq))`

Return the value of KEY in NALIST if exists TESTFN-wise, otherwise DEFAULT.

(fn KEY NALIST &key DEFAULT (TESTFN 'eq))

```lisp
(nalist-get 'c nal-1) ;; => 'd
(nalist-get 1.0 nal-2) ;; => nil
(nalist-get 1.0 nal-2 :testfn 'eql) ;; => 'b
(nalist-get "foo" nal-3 :testfn 'equal) ;; => 3
```

### nalist-set `(key value nalist &key (testfn 'eq))`

Set a pair with KEY and VALUE in NALIST by finding KEY with TESTFN.

It destructively changes the value of KEY with VALUE if their is
a pair with KEY already, otherwise creats a new pair with KEY and
VALUE.

(fn KEY VALUE NALIST &key (TESTFN 'eq))

```lisp
(nalist-set 'c 'g nal-1) ;; => 'g
;; The content of nal-1 is changed to '((a . b) (c . g))
```

### nalist-remove `(key nalist &key (testfn 'eq))`

Remove the pair with KEY from NALIST if KEY exists TESTFN-wise.

(fn KEY NALIST &key (TESTFN 'eq))

```lisp
(nalist-remove 'c nal-1) ;; => nil
;; The content of nal-1 is changed to '((a . b))
```

### nalist-pop `(key nalist)`

Remove the pair with KEY from NALIST and return it.

```lisp
(nalist-pop 'c nal-1) ;; => 'd
;; The content of nal-1 is changed to '((a . b))
```

### nalist-poppair `(nalist)`

Remove a pair from NALIST and return it.

```lisp
(nalist-poppair nal-1) ;; => '(a . b)
;; The content of nal-1 is changed to '((c . d))
```

### nalist-pairs `(nalist)`

Return a list of all pairs in NALIST.

```lisp
(nalist-pairs nal-1) ;; => '((a . b) (c . d))
```

### nalist-keys `(nalist)`

Return a list of all keys in NALIST.

```lisp
(nalist-keys nal) ;; => '(a c)
```

### nalist-values `(nalist)`

Retrun a list of all values in NALIST.

```lisp
(nalist-values nal) ;; => '(b d)
```

### nalist-copy `(nalist-old nalist-new &key shallow)`

Create NALIST-NEW by SHALLOW or deep copying NALIST-OLD.

Shallow-copy the content of NALIST-NEW if SHALLOW is non-nil,
otherwise deep-copy it.

(fn NALIST-OLD NALIST-NEW &key SHALLOW)

```lisp
(nalist-copy nal-1 nal-deep) ;; => 'nal-deep
(nalist-copy nal-1 nal-shallow :shallow t) ;; => 'nal-shallow
```

### nalist-clear `(nalist)`

Set NALIST nil.

```lisp
(nalist-clear nal) ;; => nil
```

### nalist-map `(function nalist)`

Call FUNCTION for all entries in NALIST.

FUNCTION is called with two arguments, KEY and VALUE.
‘nalist-map’ always returns nil.

```lisp
(let ((res nil))
  (nalist-map #'(lambda (k v) (push k res)) nal-1)
  res) ;; => '(a c)
```

### nalist-pairp `(obj)`

Return t if OBJ is a pair, otherwise nil.

```lisp
(nalist-pairp 'a) ;; => nil
(nalist-pairp '(a . b)) ;; => t
```

### nalist-proper-list-p `(obj)`

Return t if OBJ is a proper list, otherwise nil.

A proper list is a non circular cons chain whose last cdr points nil.

```lisp
(nalist-proper-list-p nil) ;; => t
(nalist-proper-list-p '#1=(#1# . #1#)) ;; => nil
(nalist-proper-list-p '#1=(#1#  #1#)) ;; => t
(nalist-proper-list-p '(a)) ;; => t
(nalist-proper-list-p '(a . b)) ;; => nil

```

### nalist-nalist-p `(obj)`

Return t if OBJ is alist, otherwise nil.

```lisp
(nalist-nalist-p nil) ;; => t
(nalist-nalist-p '(a . b)) ;; => nil
(nalist-nalist-p '((a . b) . c)) ;; => nil
(nalist-nalist-p '((a . b) (c . d))) ;; => t
```

### nalist-subset-p `(nalist-a nalist-b)`

Return t if NALIST-A is a subset of NALIST-B ‘equal’-wise, otherwise nil.

```lisp
(nalist-subset-p '((a . b)) '((a . b) (c . d))) ;; => t
(nalist-subset-p '((a . b) (c . d)) '((a . b))) ;; => nil
```

### nalist-equal `(nalist-a nalist-b)`

Return t if NALIST-A nad NALIST-B are identical ‘equal’-wise, otherwise nil.

```lisp
(nalist-equal nal-1 '((a . b) (c . d)))) ;; => t
(nalist-equal nal-1 '((c . d) (a . b)))) ;; => nil
```

### nalist-set-equal `(nalist-a nalist-b &optional testfn)`

Check with TESTFN if NALIST-A and NALIST-B have the same pairs.

If so, return t, otherwise nil.  The default TESTFN is ‘equal’.

```lisp
(nalist-set-equal nal-1 '((a . b) (c . d)))) ;; => t
(nalist-set-equal nal-1 '((c . d) (a . b)))) ;; => t
```
