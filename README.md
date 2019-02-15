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

Bind SYMBOL to ALIST if SHALLOW is non-nil, otherwise to a deep-copy of ALIST.

(fn SYMBOL ALIST &key SHALLOW)

```lisp
(nalist-init nal-1 '((a . b) (c . d))) ;; => nal-1
(nalist-init nal-2 '((1 . a) (1.0 . b))) ;; => nal-2
(nalist-init nal-3 '(("foo" . 3) ((a (b c)) . d))) ;; => nal-3

```

### nalist-get `(key nalist &key default (testfn 'eq))`

Return the value of KEY in NALIST if found with TESTFN, otherwise DEFAULT.

The default value of TESTFN is ’eq.

(fn KEY NALIST &key DEFAULT (TESTFN 'eq))

```lisp
(nalist-get 'c nal-1) ;; => 'd
(nalist-get 1.0 nal-2) ;; => nil
(nalist-get 1.0 nal-2 :testfn 'eql) ;; => 'b
(nalist-get "foo" nal-3 :testfn 'equal) ;; => 3
```

### nalist-set `(key value nalist &key (testfn 'eq))`

Find the pair with KEY in NALIST with TESTFN, and set its value to VALUE.

It destructively changes the value of the pair with KEY into
VALUE if the pair with KEY already exists, otherwise add a
new pair with KEY and VALUE to NALIST.

(fn KEY VALUE NALIST &key (TESTFN ''eq))

```lisp
(nalist-set 'c 'g nal-1) ;; => 'g
;; The content of nal-1 is changed to '((a . b) (c . g))
```

### nalist-remove `(key nalist &key (testfn 'eq))`

Remove the pair with KEY from NALIST if found with TESTFN.

(fn KEY NALIST &key (TESTFN ''eq))

```lisp
(nalist-remove 'c nal-1) ;; => nil
;; The content of nal-1 is changed to '((a . b))
```

### nalist-pop `(key nalist &key (testfn 'eq)`

Remove the pair with KEY from NALIST, and return the value of the pair.

This macro uses TESTFN to find the pair with the KEY. The default
value of TESTFN is ‘eq’.

(fn KEY NALIST &key (TESTFN ''eq))

```lisp
(nalist-pop 'c nal-1) ;; => 'd
;; The content of nal-1 is changed to '((a . b))
```

### nalist-poppair `(nalist)`

Return a pair in NALIST, and remove it from NALIST.

```lisp
(nalist-poppair nal-1) ;; => '(a . b)
;; The content of nal-1 is changed to '((c . d))
```

### nalist-pairs `(nalist)`

Return a list consisting all the pairs in NALIST.

```lisp
(nalist-pairs nal-1) ;; => '((a . b) (c . d))
```

### nalist-keys `(nalist)`

Return a list consisting all the keys in NALIST.

```lisp
(nalist-keys nal-1) ;; => '(a c)
```

### nalist-values `(nalist)`

Return a list consisting all the values in NALIST.

```lisp
(nalist-values nal-1) ;; => '(b d)
```

### nalist-copy `(nalist-old nalist-new &key shallow)`

Copy and bind the content of NALIST-OLD to NALIST-NEW.

This macro uses shallow-copy if SHALLOW is non-nil, otherwise
uses deep-copy.

(fn NALIST-OLD NALIST-NEW &key SHALLOW)

```lisp
(nalist-copy nal-1 nal-deep) ;; => 'nal-deep
(nalist-copy nal-1 nal-shallow :shallow t) ;; => 'nal-shallow
```

### nalist-clear `(nalist)`

Set NALIST nil.

```lisp
(nalist-clear nal-1) ;; => nil
```

### nalist-map `(function nalist)`

Call FUNCTION for all pairs in NALIST.

FUNCTION is called with two arguments, KEY and VALUE.
‘nalist-map’ always returns nil.

```lisp
(let ((res nil))
  (nalist-map #'(lambda (k v) (push k res)) nal-1)
  res) ;; => '(a c)
```

### nalist-pairp `(obj)`

Return t if OBJ is a pair, otherwise nil.

A pair is a cons cell, regardless of what Lisp objects its car
and cdr point.

```lisp
(nalist-pairp 'a) ;; => nil
(nalist-pairp '(a . b)) ;; => t
```

### nalist-proper-list-p `(obj)`

Return t if OBJ is a proper list, otherwise nil.

A proper list is a non circular cons chain whose last ‘cdr’ points nil.

```lisp
(nalist-proper-list-p nil) ;; => t
(nalist-proper-list-p '#1=(#1# . #1#)) ;; => nil
(nalist-proper-list-p '#1=(#1#  #1#)) ;; => t
(nalist-proper-list-p '(a)) ;; => t
(nalist-proper-list-p '(a . b)) ;; => nil

```

### nalist-nalist-p `(obj)`

Return t if OBJ is an (n)alist, otherwise nil.

An alist, or association list, is a proper list of pairs.  What
‘car’ and ‘cdr’ of a pair in alist point is often called a key
and value respectively.

```lisp
(nalist-nalist-p nil) ;; => t
(nalist-nalist-p '(a . b)) ;; => nil
(nalist-nalist-p '((a . b) . c)) ;; => nil
(nalist-nalist-p '((a . b) (c . d))) ;; => t
```

### nalist-subset-p `(nalist-a nalist-b)`

Return t if NALIST-A is a subset of NALIST-B with ‘equal’, otherwise nil.

```lisp
(nalist-subset-p '((a . b)) '((a . b) (c . d))) ;; => t
(nalist-subset-p '((a . b) (c . d)) '((a . b))) ;; => nil
```

### nalist-equal `(nalist-a nalist-b)`

Return t if NALIST-A nad NALIST-B are identical with ‘equal’, otherwise nil.

```lisp
(nalist-equal nal-1 '((a . b) (c . d)))) ;; => t
(nalist-equal nal-1 '((c . d) (a . b)))) ;; => nil
```

### nalist-set-equal-p `(nalist-a nalist-b)`

Test with ‘equal’ if NALIST-A and NALIST-B have the same set of pairs.

Return t if so, otherwise nil.

```lisp
(nalist-set-equal nal-1 '((a . b) (c . d)))) ;; => t
(nalist-set-equal nal-1 '((c . d) (a . b)))) ;; => t
```

### nalist-make-local-variable `(nalist)`

Create a buffer-local binding in the current buffer for NALIST.

This macro binds a deep-copy of the content of the original
NALIST to the buffer-local NALIST to avoid their sharing cons
cells.

```lisp
(nalist-make-local-variable na-1)
;; na-1 is changed to a buffer-local variable in the current buffer.
```

### nalist-make-variable-buffer-local `(nalist)`

Mark NALIST automatically buffer local.

This macro binds a deep-copy of the content of the original
NALIST to the buffer-local NALIST to avoid their sharing cons
cells.

It also sets the default value of NALIST to nil to avoid the
buffer-local variables in other buffers share the cons cells
through it.

```lisp
(nalist-make-variable-buffer-local na-2)
;; na-2 is changed to an automatically buffer-local variable.
```
