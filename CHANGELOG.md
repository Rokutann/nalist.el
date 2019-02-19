# Changelog

## dev branch (HEAD)


## 0.1.4 (2019-02-19)

### Changes

* Added prefixes to all `cl-gensym` calls for slightly easier macro debugging.
* Dropped ALIST-EVAL-ONCE from `nalist-init` and make the feature default.


## 0.1.3 (2019-02-18)

### Changes

* Added a keyword argmunet ALIST-EVAL-ONCE to `nalist-init`.

* On Emacs 26, the return value of `nalist-set` was changed from the
  whole alist to the value just associated with the key.

### Bug fixes

* Fixed leaky macros: `nalist-set` and `nalist-remove`.


## 0.1.2 (2019-02-16)

### New features

* Implemented the testfn feature on Emacs 25 for `nalist-get`, `nalist-set`, and `nalist-remove`.

### Changes

* Changed the return value of `nalist-remove` to be the same as `nalist-pop`.
* Changed `nalist-pop` from a macro to an alias of `nalist-remove`.

## 0.1.1 (2019-02-16)

### Changes

* Deployed Travis CI.
* Added support for Emacs 25.1, 25.2, and 25.3.
* On Emacs 25, testfn of `nalist-get` is ignored and always 'eq.
