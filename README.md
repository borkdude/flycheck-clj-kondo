# flycheck-clj-kondo

[![MELPA Stable](https://stable.melpa.org/packages/flycheck-clj-kondo-badge.svg)](https://stable.melpa.org/#/flycheck-clj-kondo)
[![MELPA](https://melpa.org/packages/flycheck-clj-kondo-badge.svg)](https://melpa.org/#/flycheck-clj-kondo)

This package integrates [clj-kondo](https://github.com/borkdude/clj-kondo) with Emacs via [flycheck](https://www.flycheck.org).

## Installation

Before installing the Emacs package, make sure the `clj-kondo` is on your
path. For installation instructions, see https://github.com/borkdude/clj-kondo.

### MELPA

Install from [MELPA](https://melpa.org/#/flycheck-clj-kondo):

```
M-x package-install flycheck-clj-kondo
```

Then add the following to your init.el:

```
(require 'flycheck-clj-kondo)
```

### use-package

Install via [use-package](https://jwiegley.github.io/use-package/):

```emacs-lisp
;; First install the package:
(use-package flycheck-clj-kondo
  :ensure t)

;; then install the checker as soon as `clojure-mode' is loaded
(use-package clojure-mode
  :ensure t
  :config
  (require 'flycheck-clj-kondo))
```

### el-get

Install via [el-get](https://github.com/dimitri/el-get):

``` emacs-lisp
(el-get-bundle flycheck-clj-kondo
  :url "https://raw.githubusercontent.com/borkdude/flycheck-clj-kondo/master/flycheck-clj-kondo.el"
  (require 'flycheck-clj-kondo))
```

## Multiple linters

To set up multiple linters, e.g. in combination with
[flycheck-joker](https://github.com/candid82/flycheck-joker), add:

``` emacs-lisp
(dolist (checkers '((clj-kondo-clj . clojure-joker)
                    (clj-kondo-cljs . clojurescript-joker)
                    (clj-kondo-cljc . clojure-joker)))
  (flycheck-add-next-checker (car checkers) (cons 'error (cdr checkers))))
```
