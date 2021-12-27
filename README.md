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
[flycheck-joker](https://github.com/candid82/flycheck-joker), add this after you required the linter packages:

``` emacs-lisp
(dolist (checker '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
  (setq flycheck-checkers (cons checker (delq checker flycheck-checkers))))
```

This ensures that the clj-kondo checkers are the first ones in the `flycheck-checkers` list. This is needed to make the chain work. To create the chain, also add the following code:

``` emacs-lisp
(dolist (checkers '((clj-kondo-clj . clojure-joker)
                    (clj-kondo-cljs . clojurescript-joker)
                    (clj-kondo-cljc . clojure-joker)
                    (clj-kondo-edn . edn-joker)))
  (flycheck-add-next-checker (car checkers) (cons 'error (cdr checkers))))
```

## Testing

Make sure [Eldev](https://github.com/doublep/eldev) is installed and
run `eldev test` for testing, `eldev lint` for linting.

## Troubleshooting

### clj-kondo is on my PATH but Flycheck still says it's disabled (M-x flycheck-verify-setup)

Check your PATH according to Emacs: `M-x shell`, then `echo $PATH`. If there are any differences between that PATH and the one you are able to run clj-kondo from (e.g., your PATH according to Terminal on macOS), add them by including them in your Emacs config. For example, if you needed to include `/usr/local/bin`, you could add to init.el:

```emacs-lisp
(setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))
(setq exec-path (append exec-path '("/usr/local/bin")))
```

Then, restart Emacs.
