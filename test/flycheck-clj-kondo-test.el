;; -*- lexical-binding: t; -*-
(require 'flycheck-clj-kondo)
(require 'flycheck-buttercup)

(defmacro flycheck-clj-kondo-def-checker-tests (checker language name file mode ts-mode &rest errors)
  "Define syntax checker tests for a regular major mode and optionally its tree-sitter variant.

CHECKER, LANGUAGE and NAME are passed to 'flycheck-buttercup-def-checker-test'.
FILE is the resource to check.
MODE is the regular major mode.
TS-MODE is the optional tree-sitter variant. When TS-MODE is nil, no tree-sitter test is generated.
ERRORS are the expected forms for 'flycheck-buttercup-should-syntax-check'; when omitted, no
errors are expected.

The tree-sitter test name gets a =-ts' suffix and is only
defined when TS-MODE is non-nil and Emacs is 29.1 or later."
  (declare (indent 4)
           (debug (symbolp symbolp symbolp form symbolp symbolp &rest form)))
  `(progn
     (flycheck-buttercup-def-checker-test ,checker ,language ,name
       (flycheck-buttercup-should-syntax-check ,file ',mode ,@errors))
     ,(when ts-mode
        `(when (version<= "29.1" emacs-version)
           (flycheck-buttercup-def-checker-test ,checker ,language ,(intern (format "%s-ts" name))
             (flycheck-buttercup-should-syntax-check ,file ',ts-mode ,@errors))))))

(describe "clj-kondo-find"
  (it "finds the clj-kondo executable"
    (expect (executable-find "clj-kondo") :to-be-truthy)))

(describe "clj-kondo-clj"
  (flycheck-clj-kondo-def-checker-tests clj-kondo-clj clj basic
    "test/corpus/basic.clj" clojure-mode clojure-ts-mode
    '(3 23 warning "unused binding y"
        :checker clj-kondo-clj)
    '(3 25 warning "unused binding z"
        :checker clj-kondo-clj)
    '(4 65 warning "unused binding y"
        :checker clj-kondo-clj)
    '(5 25 warning "unused binding y"
        :checker clj-kondo-clj)
    '(5 29 warning "unused binding zs"
        :checker clj-kondo-clj)
    '(7 1 error "corpus.basic/public-fixed is called with 1 arg but expects 3"
        :checker clj-kondo-clj)
    '(10 1 error "corpus.basic/public-multi-arity is called with 3 args but expects 1 or 2"
        :checker clj-kondo-clj)
    '(11 1 error "corpus.basic/public-varargs is called with 1 arg but expects 2 or more"
        :checker clj-kondo-clj))

  (flycheck-clj-kondo-def-checker-tests clj-kondo-clj clj utf8
    "test/corpus/utf8.clj" clojure-mode clojure-ts-mode))

(describe "clj-kondo-cljc"
  (flycheck-clj-kondo-def-checker-tests clj-kondo-cljc cljc basic
    "test/corpus/basic.cljc" clojurec-mode clojure-ts-clojurec-mode
    '(3 26 warning "unused binding y"
        :checker clj-kondo-cljc)
    '(13 9 error "test.corpus.basic/foo is called with 1 arg but expects 2"
        :checker clj-kondo-cljc)
    '(14 10 error "test.corpus.basic/foo is called with 2 args but expects 1"
        :checker clj-kondo-cljc)
    '(21 1 error "test.corpus.basic/bar is called with 3 args but expects 1"
        :checker clj-kondo-cljc)))

(describe "clj-kondo-cljs"
  (flycheck-clj-kondo-def-checker-tests clj-kondo-cljs cljs basic
    "test/corpus/basic.cljs" clojurescript-mode clojure-ts-clojurescript-mode
    '(3 5 error "Namespace name does not match file name: cond-without-else1"
        :checker clj-kondo-cljs)
    '(11 3 warning "use :else as the catch-all test expression in cond"
        :checker clj-kondo-cljs)
    '(18 3 warning "use :else as the catch-all test expression in cond"
        :checker clj-kondo-cljs)))

(describe "clj-kondo-edn"
  (flycheck-clj-kondo-def-checker-tests clj-kondo-edn edn basic
    "test/corpus/basic.edn" edn-mode clojure-ts-mode
    '(1 10 error "duplicate key a"
      :checker clj-kondo-edn)))
