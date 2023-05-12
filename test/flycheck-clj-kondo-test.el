(require 'flycheck-clj-kondo)
(require 'flycheck-ert)

(ert-deftest clj-kondo-find ()
    (should (executable-find "clj-kondo")))

(flycheck-ert-def-checker-test clj-kondo-clj clj basic
  (flycheck-ert-should-syntax-check
   "test/corpus/basic.clj" 'clojure-mode
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
       :checker clj-kondo-clj)))

(flycheck-ert-def-checker-test clj-kondo-cljc cljc basic
  (flycheck-ert-should-syntax-check
   "test/corpus/basic.cljc" 'clojurec-mode
   '(3 26 warning "unused binding y"
       :checker clj-kondo-cljc)
   '(13 9 error "test.corpus.basic/foo is called with 1 arg but expects 2"
        :checker clj-kondo-cljc)
   '(14 10 error "test.corpus.basic/foo is called with 2 args but expects 1"
        :checker clj-kondo-cljc)
   '(21 1 error "test.corpus.basic/bar is called with 3 args but expects 1"
        :checker clj-kondo-cljc)))

(flycheck-ert-def-checker-test clj-kondo-cljs cljs basic
  (flycheck-ert-should-syntax-check
   "test/corpus/basic.cljs" 'clojurescript-mode
   '(3 5 error "Namespace name does not match file name: cond-without-else1"
       :checker clj-kondo-cljs)
   '(11 3 warning "use :else as the catch-all test expression in cond"
        :checker clj-kondo-cljs)
   '(18 3 warning "use :else as the catch-all test expression in cond"
        :checker clj-kondo-cljs)))

(flycheck-ert-def-checker-test clj-kondo-edn edn basic
  (flycheck-ert-should-syntax-check
   "test/corpus/basic.edn" 'clojure-mode
   '(1 10 error "duplicate key a"
        :checker clj-kondo-edn)))

(flycheck-ert-def-checker-test clj-kondo-clj clj utf8
  (flycheck-ert-should-syntax-check
   "test/corpus/utf8.clj" 'clojure-mode))
