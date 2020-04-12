;;; flycheck-clj-kondo.el --- Add clj-kondo linter to flycheck

;; Copyright (C) 2019 Michiel Borkent <michielborkent@gmail.com>
;; This code borrows heavily from flycheck-joker:
;; https://github.com/candid82/flycheck-joker
;;
;; Author: Michiel Borkent <michielborkent@gmail.com>
;; Created: 3 April 2019
;; Version: 2019.04.03
;; Homepage: https://github.com/borkdude/flycheck-clj-kondo
;; Package-Requires: ((flycheck "0.18"))

;;; Commentary:

;; This package integrates clj-kondo with Emacs via flycheck.  To use it, add to
;; your init.el:

;; (require 'flycheck-clj-kondo)

;; Make sure the clj-kondo binary is on your path.  For installation
;; instructions, see https://github.com/borkdude/clj-kondo.

;;; License:

;; This file is not part of GNU Emacs.
;; However, it is distributed under the same license.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:
(require 'flycheck)

(defmacro flycheck-clj-kondo--define-checker
    (name lang mode &rest extra-args)
  "Internal macro to define checker.
Argument NAME: the name of the checker.
Argument LANG: language string.
Argument MODE: the mode in which this checker is activated.
Argument EXTRA-ARGS: passes extra args to the checker."
  (let ((command
         (append
          (list "clj-kondo" "--lint" "-" "--lang" lang)
          extra-args)))
    `(flycheck-define-checker ,name
       "See https://github.com/borkdude/clj-kondo"
       :command ,command
       :standard-input t
       :error-patterns
       ((error line-start "<stdin>:" line ":" column ": " (0+ not-newline) (or "error: " "Exception: ") (message) line-end)
        (warning line-start "<stdin>:" line ":" column ": " (0+ not-newline) "warning: " (message) line-end)
        (info line-start "<stdin>:" line ":" column ": " (0+ not-newline) "info: " (message) line-end))
       :modes (,mode)
       :predicate (lambda () (string= ,lang (file-name-extension (buffer-file-name)))))))

(defmacro flycheck-clj-kondo-define-checkers (&rest extra-args)
  "Defines all clj-kondo checkers.
Argument EXTRA-ARGS: passes extra arguments to the checkers."
  `(progn
     (flycheck-clj-kondo--define-checker clj-kondo-clj "clj" clojure-mode ,@extra-args)
     (flycheck-clj-kondo--define-checker clj-kondo-cljs "cljs" clojurescript-mode ,@extra-args)
     (flycheck-clj-kondo--define-checker clj-kondo-cljc "cljc" clojurec-mode ,@extra-args)
     (flycheck-clj-kondo--define-checker clj-kondo-edn "edn" clojure-mode ,@extra-args)
     (dolist (element '(clj-kondo-clj clj-kondo-cljs clj-kondo-cljc clj-kondo-edn))
       (add-to-list 'flycheck-checkers element))))

(flycheck-clj-kondo-define-checkers "--cache")

(provide 'flycheck-clj-kondo)
;;; flycheck-clj-kondo.el ends here
