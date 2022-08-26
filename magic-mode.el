;;; mylsl-mode.el --- sample major mode for editing LSL. -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2017, by you

;; Author: your name ( your email )
;; Version: 2.0.13
;; Created: 26 Jun 2015
;; Keywords: languages
;; Homepage: http://ergoemacs.org/emacs/elisp_syntax_coloring.html

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.


;;; Commentary:
;; short description here

;; full doc on how to use here

;;; Code:
(require 'cc-mode)
(require 'cc-langs)

;; create the list for font-lock.
(defvar magic-font-lock-keywords nil "Magic font lock keywords `magic-mode'.")

;; each category of keyword is given a particular face
(setq magic-font-lock-keywords
      (let* (
             ;; define several category of keywords
             (x-keywords '("func" "function" "effc" "effect" "eff" "handler" "ef" "continue" "hd" "do" "while" "resume" "with" "yield" "then" "in" "operator" "assert" "else" "for" "if" "return" "while" "where" "match" "extern" "implies" "union" "struct" "fn" "ty" "as" "is" "from" "therefore" "because"))
             (x-types '("unit" "bool" "c8" "n32" "i32" "Undefined" "nat32" "int32" "char8" "any" "type"))
             (x-constants '("ACTIVE" "AGENT" "ALL_SIDES" "ATTACH_BACK"))
             (x-events '("at_rot_target" "at_target" "attach"))
             (x-symbols '("and" "not" "or" "_" "(" ")" "." "{" "}" "+" "-" "?" "*" "/" "|-" "-|" "=" ":" "[" "]" "|" "&" "%" "^" "," ";" "~" "!" "," ">" "<" ">=" "<=" ".."))
             (x-functions '("assert"))

             ;; generate regex string for each category of keywords
             (x-keywords-regexp (regexp-opt x-keywords 'words))
             (x-types-regexp (regexp-opt x-types 'words))
             (x-constants-regexp (regexp-opt x-constants 'words))
             (x-events-regexp (regexp-opt x-events 'words))
             (x-functions-regexp (regexp-opt x-functions 'words))
             (x-symbols-regexp (regexp-opt x-symbols 1))
						 )
        `(

					;; the first regexp is the anchor of the fontification, meaning the
					;; "starting point" of the region
					("(\\(type\\) +\\([a-zA-Z_0-9]+\\) +::"
					 ;; fontify the `type' as keyword
					 (1 font-lock-keyword-face)
					 ;; fontify the function name as function
					 (2 font-lock-function-name-face)

					 ;; look for symbols after the `::', they are types
					 ("\\_<\\([a-zA-Z_0-9]+\\)\\_>"
						;; set the limit of search to the current `type' form only
						(save-excursion (up-list) (point))
						;; when we found all the types in the region (`type' form) go
						;; back to the `::' marker
						(re-search-backward "::")
						;; fontify each matched symbol as type
						(0 font-lock-type-face))

					 ;; when done with the symbols look for the arrows
					 ("->"
						;; we are starting from the `::' again, so set the same limit as
						;; for the previous search (the `type' form)
						(save-excursion (up-list) (point))
						;; do not move back when we've found all matches to ensure
						;; forward progress.  At this point we are done with the form
						nil
						;; fontify the found arrows as variables (whatever...)
						(0 font-lock-variable-name-face t)))


					
					(,x-keywords-regexp . 'font-lock-keyword-face)
					
					; ("\\([a-zA-Z_0-9]*\\) *(" . (1 font-lock-function-name-face))


					;; ("| *\\([a-zA-Z_0-9]*\\) *"  . (1 font-lock-type-face))
					;; ("& *\\([a-zA-Z_0-9]*\\) *"  . (1 font-lock-type-face))
					("with *\\([a-zA-Z_0-9]+\\) *do" . (1 font-lock-variable-name-face))
					
					("true" . 'font-lock-variable-name-face)
					("false" . 'font-lock-variable-name-face)
					("undefined" . 'font-lock-variable-name-face)

					("\\([a-zA-Z_0-9]+\\) *:\\([^:,=\n(){};]*\\)\\(:\\|,\\|=\\|\n\\|(\\|)\\|{\\|}\\|;\\)" . (1 font-lock-variable-name-face))

					("#\\([a-zA-Z_0-9]+\\) *" . 'font-lock-preprocessor-face)
					("\\([a-zA-Z_0-9]+\\) *(" . (1 font-lock-function-name-face))

					(,x-types-regexp . 'font-lock-type-face)
          (,x-constants-regexp . 'font-lock-constant-face)
          (,x-events-regexp . 'font-lock-builtin-face)
          (,x-functions-regexp . 'font-lock-function-name-face)

					;; (,x-symbols-regexp . 'font-lock-keyword-face)

          ))
			)

(defvar magic-mode-syntax-table nil "Syntax table for `magic-mode'.")

(setq magic-mode-syntax-table
      (let ( (synTable (make-syntax-table)))
				;; C++ style comment “// …”
				(modify-syntax-entry ?\/ ". 12" synTable)
        (modify-syntax-entry ?\n ">" synTable)

				;; comment style “/* … */”
        ;; (modify-syntax-entry ?\/ ". 14" synTable)
        ;; (modify-syntax-entry ?* ". 23" synTable)
				synTable))

;;;###autoload
(define-derived-mode magic-mode fundamental-mode "magic mode"
  "Major mode for editing my language code."
  (setq font-lock-defaults '((magic-font-lock-keywords)))
	(set-syntax-table magic-mode-syntax-table)
	(setq-local comment-start "//")
  (setq-local comment-start-skip "/\\*+[ \t]*")
  (setq-local comment-end "")
  (setq-local comment-end-skip "[ \t]*\\*+/")
	;; ;; pretify
	;; (setq prettify-symbols-alist magic-prettify-symbols-alist)
  ;; (setq prettify-symbols-compose-predicate #'magic--prettify-symbols-compose-p)
	)

;; add the mode to the `features' list
(provide 'magic-mode)

(prettify-symbols-mode)
;;; magic-mode.el ends here
