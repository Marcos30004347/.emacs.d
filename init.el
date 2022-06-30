(require 'package)

;; Disable automatic package startup
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("elpa" . "http://elpa.gnu.org/packages/"))


;; Initialize packages
(package-initialize)

;; Instale use-package
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

(use-package evil
	:ensure t
	:init
	(setq evil-want-integration t)
	(setq evil-want-C-u-scroll t)

	(setq evil-cross-lines t)

	:config
	(evil-mode 1)

	(evil-global-set-key 'motion "j" 'evil-next-visual-line)
	(evil-global-set-key 'motion "k" 'evil-previous-visual-line))

;;(use-package auto-complete
;;	:ensure t
;;	:init
;;	(progn
;;		(ac-config-default)
;;		(global-auto-complete-mode t)))

(use-package company
	:ensure t
	:after lsp-mode
	:hook (lsp-mode . company-mode)
	:bind (:map company-active-map
							("<tab>" . company-complete-selection))
	(:map lsp-mode-map
				("<tab>" . company-indent-or-complete-common))
	:custom
	(company-minimum-prefix-length 1)
	(company-idle-delay 0.0))
(use-package company-box
	:ensure t
	:hook (company-mode . company-box-mode))

;; (use-package ivy
;; 	:ensure t
;; 	:diminish
;; 	:bind (("C-s" . swiper)
;; 				 :map ivy-minibuffer-map
;; 				 ("TAB" . ivy-alt-done)
;; 				 ("C-l" . ivy-alt-done)
;; 				 ("C-j" . ivy-next-line)
;; 				 ("C-k" . ivy-previous-line)
;; 				 :map ivy-switch-buffer-map
;; 				 ("C-k" . ivy-previous-line)
;; 				 ("C-l" . ivy-done)
;; 				 ("C-d" . ivy-switch-buffer-kill)
;; 				 :map ivy-reverse-i-search-map
;; 				 ("C-k" . ivy-previous-line)
;; 				 ("C-d" . ivy-reverse-i-search-kill))
;; 	:config
;; 	(ivy-mode 1))

;; (use-package counsel
;; 	:ensure t
;; 	:bind (("C-M-j" . 'counsel-switch-buffer)
;; 				 :map minibuffer-local-map
;; 				 ("C-r" . 'counsel-minibuffer-history))
;; 	:custom
;; 	(counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
;; 	:config
;; 	(counsel-mode 1))

;; (use-package counsel-projectile
;; 	:ensure t
;; 	:after projectile
;; 	:config
;; 	(counsel-projectile-mode 1))

(use-package projectile
	:ensure t
	:init
	(projectile-mode +1)
	:bind (:map projectile-mode-map
							("s-p" . projectile-command-map)
							("C-c p" . projectile-command-map)))

(setq projectile-project-search-path '("~/workspace/"))

(use-package editorconfig
	:ensure t
	:config
	(editorconfig-mode 1))

(use-package move-text
	:ensure t)

(use-package helm
	:ensure t
	:preface (require 'helm-config)
	:init
	(setq helm-split-window-in-side-p t
				helm-move-to-line-cycle-in-source t)
	:config 
	(helm-mode 1) ;; Most of Emacs prompts become helm-enabled
	(helm-autoresize-mode 1) ;; Helm resizes according to the number of candidates
	(global-set-key (kbd "M-b") 'helm-buffers-list) ;; List buffers ( Emacs way )
	(define-key evil-ex-map "b" 'helm-buffers-list) ;; List buffers ( Vim way )
	(global-set-key (kbd "C-x r b") 'helm-bookmarks) ;; Bookmarks menu
																				;(global-set-key (kbd "C-x C-f") 'helm-find-file) ;; Finding files with Helm
	(global-set-key (kbd "M-c") 'helm-calcul-expression) ;; Use Helm for calculations
	(global-set-key (kbd "C-s") 'helm-occur)  ;; Replaces the default isearch keybinding
	(global-set-key (kbd "C-h a") 'helm-apropos)  ;; Helmized apropos interface
	(global-set-key (kbd "M-x") 'helm-M-x)  ;; Improved M-x menu
	(global-set-key (kbd "M-y") 'helm-show-kill-ring)  ;; Show kill ring, pick something to paste
	:ensure t)
(use-package helm-projectile
	:ensure t)

;; (use-package yasnippet
;; 	:ensure t)

;; (use-package auto-yasnippet
;; 	:ensure t)

;; (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
;; (yas-global-mode 1)

(use-package ag
	:ensure t)

(setq ag-highlight-search t)
(setq ag-reuse-window t)

(use-package wgrep
	:ensure t)
(use-package wgrep-ag
	:ensure t)

(load  (expand-file-name "gendoxy.el" user-emacs-directory))

;;(use-package good-scroll
;;  :ensure t)

;;(good-scroll-mode 1)

;; (use-package fixmee
;; 	:ensure t)
;; (use-package button-lock
;; 	:ensure t)

;; (global-fixmee-mode 1)

;; (use-package highlight-indent-guides
;; 	:ensure t)

;; (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)

;; (setq highlight-indent-guides-method 'bitmap)

(setq auto-save-file-name-transforms
			`((".*" ,(concat user-emacs-directory "auto-save") t)))
(setq backup-directory-alist
			`(("." . ,(expand-file-name
								 (concat user-emacs-directory "backups")))))
(setq create-lockfiles nil)

(show-paren-mode 1)

(setq ns-alternate-modifier 'meta)
(setq ns-right-alternate-modifier 'none)

;; Delete selected text on insert
(delete-selection-mode 1)

(setq tab-always-indent 'complete
			indent-tabs-mode nil)
(setq-default indent-tabs-mode t)

(setq-default tab-width 2)
(define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)
(setq indent-tabs-mode t)

(global-visual-line-mode t)

(setq-default word-wrap t)

(setq ns-alternate-modifier 'none)
(setq ns-right-alternate-modifier 'none)
(setq mac-command-modifier 'meta)

;; Remove Welcome message
(setq inhibit-startup-message t)

;; ;; Hilight on current line
;; (global-hl-line-mode t)

;; Remove blinking cursor
(blink-cursor-mode 0)
;; Remover tool bar
(tool-bar-mode -1)
;; Remove menu bar
(menu-bar-mode -1)
;; Remover barra de rolagem
(scroll-bar-mode -1)
;; Show line number globally
(global-linum-mode t)

(setq visible-bell nil)

;; Add doom modeline to the bottom
(use-package doom-modeline
	:ensure t
	:init (doom-modeline-mode 1)
	:custom ((doom-modeline-height 15)))

		 ;; Setup doom-themes
		 (use-package gruvbox-theme :ensure t)
		 (use-package ample-theme :ensure t)
		 (use-package zenburn-theme :ensure t)
		 (use-package solarized-theme :ensure t)


		 (use-package doom-themes
			 :ensure t
			 :config
			 ;; (setq doom-themes-enable-bold nil
			 ;; 			doom-themes-enable-italic nil)

			 ;; (load-theme 'gruvbox-dark-medium t)
			 ;; (load-theme 'ample-light t)

			 (doom-themes-visual-bell-config)
			 (doom-themes-neotree-config)
			 ;; (setq doom-themes-treemacs-theme "doom-gruvbox")
			 ;; (setq doom-themes-treemacs-theme "gruvbox-dark-medium")
			 (doom-themes-treemacs-config)
			 (doom-themes-org-config))

		 (use-package almost-mono-themes
			 :ensure t)

		 ;; (load-theme 'zenburn t)
     (load-theme 'solarized-dark t)

		 ;; (use-package gruvbox-theme
		 ;; 	:ensure t)
		 ;; 	(use-package spacemacs-theme
		 ;; 	:defer t
		 ;; 	:init (load-theme 'spacemacs-dark t))

(use-package all-the-icons
	:ensure t
	:if (display-graphic-p)
	:commands all-the-icons-install-fonts
	:init
	(unless (find-font (font-spec :name "all-the-icons"))
		(all-the-icons-install-fonts t)))

(use-package all-the-icons-dired
	:ensure t
	:if (display-graphic-p)
	:hook (dired-mode . all-the-icons-dired-mode))

;;(set-face-bold-p 'bold nil)

;;(set-face-attribute 'default nil :font "Fixedsys Excelsior 3.01" :height 120 :weight 'normal :underline nil)
(set-face-attribute 'default nil :font "IBM Plex Mono" :height 120)
;; (set-face-attribute 'default nil :font "Source Code Pro" :height 120)
;;(set-face-attribute 'default nil :font "Cutive Mono" :height 120)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "IBM Plex Mono" :height 120)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Cantarell" :height 120 :weight 'regular)

(use-package dashboard
	:ensure t
	:config
	(setq dashboard-set-heading-icons t)
	(setq dashboard-set-file-icons t)
	(setq dashboard-set-navigator t)
	(setq dashboard-banner-logo-title "Welcome to Emacs Dashboard")
	(setq dashboard-startup-banner "~/.emacs.d/dashboard-logos/acdc.txt")
	(setq dashboard-center-content t)
	(setq dashboard-show-shortcuts t)
	(setq dashboard-items '((recents  . 5)
													(bookmarks . 5)
													(projects . 5)
													(agenda . 5)
													(registers . 5)))	
	(dashboard-setup-startup-hook))

(use-package all-the-icons
	:ensure t)
;; (use-package neotree
;; 	:ensure t)
;; (defun neo-open-file-hide (full-path &optional arg)
;; 	"Open a file node and hides tree."
;; 	(neo-global--select-mru-window arg)
;; 	(find-file full-path)
;; 	(neotree-hide))

;; (defun neotree-enter-hide (&optional arg)
;; 	"Enters file and hides neotree directly"
;; 	(interactive "P")
;; 	(neo-buffer--execute arg 'neo-open-file-hide 'neo-open-dir))

;; (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
;; (setq-default neo-show-hidden-files t)
;; (setq neo-smart-open t)
;; (setq projectile-switch-project-action 'neotree-projectile-action)
;; (add-hook 'neotree-mode-hook
;; 					(lambda ()
;; 						(define-key evil-normal-state-local-map (kbd "TAB") 'neotree-enter-hide)
;; 						;;(define-key evil-normal-state-local-map (kbd "SPC") 'neotree-quick-look)
;; 						(define-key evil-normal-state-local-map (kbd "q") 'neotree-hide)
;; 						(define-key evil-normal-state-local-map (kbd "RET") 'neotree-enter)
;; 						(define-key evil-normal-state-local-map (kbd "g") 'neotree-refresh)
;; 						(define-key evil-normal-state-local-map (kbd "n") 'neotree-next-line)
;; 						(define-key evil-normal-state-local-map (kbd "p") 'neotree-previous-line)
;; 						(define-key evil-normal-state-local-map (kbd "A") 'neotree-stretch-toggle)
;; 						(define-key evil-normal-state-local-map (kbd "H") 'neotree-hidden-file-toggle)))
(use-package treemacs
	:ensure t
	:defer t
	:init
	(with-eval-after-load 'winum
		(define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
	:config
	(progn
		(setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
					treemacs-deferred-git-apply-delay        0.5
					treemacs-directory-name-transformer      #'identity
					treemacs-display-in-side-window          t
					treemacs-eldoc-display                   t
					treemacs-file-event-delay                5000
					treemacs-file-extension-regex            treemacs-last-period-regex-value
					treemacs-file-follow-delay               0.2
					treemacs-file-name-transformer           #'identity
					treemacs-follow-after-init               t
					treemacs-expand-after-init               t
					treemacs-git-command-pipe                ""
					treemacs-goto-tag-strategy               'refetch-index
					treemacs-indentation                     2
					treemacs-indentation-string              " "
					treemacs-is-never-other-window           nil
					treemacs-max-git-entries                 5000
					treemacs-missing-project-action          'ask
					treemacs-move-forward-on-expand          nil
					treemacs-no-png-images                   nil
					treemacs-no-delete-other-windows         t
					treemacs-project-follow-cleanup          nil
					treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
					treemacs-position                        'left
					treemacs-read-string-input               'from-child-frame
					treemacs-recenter-distance               0.1
					treemacs-recenter-after-file-follow      nil
					treemacs-recenter-after-tag-follow       nil
					treemacs-recenter-after-project-jump     'always
					treemacs-recenter-after-project-expand   'on-distance
					treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
					treemacs-show-cursor                     nil
					treemacs-show-hidden-files               t
					treemacs-silent-filewatch                nil
					treemacs-silent-refresh                  nil
					treemacs-sorting                         'alphabetic-asc
					treemacs-select-when-already-in-treemacs 'move-back
					treemacs-space-between-root-nodes        t
					treemacs-tag-follow-cleanup              t
					treemacs-tag-follow-delay                1.5
					treemacs-user-mode-line-format           nil
					treemacs-user-header-line-format         nil
					treemacs-wide-toggle-width               70
					treemacs-width                           25
					treemacs-width-increment                 1
					treemacs-width-is-initially-locked       nil
					treemacs-workspace-switch-cleanup        nil)

		;; The default width and height of the icons is 22 pixels. If you are
		;; using a Hi-DPI display, uncomment this to double the icon size.
		;;(treemacs-resize-icons 44)

		(treemacs-follow-mode t)
		(treemacs-filewatch-mode t)
		(treemacs-fringe-indicator-mode 'always))

	;; (pcase (cons (not (null (executable-find "git")))
	;; 						 (not (null treemacs-python-executable)))
	;; 	(`(t . t)
	;; 	 (treemacs-git-mode 'deferred))
	;; 	(`(t . _)
	;; 	 (treemacs-git-mode 'simple)))
	;;(treemacs-hide-gitignored-files-mode nil))
	:bind
	(:map global-map
				("M-0"       . treemacs-select-window)
				("C-x t 1"   . treemacs-delete-other-windows)
				("C-x t t"   . treemacs)
				("C-x t B"   . treemacs-bookmark)
				("C-x t C-t" . treemacs-find-file)
				("C-x t M-t" . treemacs-find-tag)))

(with-eval-after-load 'treemacs
	(define-key treemacs-mode-map [mouse-1] #'treemacs-single-click-expand-action))

;; (add-hook 'projectile-after-switch-project-hook 'treemacs-display-current-project-exclusively)

(use-package treemacs-evil
	:after (treemacs evil)
	:ensure t)

(use-package treemacs-projectile
	:after (treemacs projectile)
	:ensure t)

(use-package treemacs-icons-dired
	:hook (dired-mode . treemacs-icons-dired-enable-once)
	:ensure t)

(use-package hl-todo
	:ensure t
	:hook (prog-mode . hl-todo-mode)
	:config
	(setq hl-todo-highlight-punctuation ":"
				hl-todo-keyword-faces
				`(("TODO"       warning bold)
					("FIXME"      error bold)
					("HACK"       font-lock-constant-face bold)
					("REVIEW"     font-lock-keyword-face bold)
					("NOTE"       success bold)
					("DEPRECATED" font-lock-doc-face bold))))

(defun efs/org-mode-setup ()
	(org-indent-mode)
	(variable-pitch-mode 1)
	(visual-line-mode 1))

(defun efs/org-font-setup ()
	;; Replace list hyphen with dot
	(font-lock-add-keywords 'org-mode
													'(("^ *\\([-]\\) "
														 (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

	;; Set faces for heading levels
	(dolist (face '(
									(org-level-1 . 1.2)
									(org-level-2 . 1.1)
									(org-level-3 . 1.05)
									(org-level-4 . 1.0)
									(org-level-5 . 1.1)
									(org-level-6 . 1.1)
									(org-level-7 . 1.1)
									(org-level-8 . 1.1)))
		(set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

	;; Ensure that anything that should be fixed-pitch in Org files appears that way
	(set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
	(set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
	(set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
	(set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
	(set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
	(set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
	(set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
	:hook (org-mode . efs/org-mode-setup)
	:config
	;; (setq org-ellipsis " ▾")
	(setq org-agenda-start-with-log-mode t)
	(setq org-log-done 'time)
	(setq org-log-into-drawer t)
	(setq org-src-preserve-indentation t)
	(setq org-src-tab-acts-natively t)
	(setq org-agenda-files
				'("~/workspace/orgfiles/tasks.org"))
	(setq org-adapt-indentation nil)
	(setq org-hide-leading-stars t)
	(setq org-todo-keywords
				'((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!)")
					(sequence "BACKLOG(b)" "PLAN(p)" "READY(r)" "ACTIVE(a)" "REVIEW(v)" "WAIT(w@/!)" "HOLD(h)" "|" "COMPLETED(c)" "CANC(k@)")))

	(setq org-refile-targets
				'(("tasks.org" :maxlevel . 1)))

	;; Save Org buffers after refiling!
	(advice-add 'org-refile :after 'org-save-all-org-buffers)

	(setq org-tag-alist
				'((:startgroup)
																				; Put mutually exclusive tags here
					(:endgroup)
					("@errand" . ?E)
					("@home" . ?H)
					("@work" . ?W)
					("agenda" . ?a)
					("planning" . ?p)
					("publish" . ?P)
					("batch" . ?b)
					("note" . ?n)
					("idea" . ?i)))

	;; Configure custom agenda views
	(setq org-agenda-custom-commands
				'(("d" "Dashboard"
					 ((agenda "" ((org-deadline-warning-days 7)))
						(todo "NEXT"
									((org-agenda-overriding-header "Next Tasks")))
						(tags-todo "agenda/ACTIVE" ((org-agenda-overriding-header "Active Projects")))))

					("n" "Next Tasks"
					 ((todo "NEXT"
									((org-agenda-overriding-header "Next Tasks")))))

					("W" "Work Tasks" tags-todo "+work-email")

					;; Low-effort next actions
					("e" tags-todo "+TODO=\"NEXT\"+Effort<15&+Effort>0"
					 ((org-agenda-overriding-header "Low Effort Tasks")
						(org-agenda-max-todos 20)
						(org-agenda-files org-agenda-files)))

					("w" "Workflow Status"
					 ((todo "WAIT"
									((org-agenda-overriding-header "Waiting on External")
									 (org-agenda-files org-agenda-files)))
						(todo "REVIEW"
									((org-agenda-overriding-header "In Review")
									 (org-agenda-files org-agenda-files)))
						(todo "PLAN"
									((org-agenda-overriding-header "In Planning")
									 (org-agenda-todo-list-sublevels nil)
									 (org-agenda-files org-agenda-files)))
						(todo "BACKLOG"
									((org-agenda-overriding-header "Project Backlog")
									 (org-agenda-todo-list-sublevels nil)
									 (org-agenda-files org-agenda-files)))
						(todo "READY"
									((org-agenda-overriding-header "Ready for Work")
									 (org-agenda-files org-agenda-files)))
						(todo "ACTIVE"
									((org-agenda-overriding-header "Active Projects")
									 (org-agenda-files org-agenda-files)))
						(todo "COMPLETED"
									((org-agenda-overriding-header "Completed Projects")
									 (org-agenda-files org-agenda-files)))
						(todo "CANC"
									((org-agenda-overriding-header "Cancelled Projects")
									 (org-agenda-files org-agenda-files)))))))

	(efs/org-font-setup))

(add-hook 'org-tab-first-hook
          (lambda ()
            (when (org-in-src-block-p t)
              (let* ((elt (org-element-at-point))
                     (lang (intern (org-element-property :language elt)))
                     (langs org-babel-load-languages))
                (unless (alist-get lang langs)
                  (indent-to 4))))))

(defun my/use-text-mode-org-comments (args)
  "Use text-mode for editing comments"
  (unless (nth 2 args)
    (setf (nth 2 args) 'text-mode))
  args)

(advice-add 'org-src--edit-element 
            :filter-args #'my/use-text-mode-org-comments)

(use-package org-bullets
	:after org
	:ensure t
	:hook (org-mode . org-bullets-mode)
	:custom
	(org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun efs/org-mode-visual-fill ()
	(setq visual-fill-column-width 150
				visual-fill-column-center-text t)
	(visual-fill-column-mode 1))

(use-package visual-fill-column
	:ensure t
	:hook (org-mode . efs/org-mode-visual-fill))

(defun my-fill-column-hook ()
  (visual-fill-column-mode 1)
  (visual-line-mode 1))

(add-hook 'text-mode-hook #'efs/org-mode-visual-fill)

; (add-hook 'visual-line-mode-hook 'visual-fill-column-mode)
; (global-visual-line-mode t)
; (define-key visual-line-mode-map [remap kill-line] nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t)
	 (python . t)))

;; Automatically tangle our emacs.org config file when we save it
(defun efs/org-babel-tangle-config ()
	(when (string-equal (buffer-file-name)
											(expand-file-name "~/.emacs.d/emacs.org"))
		;; Dynamic scoping to the rescue
		(let ((org-confirm-babel-evaluate nil))
			(org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook #'efs/org-babel-tangle-config)))

(setq lsp-log-io nil) ;; Don't log everything = speed
(setq lsp-keymap-prefix "C-c l")
(setq lsp-restart 'auto-restart)
(setq lsp-ui-sideline-show-diagnostics t)
(setq lsp-ui-sideline-show-hover t)
(setq lsp-ui-sideline-show-code-actions t)

(use-package lsp-mode
	:ensure t
	:hook (

				 (web-mode . lsp-deferred)
				 (lsp-mode . (lambda ()
											 (let ((lsp-keymap-prexix "C-c l")))))
				 )
	:config
	(setq lsp-headerline-breadcrumb-enable nil)
	(setq lsp-enable-on-type-formatting nil)
	(setq lsp-enable-links nil)
	(define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
	:commands lsp lsp-deferred)

(use-package lsp-ui
	:ensure t
	:hook (lsp-mode . lsp-ui-mode)
	:custom
	(lsp-ui-doc-position 'bottom))

(use-package lsp-ivy
	:ensure t)

(setq lsp-language-id-configuration '((java-mode . "java")

																			(python-mode . "python")

																			(gfm-view-mode . "markdown")

																			(rust-mode . "rust")

																			(css-mode . "css")

																			(xml-mode . "xml")

																			(c-mode . "c")

																			(c++-mode . "cpp")

																			(objc-mode . "objective-c")

																			(web-mode . "html")

																			(html-mode . "html")

																			(sgml-mode . "html")

																			(mhtml-mode . "html")

																			(go-mode . "go")

																			(haskell-mode . "haskell")

																			(php-mode . "php")

																			(json-mode . "json")

																			(web-mode . "javascript")

																			;;(typescript-mode . "typescript")

																			))

(use-package flycheck
	:ensure t
	:init
	(global-flycheck-mode))

(add-hook 'c++-mode-hook 'lsp-deferred)
(add-hook 'c-mode-hook 'lsp-deferred)
(add-hook 'cuda-mode-hook 'lsp-deferred)
(add-hook 'objc-mode-hook 'lsp-deferred)

(use-package cmake-mode
	:ensure t
	:mode ("CMakeLists\\.txt\\'" "\\.cmake\\'")
	:hook (cmake-mode . lsp-deferred))

(add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

;; (add-to-list 'auto-mode-alist '("\\.js\\'" . typescript-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . typescript-mode))
(add-to-list 'auto-mode-alist '("\\.json\\'" . json-mode))

(use-package web-mode
  :ensure t
  :mode ("\\.html?\\'"
         "/themes/.*\\.php?\\'"
         "/\\(components\\|containers\\|src\\)/.*\\.js[x]?\\'"
         "\\.\\(handlebars\\|hbs\\)\\'")
  :config (progn
            (setq
             web-mode-markup-indent-offset 2
             web-mode-css-indent-offset 2
             web-mode-code-indent-offset 2
             web-mode-enable-auto-closing t
             web-mode-enable-auto-opening t
             web-mode-enable-auto-pairing t
             web-mode-enable-auto-indentation t
             web-mode-enable-auto-quoting t
             web-mode-enable-current-column-highlight t
             web-mode-enable-current-element-highlight t
             web-mode-content-types-alist
             '(("jsx" . "/\\(components\\|containers\\|src\\)/.*\\.js[x]?\\'")))))

;;(use-package js2-mode :ensure t
;;	:mode
;;	(("\\.js\\'" . js2-mode))
;;	:custom
;;	(js2-include-node-externs t)
;;	(js2-global-externs '("customElements"))
;;	(js2-highlight-level 3)
;;	(js2r-prefer-let-over-var t)
;;	(js2r-prefered-quote-type 2)
;;	(js-indent-align-list-continuation t)
;;	(global-auto-highlight-symbol-mode t)
;;	:config
;;	(setq js-indent-level 2)
;;	(advice-add #'js2-identifier-start-p
;;							:after-until
;;							(lambda (c) (eq c ?#))))


(add-hook 'typescript-mode-hook 'lsp-deferred)
(add-hook 'json-mode-hook 'lsp-deferred)
(add-hook 'web-mode-hook 'lsp-deferred)
(add-hook 'css-mode 'lsp-deferred)

(add-to-list 'auto-mode-alist '("\\.sh\\'" . sh-mode))
(add-hook 'sh-mode-hook 'lsp-deferred)

(use-package yaml-mode 
	:ensure t)

(add-to-list 'auto-mode-alist '("\\.yml\\'" . yaml-mode))
(add-to-list 'auto-mode-alist '("\\.yaml\\'" . yaml-mode))
(add-hook 'yaml-mode-hook 'lsp-deferred)

(global-set-key (kbd "M-<tab>") 'other-window)

(define-key evil-normal-state-map (kbd "C-t") 'treemacs)

(define-key evil-motion-state-map " " nil)

(define-key evil-normal-state-map (kbd "C-r") 'replace-regexp)
(define-key evil-normal-state-map (kbd "C-S-R") 'ag-project-regexp)

;; Double spaces for finding files
(define-key evil-normal-state-map (kbd "SPC SPC") 'helm-projectile-find-file)

(define-key evil-motion-state-map (kbd "SPC h") 'evil-window-left)
(define-key evil-motion-state-map (kbd "SPC j") 'evil-window-down)
(define-key evil-motion-state-map (kbd "SPC k") 'evil-window-up)
(define-key evil-motion-state-map (kbd "SPC l") 'evil-window-right)

(define-key evil-normal-state-map (kbd "SPC h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "SPC j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "SPC k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "SPC l") 'evil-window-right)

;; Quick buffer switching
(define-key evil-normal-state-map (kbd "M-l") 'next-buffer)
(define-key evil-normal-state-map (kbd "M-h") 'previous-buffer)

(define-key evil-normal-state-map (kbd "C-c c") 'uncomment-region)
(define-key evil-insert-state-map (kbd "C-c u") 'uncomment-region)
(define-key evil-normal-state-map (kbd "C-c c") 'comment-region)
(define-key evil-insert-state-map (kbd "C-c u") 'comment-region)

;; Move lines with M-j, M-k in normal and insert mode
(define-key evil-normal-state-map (kbd "M-k") 'move-text-up)
(define-key evil-normal-state-map (kbd "M-j") 'move-text-down)
(define-key evil-insert-state-map (kbd "M-k") 'move-text-up)
(define-key evil-insert-state-map (kbd "M-j") 'move-text-down)


(define-key evil-normal-state-map (kbd "M-<up>") 'move-text-up)
(define-key evil-normal-state-map (kbd "M-<down>") 'move-text-down)
(define-key evil-insert-state-map (kbd "M-<up>") 'move-text-up)
(define-key evil-insert-state-map (kbd "M-<down>") 'move-text-down)


(define-key evil-insert-state-map (kbd "C-c h") 'evil-window-left)
(define-key evil-insert-state-map (kbd "C-c j") 'evil-window-down)
(define-key evil-insert-state-map (kbd "C-c k") 'evil-window-up)
(define-key evil-insert-state-map (kbd "C-c l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "C-c h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-c j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-c k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-c l") 'evil-window-right)

(global-set-key (kbd "<escape>") 'keyboard-escape-quit)

(define-key evil-insert-state-map (kbd "M-b") 'helm-buffers-list)
(define-key evil-normal-state-map (kbd "M-b") 'helm-buffers-list)
