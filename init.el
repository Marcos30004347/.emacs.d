;; Setup

(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/"))
(add-to-list 'package-archives '("elpa" . "http://elpa.gnu.org/packages/"))

(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t
				use-package-expand-minimally t))


																				;(setq make-backup-files nil) ; stop creating ~ files
(setq auto-save-file-name-transforms
      `((".*" "~/.emacs.d/auto-save-list/" t)))
(setq backup-directory-alist '(("." . "~/.emacs.d/backup"))
			backup-by-copying t    ; Don't delink hardlinks
			version-control t      ; Use version numbers on backups
			delete-old-versions t  ; Automatically delete excess backups
			kept-new-versions 20   ; how many of the newest versions to keep
			kept-old-versions 5    ; and how many of the old
			)
(setq mac-pass-command-to-system nil)
(setq ns-alternate-modifier 'none)
(setq ns-right-alternate-modifier 'none)
(setq mac-command-modifier 'meta)
(setq indent-tabs-mode t)
(setq inhibit-startup-message t)

(show-paren-mode 1)
(delete-selection-mode 1)
(global-visual-line-mode t)
(global-prettify-symbols-mode +1)
(blink-cursor-mode 0)
(tool-bar-mode -1)
(menu-bar-mode -1)
(scroll-bar-mode 0)
(global-display-line-numbers-mode 1)

(setq visible-bell nil)
(setq tab-always-indent 'complete indent-tabs-mode nil)
(setq-default indent-tabs-mode t)
(setq-default tab-width 2)
(setq-default word-wrap t)





;; Evil Mode
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


;; Key bindings
(define-key evil-insert-state-map (kbd "TAB") 'tab-to-tab-stop)
(define-key evil-motion-state-map " " nil)
(define-key evil-normal-state-map (kbd "C-x s") 'ag-project)
(define-key evil-normal-state-map (kbd "C-s") 'swiper)
(define-key evil-normal-state-map (kbd "C-r") 'replace-regexp)
(define-key evil-visual-state-map (kbd "c") 'uncomment-region)
(define-key evil-visual-state-map (kbd "u") 'comment-region)
(define-key evil-normal-state-map (kbd "M-k") 'move-text-up)
(define-key evil-normal-state-map (kbd "M-j") 'move-text-down)
(define-key evil-insert-state-map (kbd "M-k") 'move-text-up)
(define-key evil-insert-state-map (kbd "M-j") 'move-text-down)
(define-key evil-insert-state-map (kbd "M-b") 'helm-buffers-list)
(define-key evil-normal-state-map (kbd "M-b") 'helm-buffers-list)
(define-key evil-normal-state-map (kbd "M-i") 'indent-region)
(define-key evil-normal-state-map (kbd "M-l") 'evil-next-buffer)
(define-key evil-normal-state-map (kbd "M-h") 'evil-prev-buffer)
(define-key evil-normal-state-map (kbd "M-j") 'evil-window-next)
(define-key evil-normal-state-map (kbd "M-k") 'evil-window-prev)
(define-key evil-normal-state-map (kbd "<backspace>") 'xref-go-back)
(define-key evil-normal-state-map (kbd "<return>") 'lsp-find-definition)
(define-key evil-normal-state-map (kbd "<S-return>") 'lsp-find-declaration)

;; Font
(set-face-attribute 'default nil :font "Iosevka" :height 200)
(set-face-attribute 'fixed-pitch nil :font "Iosevka" :height 200)
(load "~/.emacs.d/iosevka-lig")
(set-face-attribute 'mode-line nil :font "Fira Code" :height 120)
(use-package nerd-icons
  :ensure t
  :custom
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  )
(use-package all-the-icons
  :ensure t)

;; Theme
(use-package gruber-darker-theme :ensure t)
(load-theme 'gruber-darker t)

;; Path
(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-env "GOPATH"))


;; Highlights
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
(hl-todo-mode)
(add-hook 'text-mode-hook 'hl-todo-mode)


;; Projectile
(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :bind (:map projectile-mode-map
							("s-p" . projectile-command-map)
							("C-c p" . projectile-command-map)))
(setq projectile-completion-system 'ivy)

;; Ivy
(use-package ivy
  :ensure t
  :diminish
  :config
  (ivy-mode 1))

;; Swiper
(use-package swiper :ensure t)
(use-package counsel :ensure t)


;; Move text
(use-package move-text :ensure t)

;; Helm
(use-package helm
  :ensure t
  :init
  (setq helm-split-window-in-side-p t
				helm-move-to-line-cycle-in-source t)
  :config 
  (helm-mode 1) ;; Most of Emacs prompts become helm-enabled
  (helm-autoresize-mode 1) ;; Helm resizes according to the number of candidates
  (global-set-key (kbd "M-b") 'helm-buffers-list) ;; List buffers ( Emacs way )
  (define-key evil-ex-map "b" 'helm-buffers-list) ;; List buffers ( Vim way )
  (global-set-key (kbd "M-x") 'helm-M-x)  ;; Improved M-x menu
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)  ;; Show kill ring, pick something to paste
  :ensure t)

(use-package helm-projectile
	:ensure t
	:config
  (helm-projectile-on)
	(global-set-key (kbd "C-x C-f") 'helm-projectile-find-file)
	(global-set-key (kbd "C-x C-s") 'helm-projectile-switch-to-buffer)
	(global-set-key (kbd "C-x C-b") 'helm-projectile-switch-to-buffer))

(use-package helm-gitignore :ensure t)


;; AG
;; For  Ag to work, it is neccessary to install "The Silver Searcher" on you system, follow https://github.com/ggreer/the_silver_searcher#installation for instructions.
(use-package ag :ensure t)
(setq ag-highlight-search t)
(setq ag-reuse-window t)

(use-package wgrep :ensure t)
(use-package wgrep-ag :ensure t)



;; LSP
(use-package lsp-mode
  :ensure t
  :hook ((go-mode . lsp-deferred)
         (c-mode . lsp-deferred)
         (c++-mode . lsp-deferred)
         (python-mode . lsp-deferred)
         (js-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (web-mode . lsp-deferred))
  :commands lsp
  :config
  (setq lsp-prefer-flymake nil  ;; Prefer lsp-ui (flycheck) over flymake
        lsp-headerline-breadcrumb-enable nil))  ;; Optional: disable headerline breadcrumb
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :config
  (setq lsp-ui-doc-enable t
        lsp-ui-doc-use-webkit nil  ;; Use webkit for rendering documentation if available
        lsp-ui-doc-delay 0.3
        lsp-ui-doc-position 'at-point
        lsp-ui-sideline-enable t
        lsp-ui-sideline-show-diagnostics t
        lsp-ui-sideline-show-hover t
        lsp-ui-sideline-show-code-actions t
        lsp-ui-sideline-update-mode 'line))


;; Code Completion
(use-package company
  :ensure t
  :hook (prog-mode . company-mode)
  :config
  (setq company-minimum-prefix-length 1
        company-idle-delay 0.0
				lsp-completion-provider :capf)) ;; Set the delay before suggestions pop up
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

(use-package flycheck
  :ensure t
  :hook (prog-mode . flycheck-mode))


(use-package which-key
  :ensure t
  :config
  (which-key-mode))


;; Configure Go mode
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :hook ((before-save . lsp-format-buffer)  ;; Format buffer before saving
         (before-save . lsp-organize-imports))  ;; Organize imports before saving
  :config
  (setq tab-width 4
        indent-tabs-mode 1))

;; Configure C/C++ mode
(use-package ccls
  :ensure t
  :hook ((c-mode c++-mode objc-mode) . (lambda () (require 'ccls) (lsp))))

;; Configure python-mode for Python programming
(use-package python-mode
  :ensure t
  :hook (python-mode . lsp-deferred)
  :custom
  (python-shell-interpreter "python3"))

;; Configure js-mode and ts-mode for JavaScript and TypeScript programming
(use-package js-mode
  :ensure nil
  :mode "\\.js\\'"
  :hook (js-mode . lsp-deferred))

(use-package typescript-mode
  :ensure t
  :mode "\\.ts\\'"
  :hook (typescript-mode . lsp-deferred)
  :config
  (setq typescript-indent-level 2))

;; Configure web-mode for JavaScript, TypeScript, and Node.js
(use-package web-mode
  :ensure t
  :mode ("\\.jsx?\\'" "\\.tsx?\\'")
  :hook ((web-mode . lsp-deferred))
  :config
  (setq web-mode-content-types-alist
        '(("jsx" . "\\.js[x]?\\'")
          ("tsx" . "\\.ts[x]?\\'"))))

;; Optionally, configure dap-mode for debugging
(use-package dap-mode
  :ensure t
  :config
  (require 'dap-ui) 
  (require 'dap-go)  ;; Load the DAP adapter for Go
  (require 'dap-python)  ;; Load the DAP adapter for Go
  (require 'dap-node)  ;; Load the DAP adapter for Node.js
  (require 'dap-lldb)  ;; Load the DAP adapter for Node.js
  (dap-go-setup)
																				; (dap-python-setup)
  (dap-node-setup)
  (dap-mode t)
  (dap-ui-mode t)
  (tooltip-mode t)
	(dap-register-debug-template "C++ LLDB Debug"
															 (list :type "lldb-vscode"
																		 :request "launch"
																		 :name "C++ LLDB Debug"
																		 :gdbpath "lldb-mi"  ;; Adjust this path if necessary
																		 :program "${workspaceFolder}/a.out"
																		 :cwd "${workspaceFolder}")))



;; Rest client
(use-package restclient
  :ensure t
  :mode ("\\.http\\'" . restclient-mode))
(use-package company-restclient
  :ensure t
  :after (company)
  :config
  (add-to-list 'company-backends 'company-restclient))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
	 '(company-restclient restclient which-key wgrep-ag web-mode typescript-mode python-mode nerd-icons move-text lsp-ui ivy hl-todo helm-projectile helm-gitignore gruber-darker-theme go-mode flycheck exec-path-from-shell evil dap-mode company-box ccls all-the-icons ag)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
