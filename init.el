(setq user-emacs-directory "~/.emacs.aniki"
      inhibit-startup-message t
      backup-inhibited t
      auto-save-default nil
      tab-width 4
	  dired-kill-when-opening-new-dired-buffer t
	  use-short-answers t
      custom-file "~/.emacs.aniki/custom.el")

(load custom-file t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(column-number-mode 1)
(global-display-line-numbers-mode t)
(delete-selection-mode 1)
;; (load-theme 'tango-dark)
(hl-line-mode 1)

(dolist (mode '(org-mode-hook
                term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(set-face-attribute 'default nil :height 150 :font "monaco")
(setq-default default-text-properties '(line-spacing 0.2 line-height 1.1))

(defadvice term-handle-exit
    (after term-kill-on-exit activate)
  (kill-buffer))

(global-set-key
 (kbd "C-`")
 (lambda()
   (interactive)
   (term "zsh")))

(dolist (map (list emacs-lisp-mode-map lisp-interaction-mode-map))
  (define-key map (kbd "C-c C-c") 'eval-buffer))

(setq package-archives
	  '(("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

;; (setq auto-save-file-name-transforms
;;      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(use-package doom-themes
  :init (load-theme 'doom-one t))


(use-package doom-modeline
  :init (doom-modeline-mode 1)
  :custom ((doom-modeline-height 15)))

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 1))

(use-package ivy
  :diminish
  :bind (("C-s" . swiper)
         :map ivy-switch-buffer-map
         ("C-d" . ivy-switch-buffer-kill)
         :map ivy-reverse-i-search-map
         ("C-d" . ivy-reverse-i-search-kill))
  :config
  (setq ivy-initial-inputs-alist nil)
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :init
  (ivy-rich-mode 1))

(use-package counsel
  :bind (("C-M-j" . 'counsel-switch-buffer)
         :map minibuffer-local-map
         ("C-r" . 'counsel-minibuffer-history))
  :custom
  (counsel-linux-app-format-function #'counsel-linux-app-format-function-name-only)
  :config
  (counsel-mode 1))

(use-package ivy-prescient
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  ;; Uncomment the following line to have sorting remembered across sessions!
  ;(prescient-persist-mode 1)
  (ivy-prescient-mode 1))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ([remap describe-function] . counsel-describe-function)
  ([remap describe-command] . helpful-command)
  ([remap describe-variable] . counsel-describe-variable)
  ([remap describe-key] . helpful-key))

(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode -1)
  (visual-line-mode 1)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((groovy . t))))

(use-package org
  :pin org
  :commands (org-capture org-agenda)
  :hook (org-mode . efs/org-mode-setup)
  :custom
  (org-image-actual-width nil)
  (org-confirm-babel-evaluate nil)
  :config
  (setq org-ellipsis " ▾"))

(use-package org-bullets
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(use-package lsp-mode
  :commands (lsp)
  :init
   (setq-default lsp-groovy-classpath 
	[
	 "/usr/local/opt/groovy/libexec/lib"
	 "/home/aniki/.m2/repository/org/springframework/boot/spring-boot-starter/2.3.1.RELEASE/spring-boot-starter-2.3.1.RELEASE.jar"
	 ])
  (setq lsp-keymap-prefix "C-c l") ;; Or 'C-l', 's-l'
  :config
  (lsp-enable-which-key-integration t)
  :custom
  (lsp-lens-enble nil)
  (lsp-ui-doc-show-with-cursor nil)
  (lsp-ui-doc-show-with-mouse nil)
  :bind (:map lsp-mode-map
		 ("C-c C-j" . 'lsp-ui-doc-show)))

;; (use-package lsp-ui
;; ;;  :hook (lsp-mode . lsp-ui-mode)
;;   :custom
;;   (lsp-ui-doc-position 'bottom))

(use-package company
    :bind (:map company-active-map
		("<tab>" . company-complete-selection))
    :custom
    (company-minimum-prefix-length 1)
    (company-idle-delay 0.0)
    :init
    (global-company-mode))

(use-package company-box
  :hook (company-mode . company-box-mode))

;; (use-package projectile
;;   :diminish projectile-mode
;;   :config (projectile-mode)
;;   :custom ((projectile-completion-system 'ivy)
;; 		   (projectile-globally-ignored-file-suffixes "class")
;; 		   (projectile-indexing-method 'hybrid))
;;   :bind-keymap
;;   ("C-c p" . projectile-command-map)
;;   :bind (:map project-prefix-map ("a" . projectile-add-known-project))
;;   :init
;;   ;; NOTE: Set this to the folder where you keep your Git repos!
;;   (when (file-directory-p "~/code")
;;     (setq projectile-project-search-path '("~/code")))
;;   (setq projectile-switch-project-action #'projectile-dired))

(use-package magit
  :commands magit-status)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :bind (("C-x C-j" . dired-jump))
  :hook (dired-mode . dired-omit-mode)
  :custom
  (dired-omit-mode t)
  (dired-omit-files "^\\.")
  (dired-listing-switches "-lah"))

(use-package dired-single
  :commands (dired dired-jump))

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(defun indent-between-pair (&rest _ignored)
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))
(use-package smartparens
  :init (smartparens-global-mode 1)
  :config
  (dolist (mode (list 'emacs-lisp-mode 'lisp-mode 'lisp-interaction-mode))
	(sp-local-pair mode "'" nil :actions nil)
	(sp-local-pair mode "`" nil :actions nil))
  (sp-local-pair 'prog-mode "{" nil :post-handlers '((indent-between-pair "RET")))
  (sp-local-pair 'prog-mode "[" nil :post-handlers '((indent-between-pair "RET")))
  (sp-local-pair 'prog-mode "(" nil :post-handlers '((indent-between-pair "RET"))))

(use-package set-bookmarks
  :load-path user-emacs-directory)

(use-package js2-mode
  :mode "\\.pac\\'")

(use-package fzf)

(use-package qml-mode
  :mode "\\.qml\\'")

(use-package cmake-mode)

(use-package emmet-mode
  :mode ("\\.html\\'" "\\.js\\'"))

(defun rust-run()
  (interactive)
  (compile "cargo run"))

(use-package rust-mode
  :mode "\\.rs\\'"
  :hook (rust-mode . lsp-deferred)
  :bind (:map rust-mode-map
			  ("C-c C-c" . 'rust-run)))

(defun go-run()
  (interactive)
  (compile (format "go run %s" (buffer-file-name))))

(defmacro run!(format)
  `(lambda()
	 (interactive)
	 (compile (format ,format (buffer-file-name)))))

(use-package go-mode
  :custom (tab-width 4)
  :hook (go-mode . lsp-deferred)
  :config (bind-key "C-c C-c" (run! "go run %s") go-mode-map))

(defun run-java()
  (interactive)
  (compile (format "java %s" (buffer-file-name))))

(use-package cc-mode
  :bind (:map java-mode-map
			   ("C-c C-c" . run-java)))

(use-package server
  :custom (server-name "aniki")
  :config (unless (server-running-p) (server-start)))

(use-package aniki
  :commands (aniki-ctrl-w dired-current)
  :load-path user-emacs-directory
  :bind-keymap ("C-t" . aniki-map)
  :bind
  ("C-w" . 'aniki-ctrl-w)
  ("C-x C-d" . 'dired-current))

(defmacro mk-run(name format)
  `(defun ,name()
	 (interactive)
	 (compile (format ,format (buffer-file-name)))))

(mk-run run-groovy "groovy %s")

(use-package groovy-mode
  :mode ("\\.gradle\\'" "\\.groovy\\'")
  :bind (:map groovy-mode-map
			  ("C-c C-c" . 'run-groovy)))

(use-package gradle-mode)

(use-package lsp-java
  :config
  (add-hook 'java-mode-hook 'lsp)
  (add-hook 'java-mode-hook 'lsp-ui-mode))

(use-package yaml-mode
  :hook (yaml-mode . whitespace-mode))

(use-package mhtml-mode
  :mode "\\.html\\'"
  :hook (mhtml-mode . emmet-mode))

(use-package java-snippets)

(use-package yasnippet
  :config (yas-global-mode))

(global-set-key (kbd "M-j") 'scroll-up-line)
(global-set-key (kbd "M-S-j") 'scroll-down-line)

(use-package org-roam
  :custom (org-roam-directory "~/org/roam")
  :config (org-roam-db-autosync-mode))
