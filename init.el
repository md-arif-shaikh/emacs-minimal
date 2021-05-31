;;; bootstrap straight
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
	(url-retrieve-synchronously
	  "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
	     'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;;; install use-package
(straight-use-package 'use-package)

;;; install doom-theme
(use-package doom-themes
  :straight t
  :config
  (load-theme 'doom-gruvbox t))

;;; line number
(setq linum-format "%4d \u2502 ")

;;; install autopair
(use-package autopair
  :straight t
  :defer t)
(autopair-global-mode)

;;; lsp
(use-package lsp-mode
  :straight t
  :commands lsp
  :config
  (setq lsp-enable-completion-at-point t
	lsp-prefer-capf t) 
  )

(setq gc-cons-threshold 100000000
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)

(use-package lsp-python-ms 
  :straight t
  :init
  (setq
   lsp-python-ms-auto-install-server t
   lsp-python-ms-executable (executable-find "python-language-server"))
  :hook
  (python-mode . (lambda ()
		    (require 'lsp-python-ms)
		     (lsp-deferred)))
  (python-mode . linum-mode))

;;; flycheck

(use-package flycheck
  :straight t
  :config
  (global-flycheck-mode)
  (setq flycheck-indication-mode 'left-fringe)
  (setq-default flycheck-disabled-checkers '(python-pylint))
  )

;;; company
(use-package company
  :straight t
  :defer
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1)
  (setq company-tooltip-align-annotations t)
  )

;;; magit
(use-package magit
  :straight t
  :defer
  :bind ("C-x g" . magit-status))

;;; vertico
(use-package vertico
  :straight t
  :init
  (vertico-mode))
