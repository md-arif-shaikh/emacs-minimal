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

;;; rainbow delimeters
(use-package rainbow-delimiters
  :straight t
  :hook
  (prog-mode . rainbow-delimiters-mode))

(show-paren-mode 1)
(setq show-paren-style 'parenthesis)

;;; line number
(setq linum-format "%4d \u2502 ")

;;; install autopair
(use-package autopair
  :straight t
  :defer t)
(autopair-global-mode)

;;; pyvenv
(use-package pyvenv
  :straight t
  :defer
  :config
  (setenv "WORKON_HOME" "~/miniconda3/envs/")
  (pyvenv-mode 1)
  (pyvenv-tracking-mode 1))

(add-hook 'python-mode-hook 'my/python-mode-hook)

(use-package highlight-indent-guides
  :straight t
  :defer t
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character))

(use-package company
  :straight t
  :defer
  :init
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 1))

;;; python lsp
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

(use-package python-docstring
  :straight t
  :hook
  (python-mode . python-docstring-mode))

(use-package magit
  :straight t)

(use-package doom-themes
  :straight t
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
	doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-gruvbox t))

(setq-default fill-column 79)
