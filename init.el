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

;; elpy
(use-package elpy
  :straight t
  :defer t
  :config
  (advice-add 'python-mode :before 'elpy-enable)
  (add-hook 'elpy-mode-hook (lambda () (highlight-indentation-mode -1)))
  (flymake-mode)
  (remove-hook 'elpy-modules 'elpy-module-flymake)
  :hook (python-mode . linum-mode)
  )
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook)

(use-package highlight-indent-guides
  :straight t
  :defer t
  :config
  (add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
  (setq highlight-indent-guides-method 'character))

(use-package company-jedi
  :straight t
  :defer)

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
