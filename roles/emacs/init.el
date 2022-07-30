;;; Straight-el boostrap
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

;;; Options
(setq-default indent-tabs-mode nil)
(setq use-short-answers t)
(setq read-process-output-max (* 1024 1024))
(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook #'recentf-mode)
(customize-set-variable 'kill-do-not-save-duplicates t)
(savehist-mode 1)

(add-hook 'emacs-startup-hook
          (lambda ()
            (custom-set-faces
             `(default ((t (:font "JetBrains Mono 13")))))))

(setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t)))

(add-hook 'prog-mode-hook (lambda () (setq display-line-numbers 'relative)))
(electric-pair-mode 1)
(show-paren-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;;; Completion
(straight-use-package 'consult)
(straight-use-package 'embark)
(straight-use-package 'embark-consult)
(straight-use-package 'marginalia)
(straight-use-package 'orderless)
(straight-use-package 'vertico)

;; Vertico
(require 'vertico)
(customize-set-variable 'vertico-cycle t)
(vertico-mode 1)

;; Marginalia
(require 'marginalia)
(customize-set-variable 'marginalia-annotators '(marginalia-annotators-heavy marginalia-annotators-light nil))
(marginalia-mode 1)

;; this is actually consult related
(setq completion-in-region-function #'consult-completion-in-region)

;; Orderless
(require 'orderless)
(customize-set-variable 'completion-styles '(orderless))
(customize-set-variable 'completion-category-overrides '((file (styles . (partial-completion)))))

;; Embark
(require 'embark)
(require 'embark-consult)
(global-set-key [remap describe-bindings] #'embark-bindings)
(global-set-key (kbd "C-.") 'embark-act)

;; Use Embark to show bindings in a key prefix with `C-h`
(setq prefix-help-command #'embark-prefix-help-command)
(with-eval-after-load 'embark-consult
  (add-hook 'embark-collect-mode-hook #'consult-preview-at-point-mode))

;;; Evil
(straight-use-package 'evil)
(straight-use-package 'evil-collection)

(customize-set-variable 'evil-want-integration t)
(customize-set-variable 'evil-want-keybinding nil)
(customize-set-variable 'evil-respect-visual-line-mode t)
(customize-set-variable 'evil-undo-system 'undo-redo)
(customize-set-variable 'evil-split-window-below t)
(customize-set-variable 'evil-vsplit-window-right t)
(customize-set-variable 'evil-want-C-u-scroll t)

(require 'evil)
(evil-mode 1)

(global-set-key (kbd "C-M-u") 'universal-argument)

;; start some modes in emacs state
(dolist (mode '(custom-mode
                eshell-mode
                term-mode))
  (add-to-list 'evil-emacs-state-modes mode))

(require 'evil-collection)
(evil-collection-init)

;;; Yas
(straight-use-package 'yasnippet)
(require 'yasnippet)
(yas-global-mode)

(straight-use-package 'flycheck)
(setq flycheck-check-syntax-automatically '(mode-enabled save))
(require 'flycheck)

;;; Basic fuzzy search
(straight-use-package 'affe)
(require 'affe)

;;; Which key
(straight-use-package 'which-key)
(require 'which-key)
(which-key-mode)

;;; Treesitter
(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)
(require 'tree-sitter)
(require 'tree-sitter-langs)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;;; JSON
(straight-use-package 'json-mode)
(require 'json-mode)

;;; Typescript
(straight-use-package 'typescript-mode)
(require 'typescript-mode)
(define-derived-mode typescriptreact-mode typescript-mode
  "typescript {tsx}")
(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
(add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx))

(straight-use-package '(tsi :type git :host github :repo "orzechowskid/tsi.el"))
(require 'tsi-typescript)
(require 'tsi-css)
(require 'tsi-json)
(add-hook 'typescript-mode-hook (lambda() (tsi-typescript-mode 1)))
(add-hook 'json-mode-hook (lambda() (tsi-json-mode 1)))
(add-hook 'css-mode-hook (lambda() (tsi-css-mode 1)))

;;; Company
(straight-use-package 'company)
(require 'company)
(straight-use-package 'company-box)
(require 'company-box)
(add-hook 'company-mode-hook 'company-box-mode)

;; (straight-use-package '(tsx-mode :type git :host github :repo "orzechowskid/tsx-mode.el"))
;; (require 'tsx-mode)

;;; LSP
(straight-use-package 'lsp-mode)
(setq lsp-use-plists t)
(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-enable-symbol-highlighting nil)
(setq lsp-signature-doc-lines 1)
(setq lsp-eldoc-enable-hover nil)
(setq lsp-on-type-formatting nil)
(setq lsp-enable-indentation nil)
(add-to-list 'display-buffer-alist
             '((lambda (buffer _) (with-current-buffer buffer
                                    (seq-some (lambda (mode)
                                                (derived-mode-p mode))
                                              '(help-mode))))
               (display-buffer-reuse-window display-buffer-below-selected)
               (reusable-frames . visible)
               (window-height . 0.33)))

(setq lsp-eslint-server-command `("vscode-eslint-language-server" "--stdio"))

(require 'lsp-mode)
(add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
(add-hook 'typescriptreact-mode-hook 'lsp)

;;; Magit
(straight-use-package 'magit)
(require 'magit)

;;; Diminish
(straight-use-package 'diminish)
(require 'diminish)
(diminish 'yas-minor-mode)
(diminish 'which-key-mode)
(diminish 'eldoc-mode)
(diminish 'evil-collection-unimpaired-mode)
(diminish 'tree-sitter-mode)
(diminish 'lsp-mode)
(diminish 'company-mode)
(diminish 'company-box-mode)

;;; Theme
(straight-use-package 'doom-themes)
(require 'doom-themes)
(load-theme 'doom-xcode t)

;;; Bindings
(straight-use-package 'general)
(require 'general)
(general-def
  :keymaps 'company-active-map
  "C-y" 'company-complete-selection
  "C-e" 'company-abort
  "<return>" 'newline)
(general-def
  :states 'normal
  "<escape>" 'evil-ex-nohighlight
  "SPC f r" 'consult-recent-file
  "SPC SPC" 'affe-find
  "SPC g g" 'affe-grep)
(general-def
  :states 'normal
  :predicate 'lsp-mode
  "SPC l" lsp-command-map
  "SPC c a" 'lsp-execute-code-action
  "SPC r n" 'lsp-rename
  "K" 'lsp-describe-thing-at-point
  "SPC z" 'lsp-eslint-apply-all-fixes ;; TODO: only bind this in ts(x) modes
  "gr" 'xref-find-references
  "]d" 'flycheck-next-error
  "[d" 'flycheck-previous-error)
