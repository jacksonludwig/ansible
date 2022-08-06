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

(add-hook 'prog-mode-hook (lambda() (setq display-line-numbers 'relative)))
(add-hook 'emacs-startup-hook
          (lambda ()
            (custom-set-faces
             `(default ((t (:font "JetBrains Mono 13")))))))

(setq backup-directory-alist
        `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
        `((".*" ,temporary-file-directory t)))

(electric-pair-mode 1)
(show-paren-mode 1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;;; Completion
(straight-use-package 'consult)
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
(customize-set-variable 'completion-styles '(orderless basic))
(customize-set-variable 'completion-category-overrides '((file (styles . (basic partial-completion)))))

;;; vterm
(straight-use-package 'vterm)
(require 'vterm)

;;; Evil
(straight-use-package 'evil)
(straight-use-package 'evil-collection)
(straight-use-package 'evil-surround)
(straight-use-package 'evil-commentary)

(customize-set-variable 'evil-normal-state-cursor nil)
(customize-set-variable 'evil-insert-state-cursor nil)
(customize-set-variable 'evil-visual-state-cursor nil)
(customize-set-variable 'evil-motion-state-cursor nil)
(customize-set-variable 'evil-replace-state-cursor nil)
(customize-set-variable 'evil-operator-state-cursor nil)

(customize-set-variable 'evil-want-integration t)
(customize-set-variable 'evil-want-keybinding nil)
(customize-set-variable 'evil-respect-visual-line-mode t)
(customize-set-variable 'evil-undo-system 'undo-redo)
(customize-set-variable 'evil-split-window-below t)
(customize-set-variable 'evil-vsplit-window-right t)
(customize-set-variable 'evil-want-C-u-scroll t)

;; workaround for treesitter(?) bug
(customize-set-variable 'evil-ex-search-case 'sensitive)

(require 'evil)
(evil-mode 1)
(require 'evil-surround)
(global-evil-surround-mode 1)
(require 'evil-commentary)
(evil-commentary-mode)

(global-set-key (kbd "C-M-u") 'universal-argument)

;; start some modes in emacs state
(dolist (mode '(custom-mode
                eshell-mode
                vterm-mode
                term-mode))
  (add-to-list 'evil-emacs-state-modes mode))

(require 'evil-collection)
(evil-collection-init)

;;; Yas
(straight-use-package 'yasnippet)
(require 'yasnippet)
(yas-global-mode)

(straight-use-package 'flycheck)
(customize-set-variable 'flycheck-checker-error-threshold 800)
;; (setq flycheck-check-syntax-automatically '(mode-enabled save))
(require 'flycheck)

(straight-use-package 'lsp-ui)
(customize-set-variable 'lsp-ui-show-diagnostics t)
(customize-set-variable 'lsp-ui-show-hover nil)
(customize-set-variable 'lsp-ui-show-code-actions nil)
(require 'lsp-ui)

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
(customize-set-variable 'typescript-indent-level 2)
(require 'typescript-mode)
(define-derived-mode typescriptreact-mode typescript-mode
  "tsx?")
(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
(add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx))

(straight-use-package '(tsi :type git :host github :repo "orzechowskid/tsi.el"))
(require 'tsi-typescript)
(require 'tsi-css)
(require 'tsi-json)
(add-hook 'typescript-mode-hook (lambda() (tsi-typescript-mode 1)))
(add-hook 'json-mode-hook (lambda() (tsi-json-mode 1)))
(add-hook 'css-mode-hook (lambda() (tsi-css-mode 1)))

(straight-use-package 'prettier)
(customize-set-variable 'prettier-mode-sync-config-flag nil)
(customize-set-variable 'prettier-pre-warm 'full)
(require 'prettier)
(add-hook 'typescript-mode-hook (lambda() (prettier-mode 1)))
(add-hook 'json-mode-hook (lambda() (prettier-mode 1)))

;; (straight-use-package '(tsx-mode :type git :host github :repo "orzechowskid/tsx-mode.el"))
;; (require 'tsx-mode)

;;; LSP 
(straight-use-package 'lsp-mode)

;;; Company
(straight-use-package 'company)
(require 'company)
;; (straight-use-package 'company-box)
;; (customize-set-variable 'company-box-doc-enable nil)
;; (require 'company-box)
;; (add-hook 'company-mode-hook 'company-box-mode)

(customize-set-variable 'lsp-use-plists t)
(customize-set-variable 'lsp-headerline-breadcrumb-enable nil)
(customize-set-variable 'lsp-enable-symbol-highlighting nil)
(customize-set-variable 'lsp-signature-doc-lines 5)
(customize-set-variable 'lsp-eldoc-enable-hover nil)
(customize-set-variable 'lsp-on-type-formatting nil)
(customize-set-variable 'lsp-enable-indentation nil)
(customize-set-variable 'lsp-modeline-code-actions-enable nil)
(add-to-list 'display-buffer-alist
             '((lambda (buffer _) (with-current-buffer buffer
                                    (seq-some (lambda (mode)
                                                (derived-mode-p mode))
                                              '(help-mode))))
               (display-buffer-reuse-window display-buffer-below-selected)
               (reusable-frames . visible)
               (window-height . 0.33)))

(customize-set-variable 'lsp-eslint-server-command `("vscode-eslint-language-server" "--stdio"))

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
(diminish 'tree-sitter-mode)
(diminish 'lsp-mode)
(diminish 'company-mode)
(diminish 'company-box-mode)
(diminish 'evil-commentary-mode)
(diminish 'evil-collection-unimpaired-mode)

;;; Theme
(straight-use-package 'doom-themes)
(require 'doom-themes)
(load-theme 'modus-vivendi t)

;;; Bindings
(straight-use-package 'general)
(require 'general)
(general-def
  :states 'normal
  "SPC f r" 'consult-recent-file
  "SPC SPC" 'affe-find
  "SPC g g" 'affe-grep)
(general-def
  :keymaps 'company-active-map
  "C-y" 'company-complete-selection
  "C-e" 'company-abort
  "<return>" 'newline)
(general-def
  :states 'normal
  :predicate 'lsp-mode
  "SPC r n" 'lsp-rename
  "SPC c a" 'lsp-execute-code-action
  "K" 'lsp-describe-thing-at-point
  "] d" 'flycheck-next-error
  "[ d" 'flycheck-previous-error
  "g r" 'xref-find-references)
