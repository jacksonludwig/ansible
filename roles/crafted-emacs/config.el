(require 'crafted-defaults)    ; Sensible default settings for Emacs
(require 'crafted-completion)  ; selection framework based on `vertico`
(require 'crafted-editing)     ; Whitspace trimming, auto parens etc.
(require 'crafted-evil)        ; An `evil-mode` configuration
(require 'crafted-org)         ; org-appear, clickable hyperlinks etc.

;;; Options
(add-hook 'emacs-startup-hook
          (lambda ()
            (custom-set-faces
             `(default ((t (:font "JetBrains Mono 12")))))))

(straight-use-package 'doom-themes)
(progn
  (disable-theme 'deeper-blue)
  (load-theme 'doom-zenburn t))

;;; Undo history
(straight-use-package 'undohist)
(require 'undohist)
(undohist-initialize)

;;; Treesitter
(straight-use-package 'tree-sitter)
(straight-use-package 'tree-sitter-langs)
(require 'tree-sitter)
(require 'tree-sitter-langs)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)

;;; Typescript
(straight-use-package 'typescript-mode)
(require 'typescript-mode)
(define-derived-mode typescriptreact-mode typescript-mode
"TS(X)")

;; use derived mode for tsx files
(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescriptreact-mode))
(add-to-list 'tree-sitter-major-mode-language-alist '(typescriptreact-mode . tsx))

(straight-use-package '(tsi :type git :host github :repo "orzechowskid/tsi.el"))
(require 'tsi-typescript)
(require 'tsi-css)
(require 'tsi-json)

(add-hook 'typescript-mode-hook (lambda() (tsi-typescript-mode)))

;;; Yas
(straight-use-package 'yasnippet)
(require 'yasnippet)
(yas-global-mode)

(straight-use-package 'flycheck)
(setq flycheck-check-syntax-automatically '(save))
(require 'flycheck)

;;; Completion modifications
(straight-use-package 'general)
(require 'general)
(general-def
 :keymaps 'completion-in-region-mode
 :definer 'minor-mode
 :states 'insert
 :predicate 'corfu-mode
 "C-n" 'corfu-next
 "C-p" 'corfu-previous
 "C-y" 'corfu-complete
 "C-e" 'corfu-quit
 "<return>" 'newline)
(general-def
  :states 'normal
  "SPC f r" 'consult-recent-file)
(general-def
  :states 'normal
  :predicate 'lsp-mode
  "SPC l" 'lsp-command-map
  "SPC c a" 'lsp-execute-code-action
  "K" 'lsp-describe-thing-at-point
  "SPC z" 'lsp-eslint-apply-all-fixes ;; TODO: only bind this in ts(x) modes
  "gr" 'xref-find-references
  "]d" 'flycheck-next-error
  "[d" 'flycheck-previous-error)

;;; LSP
(straight-use-package 'which-key)
(require 'which-key)
(which-key-mode)

(setq lsp-completion-provider :none)
(defun corfu-lsp-setup ()
  (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
        '(orderless)))
(add-hook 'lsp-completion-mode-hook #'corfu-lsp-setup)

(advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible)

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
(setq read-process-output-max (* 1024 1024))
(require 'lsp-mode)
(add-hook 'typescriptreact-mode-hook #'lsp)
(add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)

;;; Magit
(straight-use-package 'magit)
(require 'magit)
