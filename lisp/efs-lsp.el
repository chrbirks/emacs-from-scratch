;;; efs-lsp.el --- LSP, lsp-ui, lsp-treemacs, lsp-pyright, xref, flycheck -*- lexical-binding: t; -*-

(use-package lsp-mode
  :defer t
  :commands (lsp lsp-deferred lsp-mode)
  :hook ((verilog-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :init
  (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
  :config
  ;; Optimize parameters for lsp-mode
  (setq lsp-enable-file-watchers t
        lsp-file-watch-threshold 100000000
        read-process-output-max (* 1024 1024) ;; 1mb
        lsp-idle-delay 0.500)
  ;; Other lsp settings
  (setq ; Show info box
        lsp-ui-doc-enable t
        lsp-ui-doc-show-with-cursor nil
        lsp-ui-doc-show-with-mouse t
        lsp-ui-doc-header t
        lsp-ui-doc-include-signature t
        lsp-ui-doc-position 'at-point; 'top, 'bottom or 'at-point
        lsp-ui-doc-alignment 'window ; 'frame or 'window
        lsp-ui-doc-border "white"
        ;; lsp-ui-doc-max-width 150
        lsp-ui-doc-max-height 8
        lsp-ui-doc-use-childframe t
        lsp-ui-doc-use-webkit nil ;; Use lsp-ui-doc-webkit only in GUI. Requires compiling --with-xwidgets
        ; Show info from selected line on the same line
        lsp-ui-sideline-enable nil
        lsp-ui-sideline-show-symbol t
        lsp-ui-sideline-ignore-duplicate t
        lsp-ui-sideline-show-code-actions nil ; Show all possible LSP actions such as renaming, type casting, etc.
        ; Enable lenses
        lsp-lens-enable t
        lsp-lens-place-position 'above-line
        ; Eldoc
        lsp-eldoc-enable-hover nil ; Show LSP info in minibuffer?
        lsp-eldoc-enable-signature-help t
        lsp-eldoc-prefer-signature-help t
        lsp-eldoc-render-all t
        ; Modeline
        lsp-modeline-workspace-status-enable t
        lsp-modeline-code-actions-enable t
        lsp-modeline-diagnostics-enable t
        ; Auto completion
        lsp-completion-enable t
        lsp-completion-provider :none ;; Only ":(company-)capf" is supported
        lsp-completion-show-kind t
        lsp-completion-show-label-description t
        lsp-completion-show-detail t
        lsp-completion-enable-additional-text-edit nil
        lsp-enable-snippet t
        ; Headerline
        lsp-headerline-breadcrumb-mode t
        lsp-headerline-breadcrumb-enable t
        ; SignatureHelp
        lsp-signature-render-all t
        lsp-signature-auto-activate t
        lsp-signature-render-documentation t
        lsp-signature-function 'lsp-signature-posframe ; Use posframe with SignatureHelp. default: lsp-lv-message
        ; Other options
        ;; lsp-document-sync-method 'lsp--sync-incremental
        lsp-use-upstream-bindings t ; Bind all upstream managed `lsp-command-map` bindings behind `SPC m`. See https://emacs-lsp.github.io/lsp-mode/page/keybindings/
        lsp-enable-symbol-highlighting nil
        lsp-ui-imenu-enable t ;TODO 17-05-2019: Does not work. Should call lsp-ui-imenu which works
        lsp-enable-imenu t
        lsp-ui-imenu-auto-refresh t
        lsp-ui-peek-enable t
        lsp-ui-peek-always-show nil
        lsp-prefer-flymake nil ; 't' (flymake), 'nil' (flycheck), ':none' (None of them)
        lsp-ui-flycheck-enable t
        lsp-ui-flycheck-list-position 'bottom
        lsp-ui-flycheck-live-reporting t
        lsp-auto-configure t ; auto-configure lsp-ui and company-lsp
        lsp-auto-guess-root nil
        lsp-lens-mode t
        lsp-enable-indentation t
        lsp-enable-on-type-formatting nil
        lsp-enable-file-watchers t
        lsp-enable-xref t
        lsp-log-io nil ; log all messages to *lsp-log* for debugging
        lsp-print-performance nil ; check lsp-log data
        lsp-server-trace nil ; request tracing on the server side
        )
  ;; Start completion-at-point with C-SPC
  (evil-define-key 'insert global-map (kbd "C-SPC") #'completion-at-point)
  )

;; Diminish lsp-lens-mode
(add-hook 'elpaca-after-init-hook (lambda ()
                                    (with-eval-after-load 'lsp-lens
                                      (require 'diminish)
                                      (diminish 'lsp-lens-mode))))

;; See all error statistics in modeline
(with-eval-after-load 'lsp-mode
  (setq lsp-modeline-diagnostics-scope :project) ;project, workspace or file
  (add-hook 'lsp-managed-mode-hook 'lsp-modeline-diagnostics-mode))

(use-package lsp-ui
  :commands lsp-ui-mode
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :after lsp)

;; Only show active project in error list
(with-eval-after-load 'lsp-treemacs
  (setq lsp-treemacs-error-list-current-project-only t))

(use-package lsp-pyright
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp-deferred))))

(use-package xref) ;; On Emacs27 a newer version of xref is required for some lsp-mode functions to work

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Add verilog-mode and vhdl-mode to default-enabled flycheck modes
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  ;; :config
  ;; (add-to-list 'flycheck-global-modes 'verilog-mode)
  ;; (add-to-list 'flycheck-global-modes 'vhdl-mode)
  ;; ;; (setq 'flycheck-global-modes t)
  :config
  (defun efs--toggle-flycheck-error-list ()
    "Toggle flycheck's error list window.
If the error list is visible, hide it.  Otherwise, show it."
    (interactive)
    (if-let ((window (flycheck-get-error-list-window)))
        (save-selected-window (quit-window nil window))
      (let ((display-buffer-alist
             '(("^\\*Flycheck errors\\*$"
                (display-buffer-in-side-window)
                (side . bottom)
                (window-height . 0.15)))))
        (flycheck-list-errors))))
  ;; Only run flycheck on buffer save and enabling of flycheck mode - not while writing text. Increases LSP performance.
  ;; (setq flycheck-check-syntax-automatically '(save mode-enabled))
  ;; Global shortcuts
  (efs-leader
    "e n" '(flycheck-next-error :wk "next error")
    "e p" '(flycheck-previous-error :wk "previous error")
    "e l" '(efs--toggle-flycheck-error-list :wk "list errors")
    "e L" '(lsp-treemacs-errors-list :wk "lsp list errors")
    )
  )

(provide 'efs-lsp)
;;; efs-lsp.el ends here
