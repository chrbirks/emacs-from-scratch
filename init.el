;; -*- lexical-binding: t -*-

;; Topical configuration lives under lisp/. Load order matters:
;; bootstrap → evil (defines efs-leader) → everything else.
(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))

(require 'efs-bootstrap)   ;; Elpaca + use-package + diminish/delight
(require 'efs-evil)        ;; general/leader, evil + companions, undo-tree
(require 'efs-ui)          ;; Globals, GC, encodings, fonts, theme, modeline
(require 'efs-completion)  ;; vertico, orderless, marginalia, embark, consult, corfu, cape, helpful
(require 'efs-window)      ;; maximized-mode, which-key, vterm, symbol-overlay, transient, treemacs
(require 'efs-vc)          ;; magit, git-gutter, diff-hl
(require 'efs-projects)    ;; projectile, rg
(require 'efs-lsp)         ;; lsp-mode + family, flycheck
(require 'efs-org)         ;; org, org-roam, org-modern, org-projectile, org-download
(require 'efs-treesit)     ;; tree-sitter grammars, treesit-fold, treesit-auto
(require 'efs-hdl)         ;; verilog-mode, vhdl-mode, vhdl-ts-mode, TCL helpers
(require 'efs-misc)        ;; tramp, persp-mode, ws-butler, hl-todo, yasnippet, todo-explorer, etc.

;; Must be last
(elpaca-process-queues)
