;;; efs-misc.el --- Yasnippet, no-littering, tramp, persp-mode, ws-butler, hl-todo, etc. -*- lexical-binding: t; -*-

;; Helper functions for snippets templates
;; Format:          \sum_{}^{}
;; Snippet example: \sum_{$1}^{$2} ${3:$$(yas-delete-if-empty)}
(defun yas-delete-if-empty ()
  (save-excursion
    (when (re-search-backward "\\\\sum\\(_{}\\)^{.+}" (line-beginning-position) t)
      (replace-match "" t t nil 1))))

;; Snippets settings
(use-package yasnippet
  :init
  ; Add custom snippets dir
  (setq yas-snippet-dirs '("~/.config/emacs-from-scratch/snippets" "~/etc/spacemacs.d/private/snippets/" "~/etc/spacemacs.d/layers/+completion/auto-completion/local/snippets" yasnippet-snippets-dir))
  :defer t)

(use-package yasnippet-snippets
  :defer t
  :after yasnippet)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package no-littering
  :ensure t
  :defer nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Remote access via TRAMP
(require 'tramp)
(setq tramp-default-method "sshx"
      ;; tramp-default-user-alist '(("\\`su\\(do\\)?\\'" nil "root"))
      ;; tramp-default-user "chrbirks"
      ;; tramp-default-host "192.168.1.7"
      ;; use the settings in ~/.ssh/config instead of Tramp's
      tramp-use-ssh-controlmaster-options nil
      ;; don't generate backups for remote files opened as root (security hazzard)
      backup-enable-predicate
      (lambda (name)
        (and (normal-backup-enable-predicate name)
             (not (let ((method (file-remote-p name 'method)))
                    (when (stringp method)
                      (member method '("su" "sudo"))))))))

(use-package command-log-mode
  :commands command-log-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package indent-guide
  :diminish indent-guide-mode
  :commands (indent-guide-mode)
  :config
  (setq indent-guide-char "▒")
  (efs-leader
   "t i" '(indent-guide-mode :wk "indent-guide-mode")))

(use-package rainbow-delimiters
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package ialign
  :ensure t
  :after pcre2el
  :defer t)

;; Highlight words like todo, fixme, note, etc.
(use-package hl-todo
  :ensure t
  :config
  (setq hl-todo-exclude-modes nil
        hl-todo-keyword-faces
        '(("TODO"       . "#dc752f")
          ("DONE"       . "#86dc2f")
          ("NOTE"       . "#b1951d")
          ("HACK"       . "#b1951d")
          ("TEMP"       . "#b1951d")
          ("FIXME"      . "#dc752f")
          ("fogbugz"    . "#18be4e")
          ("XXX+"       . "#dc752f")
          ("\\?\\?\\?+" . "#dc752f")))
  (global-hl-todo-mode))

;; Highlight tabs in certain modes
(setq whitespace-style '(face tabs))
(add-hook 'vhdl-mode-hook #'whitespace-mode)
(add-hook 'verilog-mode-hook #'whitespace-mode)
(add-hook 'elpaca-after-init-hook (lambda ()
                                    (with-eval-after-load 'whitespace
                                      (set-face-attribute 'whitespace-tab nil :background "orange red")
                                      (require 'diminish)
                                      (diminish 'whitespace-mode))))

;; Clean up trailing whitespaces in modified lines on save
;; Run: "whitespace-toggle-options" "?" "r" to visualize trailing whitespaces
(use-package ws-butler
  :diminish ws-butler-mode
  :hook ((vhdl-mode verilog-mode) . ws-butler-mode)
  :config
  ;; Set to only trim whitespaces from modified lines
  (setq ws-butler-trim-predicate
        (lambda (beg end)
          (buffer-modified-p))))

;; NOTE: Buffers are shared between all perspectives when using persp-mode
(use-package persp-mode
  :ensure t
  :custom-face
  ;; Set face for persp-mode modeline for when open buffer is not in current perspective
  (persp-face-lighter-buffer-not-in-persp ((t (:background "gold" :foreground "#00F" :weight bold))))
  :init
  (persp-mode)
  :config
  ;; Only show open buffers and not recent files
  (with-eval-after-load 'marginalia
    (add-to-list 'marginalia-command-categories '(persp-switch-to-buffer* . buffer)))

  (defun efs--current-layout-name ()
    "Get name of the current perspective."
    (safe-persp-name (get-frame-persp)))
  (defun efs--layouts-ts-kill ()
    "Kill current perspective"
    (interactive)
    (persp-kill (efs--current-layout-name)))
  (defun efs--save-persp-state ()
    (interactive)
    (unless (efs--magit-status-active-p)
      (let ((inhibit-message t)) ;; Suppress printing save-message to minibuffer
        (persp-save-state-to-file persp-auto-save-fname))))
  (run-with-timer 0 (* 10 60) 'efs--save-persp-state) ;; Save perspective every 10 minutes
  (setq persp-auto-resume-time -1 ;; No autoload buffers
        ;; persp-save-dir ""
        persp-set-last-persp-for-new-frames nil
        persp-reset-windows-on-nil-window-conf t
        persp-autokill-buffer-on-remove t
        persp-add-buffer-on-after-change-major-mode t
        persp-sort 'created
        persp-show-modestring t
        persp-auto-save-opt 2
        persp-auto-save-num-of-backups 5
        persp-auto-save-persps-to-their-file-before-kill 'persp-file
        persp-kill-foreign-buffer-behaviour 'kill)
  (with-eval-after-load "vterm"
    (persp-def-auto-persp "vterm"
                          :parameters '((dont-save-to-file . t))
                          :mode 'term-mode
                          :dyn-env '(after-switch-to-buffer-functions ;; prevent recursion
                                     (persp-add-buffer-on-find-file nil)
                                     persp-add-buffer-on-after-change-major-mode)
                          :hooks '(after-switch-to-buffer-functions)
                          :switch 'window))
  (efs-leader
   ;; "l n" '(persp-add-new :wk "new persp")
   "l l" '(persp-switch :wk "switch persp")
   "l s" '(persp-save-state-to-file :wk "save all to file")
   "l S" '(persp-save-to-file-by-names :wk "save persp to file")
   "l L" '(persp-load-state-from-file :wk "load from file")
   "l x" '(efs--layouts-ts-kill :wk "kill layout")
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(use-package casual-bookmarks
;  :ensure (:host github :repo "kickingvegas/casual-bookmarks")
;  :bind (:map bookmark-bmenu-mode-map
;              ("C-o" . casual-bookmarks-tmenu)
;              ("S" . casual-bookmarks-sortby-tmenu)
;              ("J" . bookmark-jump))
;  :after (bookmark))

;(use-package casual-calc
;  :ensure (:host github :repo "kickingvegas/casual-calc")
;  :bind (:map
;         calc-mode-map
;         ("C-o" . casual-calc-tmenu)
;         :map
;         calc-alg-map
;         ("C-o" . casual-calc-tmenu))
;  :after (calc))

;; Use copilot for Emacs
;(use-package copilot
;  :ensure (:host github :repo "copilot-emacs/copilot.el")
;  )

;; (use-package todo-explorer
;;   :ensure (:host nil :repo "https://github.com/chrbirks/emacs-todo-explorer" :branch "main")
;;   :bind ("C-c t" . todo-explorer))

(add-to-list 'load-path "/home/cbs/github/emacs-todo-explorer")
(require 'todo-explorer)
(global-set-key (kbd "C-c t") #'todo-explorer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package claude-code-ide
  :ensure (:host github :repo "manzaltu/claude-code-ide.el"))

(provide 'efs-misc)
;;; efs-misc.el ends here
