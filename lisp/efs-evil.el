;;; efs-evil.el --- Evil + general/leader + companion evil packages -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Leader-key and key-map config
(use-package general
  :demand t
  :after evil
  :config
  (general-evil-setup)
  (general-create-definer efs-leader
                          :prefix "SPC"
                          :global-prefix "C-SPC"
                          :keymaps 'override ; Override other defines for SPC. This might be a bad solution?
                          :states '(normal visual motion emacs))
  ;; Global keybindings
  (efs-leader
   ;:keymaps 'clojure-mode-map
   "SPC" 'execute-extended-command
   "'" '(vterm-toggle :wk "vterm toggle")

   "TAB" '(indent-for-tab-command :wk "indent-for-tab-command")

   "a" '(:ignore t :wk "applications")
   "a k" 'efs--elpaca-helper
   "a b" 'general-describe-keybindings
   "a o a" 'org-agenda
   "a o c" 'org-capture
   "a t" '(:ignore t :wk "tree-sitter")
   "a t i" '(efs-install-tree-sitter-grammars :wk "install grammars")
   "a t l" '(efs-list-tree-sitter-grammars :wk "list grammars")

   "b" '(:ignore t :wk "buffers")
   "b p" '(switch-to-prev-buffer :wk "previous buffer")
   "b n" '(switch-to-next-buffer :wk "next buffer")
   "b d" 'kill-current-buffer

   "c" '(:ignore t :wk "comments")

   "e" '(:ignore t :wk "errors")

   "f" '(:ignore t :wk "files")
   "f s" '(save-buffer :wk "save buffer")
   "f S" '(evil-write-all :wk "save all buffers")

   "g" '(:ignore t :wk "version control")

   "j"  '(:ignore t :wk "navigate")

   "l"  '(:ignore t :wk "layouts")

   "o" '(:ignore t :wk "org mode")

   "p"  '(:ignore t :wk "project")

   "r"  '(:ignore t :wk "registers")

   "s"  '(:ignore t :wk "search/symbol")

   "t"  '(:ignore t :wk "toggles")
   "t l" '(visual-line-mode :wk "toggle-truncate-lines")
   "t t" '(consult-theme :wk "choose theme")

   "q" '(:ignore t :wk "quit")
   "q q" '(save-buffers-kill-terminal :wk "quit emacs")

   "w" '(:ignore t :wk "windows")
   "w d" '(delete-window :wk "delete window")
   "w -" '(split-window-below :wk "split below")
   "w /" '(split-window-right :wk "split right")
   "w b" '(split-root-window-below :wk "split below all")
   "w r" '(split-root-window-right :wk "split right all")
   "w m" '(efs--toggle-maximize-buffer :wk "maximize buffer")

   "x" '(:ignore t :wk "text")
   "x r" '(:ignore t :wk "rectangles")
   "x r t" '(string-rectangle :wk "string-rectangle")
   "x r k" '(kill-rectangle :wk "kill-rectangle")

   ;; "f d e" '(lambda () (interactive) (find-file (expand-file-name "~/.emacs.d/Emacs.org")))
 )
  )

(elpaca-wait)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil
  :ensure t
  :init
  (setq evil-want-integration t) ; set to t before loading evil-collections
  (setq evil-want-keybinding nil) ; Set to nil before loading evil-collections
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  (setq evil-want-fine-undo nil)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo) ;; Emacs 28+ built-in linear undo with redo; vundo provides the tree visualizer
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Overwrite evil-delete-char to not add char to kill ring
  (defun efs--evil-delete-char (count &optional kill)
    "Delete the next COUNT chars.
If KILL is non-nil, also add the text to the kill ring.
COUNT defaults to 1, and KILL defaults to nil."
    (interactive "p\nP")
    (let ((beg (point))
          (end (min (point-max) (+ (point) count))))
      (delete-region beg end)))
  (evil-define-key 'normal 'global "x" 'efs--evil-delete-char) ;; Map "x" to efs--evil-delete-char

  ;; Set cursor color based on evil state
  (setq evil-normal-state-cursor '(box "DarkGoldenrod2")
        evil-insert-state-cursor '((bar . 2) "chartreuse3")
        evil-visual-state-cursor '((hbar . 2) "gray")
        evil-iedit-state-cursor '(box "firebrick1"))

  ;; Use visual line motions even outside of visual-line-mode buffers
  (evil-global-set-key 'motion "j" 'evil-next-visual-line)
  (evil-global-set-key 'motion "k" 'evil-previous-visual-line)

  (evil-set-initial-state 'messages-buffer-mode 'normal)
  (evil-set-initial-state 'dashboard-mode 'normal)
  (mapc (lambda (mode)
          (evil-set-initial-state mode 'emacs))
        ; Defined modes that should not use evil
        '(eww-mode
          profiler-report-mode
          elfeed-mode
          pdf-view-mode
          eshell-mode
          vterm-mode)))

(with-eval-after-load 'evil
  (with-eval-after-load 'elpaca-ui   (evil-make-intercept-map elpaca-ui-mode-map))
  (with-eval-after-load 'elpaca-info (evil-make-intercept-map elpaca-info-mode-map)))

(use-package evil-collection
  :after evil
  :diminish evil-collection-unimpaired-mode
  :config
  (evil-collection-init) ;; Register evil binding for all modes at once instead of calling individual *-setup functions
  (setq evil-want-keybinding t)
  ;; Set Avy to use actual words instead of sequences of letters (requires Avy 0.5.0)
  (setq avy-style 'words)
  (efs-leader
   "j j" '(evil-avy-goto-char-timer :wk "jump to char")
   "j l" '(evil-avy-goto-line :wk "jump to line")
   "j w" '(evil-avy-goto-word-or-subword-1 :wk "jump to word")))

(use-package evil-escape
  :ensure t
  :diminish evil-escape-mode
  :init
  (setq evil-escape-key-sequence "fd"
        evil-escape-delay 0.15)
  :config
  (evil-escape-mode))

(use-package evil-iedit-state
  :after evil
  :config
  (efs-leader
   "s e" '(evil-iedit-state/iedit-mode :wk "iedit-mode at point")
   ))

(use-package evil-org
  :ensure t
  :after org
  :diminish evil-org-mode
  :hook
  (org-mode . evil-org-mode)
  (evil-org-mode . (lambda () (evil-org-set-key-theme '(textobjects insert navigation additional shift todo)))) ;; Enable all key bindings
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package vundo
  :ensure t
  :commands (vundo)
  :config
  (setq vundo-glyph-alist vundo-unicode-symbols)
  (efs-leader
    "a u" '(vundo :wk "vundo")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil-nerd-commenter
  :after evil
  :config
  (efs-leader
   "c l" '(evilnc-comment-or-uncomment-lines :wk "comment-or-uncomment-lines")
   "c p" '(evilnc-comment-or-uncomment-paragraphs :wk "comment-or-uncomment-paragraph")
   "c y" '(evilnc-copy-and-comment-lines :wk "copy-and-comment-lines")
   )
  )

;; Highlight text, press "S-<delimiter>" to surround text with delimiters.
;; Use "(" to include spaces around delimiters, use ")" for no spaces.
(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1)
  )

(provide 'efs-evil)
;;; efs-evil.el ends here
