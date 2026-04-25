;;; efs-window.el --- Maximized mode, which-key, recent/grep, winum, vterm, symbol-overlay, transient, treemacs -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Maximized Window Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Define minor-mode to indicate when window is maximized
(define-minor-mode efs-maximized-mode
  "Minor mode to indicate when window is maximized.
When active, disables window split functions to prevent changes to window configuration."
  :init-value nil
  :lighter nil
  (if efs-maximized-mode
      ;; Enable mode: disable split functions
      (progn
        (advice-add 'split-window-below :before-while #'efs--check-not-maximized)
        (advice-add 'split-window-right :before-while #'efs--check-not-maximized)
        (advice-add 'split-root-window-below :before-while #'efs--check-not-maximized)
        (advice-add 'split-root-window-right :before-while #'efs--check-not-maximized))
    ;; Disable mode: re-enable split functions
    (progn
      (advice-remove 'split-window-below #'efs--check-not-maximized)
      (advice-remove 'split-window-right #'efs--check-not-maximized)
      (advice-remove 'split-root-window-below #'efs--check-not-maximized)
      (advice-remove 'split-root-window-right #'efs--check-not-maximized))))

(defun efs--check-not-maximized (&rest _args)
  "Return nil if efs-maximized-mode is active, preventing the advised function from running."
  (when efs-maximized-mode
    (message "Window splits disabled while in maximized mode")
    nil)
  (not efs-maximized-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Unified Window Management System
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Core window configuration storage - single hash table for all window configs
(defvar efs--window-configs (make-hash-table :test #'equal)
  "Unified hash table for storing window configurations.
Keys are (perspective . identifier) pairs.")

;; Core window management functions
(defun efs--save-window-config (key)
  "Save current window configuration with KEY identifier for current perspective."
  (puthash (cons persp-last-persp-name key)
           (current-window-configuration)
           efs--window-configs))

(defun efs--restore-window-config (key &optional delete-after)
  "Restore window configuration for KEY identifier in current perspective.
If DELETE-AFTER is non-nil, remove the configuration after restoring."
  (let* ((full-key (cons persp-last-persp-name key))
         (config (gethash full-key efs--window-configs)))
    (when config
      (set-window-configuration config)
      (when delete-after
        (remhash full-key efs--window-configs)))
    config))

(defun efs--has-window-config-p (key)
  "Check if window configuration exists for KEY in current perspective."
  (gethash (cons persp-last-persp-name key) efs--window-configs))

(defun efs--maximize-window (&optional key)
  "Maximize current window, saving configuration with optional KEY."
  (efs--save-window-config (or key 'maximize))
  ;; Reset the no-delete-other-windows parameter for all windows
  (dolist (win (window-list))
    (set-window-parameter win 'no-delete-other-windows nil))
  (delete-other-windows))

;; Toggle maximize buffer function using unified system
(defun efs--toggle-maximize-buffer ()
  "Toggle the maximization of the current buffer.
Plays nice with special buffers like treemacs."
  (interactive)
  (unless (bound-and-true-p winner-mode)
    (winner-mode 1))
  (if (efs--has-window-config-p 'maximize)
      ;; If in maximized state, restore
      (progn
        (efs--restore-window-config 'maximize t)
        (efs-maximized-mode -1)  ; Disable maximized mode
        (message "Window restored"))
    ;; If not in maximized state, maximize
    (progn
      (efs--maximize-window 'maximize)
      (efs-maximized-mode 1)   ; Enable maximized mode
      (message "Window maximized"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package wgrep
  :defer t)

(use-package deadgrep
  :commands (deadgrep)
  :config
  (setq deadgrep-extra-arguments '("!*~" "--glob" "--no-config"))) ;; Exclude *~ files. NOTE: The words must be written in reverse order

(use-package recentf
  :ensure nil ;; recentf is a native package
  :delight (recentf-mode)
  :init
  (defun efs--recentf-save-list ()
    (interactive)
    (let ((inhibit-message t)) ;; Suppress printing save-message to minibuffer
      (recentf-save-list)))
  (run-with-timer 0 (* 5 60) 'efs--recentf-save-list) ;; Save recent files every 5 minutes
  :config
  (setq recentf-save-file (expand-file-name "var/recentf-save.el" user-emacs-directory)
        ;; recentf-auto-cleanup 'never
        recentf-max-menu-items 25
        recentf-max-saved-items 25)
  (add-to-list 'recentf-exclude ".*/\\.config/emacs-from-scratch/var/persp-mode/.*")
  (add-to-list 'recentf-exclude ".*/\\.config/emacs-from-scratch/var/treemacs/persist\\.org$")
  ;; Don't show the message in the bottom of the screen
  (advice-add #'recentf-cleanup :after #'(lambda (&rest _ignored)
                                           (message nil)))
  (recentf-mode 1)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package winum
  :ensure t
  :config
  (setq winum-auto-setup-mode-line nil) ;; Do not display window number in modeline since it's already included in spaceline
  (setq winum-ignored-buffers '("*Java Dependency List*" "*LSP Error List*" "*LSP Symbols List*" " *Treemacs-Framebuffer-5*" " *Treemacs-Framebuffer-4*" " *Treemacs-Framebuffer-3*" " *Treemacs-Framebuffer-2*" " *Treemacs-Framebuffer-1*" " *LV*" " *which-key*"))
  (setq winum-ignored-buffers-regexp '("\\*Treemacs-Scoped-Buffer-"))
  (winum-mode)
  (efs-leader
   "1" '(winum-select-window-1 :wk "select window 1")
   "2" '(winum-select-window-2 :wk "select window 2")
   "3" '(winum-select-window-3 :wk "select window 3")
   "4" '(winum-select-window-4 :wk "select window 4")
   "5" '(winum-select-window-5 :wk "select window 5")
   "6" '(winum-select-window-6 :wk "select window 6")
   "7" '(winum-select-window-7 :wk "select window 7")
   "8" '(winum-select-window-8 :wk "select window 8")
   "9" '(winum-select-window-9 :wk "select window 9")
   )
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package vterm
  :preface
  (defun efs--vterm-mode-hook ()
    (hl-line-mode -1)
    (display-line-numbers-mode -1)
    (display-fill-column-indicator-mode -1)
    (auto-fill-mode -1))
  :hook
  ((vterm-mode . efs--vterm-mode-hook))
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 10000)
  (vterm-tramp-shells '(("ssh" "/bin/bash")))
  :init
  (which-key-add-key-based-replacements "C-c t" "term")
  :config
  ;; Add find-file-other-window to accepted commands
  (add-to-list 'vterm-eval-cmds
               '("find-file-other-window" find-file-other-window)))

(use-package vterm-toggle
  :bind (:map vterm-mode-map
              (("<C-return>" . vterm-toggle-insert-cd)
               ("C-M-n" . vterm-toggle-forward)
               ("C-M-p" . vterm-toggle-backward)))
  :custom
  (vterm-toggle-scope 'project)
  (vterm-toggle-project-root t)
  (vterm-toggle-fullscreen-p nil)
  :config
  ;; Show at new bottom-buffer
  (setq vterm-toggle-fullscreen-p nil)
  (add-to-list 'display-buffer-alist
               '((lambda (buffer-or-name _)
                   (let ((buffer (get-buffer buffer-or-name)))
                     (with-current-buffer buffer
                       (or (equal major-mode 'vterm-mode)
                           (string-prefix-p vterm-buffer-name (buffer-name buffer))))))
                 (display-buffer-reuse-window display-buffer-at-bottom)
                 (direction . bottom)
                 (dedicated . t) ;dedicated is supported in emacs27
                 (reusable-frames . visible)
                 (window-height . 0.3)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun efs--symbol-overlay-put ()
  "Do symbol overlay at cursor and start symbol-overlay transient state."
  (interactive)
  (symbol-overlay-put)
  (efs--symbol-overlay-transient)
  )

(use-package symbol-overlay
  :ensure t
  :after transient
  :custom-face
  (symbol-overlay-face-1 ((t (:background "#689d6a"       :foreground "black"))))
  (symbol-overlay-face-2 ((t (:background "#b08588"       :foreground "black"))))
  (symbol-overlay-face-3 ((t (:background "#7c6f64"       :foreground "black"))))
  (symbol-overlay-face-3 ((t (:background "#5c6f64"       :foreground "black"))))
  (symbol-overlay-face-4 ((t (:background "#98971a"       :foreground "black"))))
  (symbol-overlay-face-5 ((t (:background "#016286"       :foreground "black"))))
  (symbol-overlay-face-6 ((t (:background "#d79921"       :foreground "black"))))
  (symbol-overlay-face-7 ((t (:background "medium orchid" :foreground "black"))))
  (symbol-overlay-face-8 ((t (:background "#fbe107"       :foreground "black"))))
  :config
  ;; Make transient state
  (transient-define-suffix efs--so-jump-next ()
    "Next symbol"
    :description "next symbol"
    :transient t ;; Do not quit transient state
    (interactive)
    (symbol-overlay-jump-next)
    )
  (transient-define-suffix efs--so-jump-prev ()
    "Previous symbol"
    :description "previous symbol"
    :transient t ;; Do not quit transient state
    (interactive)
    (symbol-overlay-jump-prev)
    )
  (transient-define-prefix efs--symbol-overlay-transient ()
    "Symbol overlay transient state"
    ;; :transient-suffix 'transient--do-stay ;; Do not quit transient state automatically
    ["Symbol overlay transient state"
     :class transient-columns
     ["Symbol navigation"
      ("n" efs--so-jump-next)
      ("N" efs--so-jump-prev)
      ("f" symbol-overlay-switch-forward :transient t :description "switch symbol forward")
      ("F" symbol-overlay-switch-backward :transient t :description "switch symbol backwards")]
     ["All symbols"
      ("o" symbol-overlay-put :transient t :description "toggle overlay") ;; TODO: Select random face when calling this
      ("O" symbol-overlay-remove-all :transient t :description "remove all overlays")]
     ["Scope"
      ("t" symbol-overlay-toggle-in-scope :transient t :description "scope")
      ("z" recenter-top-bottom :transient t :description "recenter")]
     ["Actions"
      ("r" symbol-overlay-query-replace :transient t :description "query-replace")
      ("R" symbol-overlay-rename :transient t :description "rename")
      ("s" symbol-overlay-isearch-literally :transient t :description "isearch")
      ("q" transient-quit-all :description "quit")
      ;; Quit transient when using evil navigation keys (hidden from transient display)
      ;; FIXME: Does not override evil keys as expected
      ("h" transient-quit-all :transient nil :if-nil t)
      ("j" transient-quit-all :transient nil :if-nil t)
      ("k" transient-quit-all :transient nil :if-nil t)
      ("l" transient-quit-all :transient nil :if-nil t)]
     ])

  ;; Override symbol-overlay-map that will normally interfer with evil keys
  (let ((map (make-sparse-keymap)))
    (setq symbol-overlay-map map))

  ;; Global keys
  (efs-leader
   "s o" '(efs--symbol-overlay-put :wk "toggle symbol overlay")
   "s O" '(symbol-overlay-remove-all :wk "remove symbol overlays")
   "s M-o" '(efs--symbol-overlay-transient :wk "symbol overlay transient")
   "t o" '(symbol-overlay-mode :wk "symbol overlay mode")
   )
  )

(use-package transient
  :ensure t
  :config
  ;; ;; NOTE: Example how to set up transient:
  ;; (transient-define-suffix pmx-show-prefix ()
  ;;     "Show the prefix that invoked this suffix"
  ;;     :description "prefix"
  ;;     (interactive)
  ;;     (message "Current prefix key: %s" transient-current-prefix))

  ;;   (transient-define-suffix pmx-show-command ()
  ;;     "Show this command"
  ;;     :description "current command"
  ;;     (interactive)
  ;;     (message "Current command: %s" transient-current-command))

  ;;   (transient-define-suffix pmx-show-suffixes ()
  ;;     "Show the current suffixes"
  ;;     :description "suffixes"
  ;;     (interactive)
  ;;     (message "Current suffixes: %s" (cl-mapcar
  ;;                                      (lambda (obj)
  ;;                                        (oref obj description))
  ;;                                      transient-current-suffixes)))

  ;;   (transient-define-suffix pmx-show-args ()
  ;;     "Show current infix args"
  ;;     :description "infix args"
  ;;     (interactive)
  ;;     (message "Current infix args: %s" (transient-args transient-current-command)))

  ;;   (transient-define-suffix pmx-send-message ()
  ;;     "Send message to minibuffer"
  ;;     :description "send message"
  ;;     :transient t
  ;;     (interactive)
  ;;     (message "Message sent at %s. Happy?" (shell-command-to-string "echo -n $(date)")))

  ;;   (transient-define-argument pmx-affirmative ()
  ;;     "Are we affirmative?"
  ;;     :description "affirmative"
  ;;     :argument "affirmative")

  ;;   (transient-define-argument pmx-yep-nope ()
  ;;     "Is it yep or is it nope?"
  ;;     :description "yep or nope"
  ;;     :class 'transient-option
  ;;     :shortarg "-y"
  ;;     :argument "--yepnope="
  ;;     :choices '("yep" "nope"))

  ;;   (transient-define-argument pmx-abc ()
  ;;     "Which letters do you like?"
  ;;     :description "abc"
  ;;     :class 'transient-option
  ;;     :shortarg "-a"
  ;;     :argument "--abc="
  ;;     :choices '("A" "B" "C"))

  ;;   (defvar pmx--variable "A string" "A variable brought to you by pmx")

  ;;   (transient-define-argument pmx-set-lisp-variable ()
  ;;     "Set a lisp variable, pmx--variable.  Won't show up in infix arguments."
  ;;     :description "set pmx--variable"
  ;;     :class 'transient-lisp-variable
  ;;     :shortarg "-l"
  ;;     :variable 'pmx--variable
  ;;     :argument "--letters=")

  ;;   (transient-define-suffix pmx-show-lisp-variable ()
  ;;     "Access pmx--variable"
  ;;     :description "show pmx--variable"
  ;;     (interactive)
  ;;     (message "Current value of pmx--variable: %s" pmx--variable))

  ;;   (transient-define-suffix pmx-dynamic-suffix ()
  ;;     "Description depends on pmx--variable"
  ;;     :if-not '(lambda () (string-equal pmx--variable "abc"))
  ;;     :description '(lambda () (format "pmx %s" pmx--variable))
  ;;     (interactive)
  ;;     (message "Current value of pmx--variable: %s" pmx--variable))

  ;;   (transient-define-prefix pmx-nested-transient ()
  ;;     "Some subcommands, like tree menus from the land of mice"
  ;;     ["Switches"
  ;;      ("-s" "another switch" ("-x" "--conflicting"))]
  ;;     ["Sub Command Introspection"
  ;;      ("i" pmx-show-args)
  ;;      ("p" pmx-show-prefix)
  ;;      ("s" pmx-show-suffixes)
  ;;      ("c" pmx-show-command)]
  ;;     ["Dynamic Commands"
  ;;      ("d" pmx-dynamic-suffix)])

  ;;   (transient-define-prefix pmx-transient-toy ()
  ;;     "Figure out how to use transient's API properly"
  ;;     [:class transient-columns
  ;;      ["Things"
  ;;       ("-w" "switch"  ("-w" "--switch"))]
  ;;      ["Others"
  ;;       ("i" pmx-show-args)
  ;;       ("p" pmx-show-prefix)
  ;;       ("s" pmx-show-suffixes)
  ;;       ("c" pmx-show-command)
  ;;       ("m" pmx-send-message)]
  ;;      ["More"
  ;;       ("f" pmx-affirmative)
  ;;       ("y" pmx-yep-nope)
  ;;       ("a" pmx-abc)
  ;;       ("l" pmx-set-lisp-variable)
  ;;       ("w" pmx-show-lisp-variable)]
  ;;      ["Drilldown"
  ;;       ("d" "drilldown" pmx-nested-transient)]])

  ;;   (global-set-key (kbd "M-o") 'pmx-transient-toy)

  ;; Transient helper for elpaca
  (transient-define-prefix efs--transient-elpaca-helper ()
    "elpaca transient state"
    ["Elpaca helper"
     :class transient-columns
     ["Info"
      ("i" elpaca-info        :transient nil :description "info")
      ("l" elpaca-log         :transient t   :description "log")
      ("b" elpaca-browse      :transient nil :description "browse")]
     ["Updates"
      ("m" elpaca-manager     :transient nil :description "manager")
      ("u" elpaca-merge-all   :transient t   :description "merge all")
      ("r" elpaca-menu-item   :transient nil :description "menu item")
      ("f" elpaca-fetch-all   :transient t   :description "fetch all")]
     ["Others"
      ("q" transient-quit-all                :description "quit")]
     ])
  (defun efs--elpaca-helper ()
    "Start elpaca-helper transient"
    (interactive)
    (efs--transient-elpaca-helper))
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package treemacs
  :ensure t
  :config
  (setq treemacs-git-mode 'simple)
  (setq treemacs-hide-gitignored-files-mode nil)
  (treemacs-fringe-indicator-mode 'only-when-focused)
  ;; Conditional git mode setup
  ;;  - If both are found: enables 'deferred' git mode (more advanced git integration)
  ;;  - If only git is found: falls back to 'simple' git mode
  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple)))
  ;; Keybindings
  (efs-leader
   "0" '(treemacs-select-window :wk "treemacs window")
   "f t" '(treemacs :wk "treemacs")
   )
  )

(use-package treemacs-all-the-icons
  :ensure t
  :after treemacs
  )

(use-package treemacs-projectile
  :ensure t
  :after (treemacs projectile))

(use-package treemacs-magit
  :ensure t
  :after (treemacs magit))

(use-package treemacs-persp
  :ensure t
  :after (treemacs persp-mode)
  :config (treemacs-set-scope-type 'Perspectives))

(use-package treemacs-evil
  :ensure t
  :after (treemacs evil)
  ;; :config
  ;; (setq evil-treemacs-state-cursor '(bar . 2))
  )

(provide 'efs-window)
;;; efs-window.el ends here
