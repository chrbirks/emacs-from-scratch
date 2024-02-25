;; -*- lexical-binding: t -*-

;; Package management
;; elpaca
(defvar elpaca-installer-version 0.6)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (call-process "git" nil buffer t "clone"
                                       (plist-get order :repo) repo)))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq use-package-always-ensure t))

;; Hack for transient requiring seq >= 2.24
(defun +elpaca-unload-seq (e) "Unload seq before continuing the elpaca build, then continue to build the recipe E."
  (and (featurep 'seq) (unload-feature 'seq t))
  (elpaca--continue-build e))
(elpaca `(seq :build ,(append (butlast (if (file-exists-p (expand-file-name "seq" elpaca-builds-directory))
                                          elpaca--pre-built-steps
                                        elpaca-build-steps))
                             (list '+elpaca-unload-seq 'elpaca--activate-package))))

;; Block until current queue processed.
(elpaca-wait)

;; Install packages like this:
;; (use-package evil :demand t)
;; Expands to: (elpaca evil (use-package evil :demand t))

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

   "b" '(:ignore t :wk "buffers")
   "b p" '(switch-to-prev-buffer :wk "previous buffer")
   "b n" '(switch-to-next-buffer :wk "next buffer")
   "b d" 'kill-this-buffer

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

(use-package diminish
  :ensure t
  :config
  ;; Diminish some common minor modes
  (diminish 'visual-line-mode)
  (diminish 'auto-revert-mode)
  (diminish 'eldoc-mode)
  (diminish 'undo-tree-mode)
  (diminish 'visual-line-mode)
  (diminish 'buffer-face-mode)
  (diminish 'hs-minor-mode)
)

(use-package delight)

(elpaca-wait)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq
 ;; Don't blink cursor
 blink-cursor-mode nil
 ;; No need to remind me what a scratch buffer is.
 initial-scratch-message nil
 ;; Never ding at me, ever.
 ring-bell-function 'ignore
 ;; Prompts should go in the minibuffer, not in a GUI.
 use-dialog-box nil
 ;; Fix undo in commands affecting the mark.
 mark-even-if-inactive nil
 ;; Disable mouse acceleration
 mouse-wheel-progressive-speed nil
 tab-width 2
 )

(set-charset-priority 'unicode)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(setq default-process-coding-system '(utf-8-unix . utf-8-unix))

;; Fix eln-cache path to work with native comp. Compiler otherwise stores output files in Chemacs config folder
(when (> emacs-major-version 28)
  (add-to-list 'native-comp-eln-load-path (expand-file-name "eln-cache/" user-emacs-directory)))

;; Set `gc-cons-threshold' to a high value during startup,
;; and restore it to a more reasonable default value after startup.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000))))

;; Increase `gc-cons-threshold' when Emacs is idle,
;; and decrease it back to its original value when Emacs becomes busy.
(defvar my-normal-gc-cons-threshold (* 2 1000 1000))
(defvar my-idle-gc-cons-threshold (* 100 1000 1000))
(defun my-gc-cons-threshold ()
  (if (not (eq this-command 'minibuffer-complete))
      (setq gc-cons-threshold
            (if (or (minibufferp) current-prefix-arg)
                my-normal-gc-cons-threshold
              my-idle-gc-cons-threshold))))
(add-hook 'pre-command-hook #'my-gc-cons-threshold)

;; Prevent warning buffer from stealing focus on every new warning
(setq warning-minimum-level :warning)

(setq initial-scratch-message nil)

(setq-default
   debug-on-error nil
   ;; Prevent warning buffer from stealing focus on every new async compilation warning. Set to nil to supress them entirely.
   native-comp-async-report-warnings-errors 'silent
   ;; Do not wrap lines
   truncate-lines t
   ;; Disable highlight line mode
   global-hl-line-mode t
   ;; Disable tildes in fringe
   global-vi-tilde-fringe-mode nil
   ;; Increase max number of flycheck errors
   flycheck-checker-error-threshold 1000
   ;; Compress files when access them via TRAMP
   tramp-inline-compress-start-size 1024
   ;; Enable indentation+completion using the TAB key. `completion-at-point' is often bound to M-TAB.
   tab-always-indent 'complete
   ;; Never insert tabs
   indent-tabs-mode nil)

;; Set row/column window size at startup
(if (window-system) (set-frame-size (selected-frame) 140 60))

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))

(setq inhibit-startup-message t)

(scroll-bar-mode -1) ; Disable visible scrollbar
(tool-bar-mode -1)   ; Disable the toolbar
(tooltip-mode -1)    ; Disable tooltips
(set-fringe-mode 5)  ; Give some breathing room
(menu-bar-mode -1)   ; Disable the menu bar
(column-number-mode)
(global-display-line-numbers-mode t)

;; Disable line numbers for some modes
(dolist (mode '(term-mode-hook
                shell-mode-hook
                treemacs-mode-hook
                eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

;; Fonts
;; You will most likely need to adjust this font size for your system!
(defvar efs--default-font-size 100)
(defvar efs--default-variable-font-size 100)

(set-face-attribute 'default nil :font "MesloLGS Nerd Font Mono" :height efs--default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "MesloLGS Nerd Font Mono" :height efs--default-font-size)

;; Set the variable pitch face
(set-face-attribute 'variable-pitch nil :font "Fira Code Nerd Font" :height efs--default-variable-font-size :weight 'regular)

;; Helper functions for snippets templates
;; Format:          \sum_{}^{}
;; Snippet example: \sum_{$1}^{$2} ${3:$$(yas-delete-if-empty)}
(defun yas-delete-if-empty ()
  (save-excursion
    (when (re-search-backward "\\\\sum\\(_{}\\)^{.+}" (line-beginning-position) t)
      (replace-match "" t t nil 1))))

;; Window configuration
(setq switch-to-buffer-obey-display-actions t)
(setq switch-to-buffer-in-dedicated-window 'pop) ; Opening buffer in dedicated window causes it to pop up somewhere else instead of an error

;; Snippets settings
(use-package yasnippet
  :init
  ; Add custom snippets dir
  (setq yas-snippet-dirs '("~/.config/emacs-from-scratch/snippets" "~/etc/spacemacs.d/private/snippets/" "~/etc/spacemacs.d/layers/+completion/auto-completion/local/snippets" yasnippet-snippets-dir))
  :defer t)

(use-package yasnippet-snippets
  :defer t
  :after yasnippet)

;; Any .do .qsf .qpf and .sdc file should be in tcl-mode
(add-to-list 'auto-mode-alist '("\\.do\\'" . tcl-mode))
(add-to-list 'auto-mode-alist '("\\.sdc\\'" . tcl-mode))
(add-to-list 'auto-mode-alist '("\\.qpf\\'" . tcl-mode))
(add-to-list 'auto-mode-alist '("\\.qsf\\'" . tcl-mode))

(use-package no-littering
  :ensure t
  :defer nil)

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
  (evil-set-undo-system 'undo-tree)
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
  (evil-org-mode . (lambda () (evil-org-set-key-theme '(textobjects insert navigation additional shift todo heading)))) ;; Enable all key bindings
  :config
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

(use-package undo-tree
  :ensure t
  :diminish undo-tree-mode
  :config
  (global-undo-tree-mode)
  (setq undo-tree-visualizer-diff t) ;; Show diff in separate buffer by default
  (efs-leader
    "a u" '(undo-tree-visualize :wk "undo-tree-visualize"))
  ;; Redefine evil-mode keys while in undo-tree-mode
  (evil-make-overriding-map undo-tree-visualizer-mode-map 'motion)
  (evil-define-key 'motion undo-tree-visualizer-mode-map
    "h" 'undo-tree-visualize-switch-branch-left
    "j" 'undo-tree-visualize-redo
    "k" 'undo-tree-visualize-undo
    "l" 'undo-tree-visualize-switch-branch-right
    "d" 'undo-tree-visualizer-toggle-diff)
  ;; Do not save undo-tree files named .~undo-tree~ everywhere.
  (setq undo-tree-auto-save-history nil)
  ;; Or make place the files in /.emacs.d/undo instead
  ;; (setq undo-tree-history-directory-alist '(("." . "/.emacs.d/undo")))
  )

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

(use-package spacemacs-theme
  :defer t
  :init
  (load-theme 'spacemacs-dark t)
  :config
  ;; Do not use a different background color for comments.
  (setq spacemacs-theme-comment-bg nil)
  ;; Comments should appear in italics.
  (setq spacemacs-theme-comment-italic t))

(use-package indent-guide
  :diminish indent-guide-mode
  :config
  (setq indent-guide-char "▒")
  (efs-leader
   "t i" '(indent-guide-mode :wk "indent-guide-mode")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spaceline config

(use-package spaceline
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme) ; Set the theme
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state) ; Color according to evil state
  (setq powerline-default-separator 'arrow)
  (setq spaceline-workspace-numbers-unicode t) ; Get unicode numbers when using window-numbering-mode
  (setq spaceline-window-numbers-unicode t) ; Get unicode numbers when using eyebrowse-mode
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Function for maximizing/unmaximizing the active window.
;; Could maybe be implemented safer with '(window-configuration-to-register :efs-window-conf-var)' instead.

(defvar efs--toggle-maximized-buffer-state (make-hash-table :test #'equal)
  "Hash-table to store state of the maximization status of the buffer. Unique for each perspective. Maximized if non-nill.")

(defvar efs--toggle-maximized-buffer-prev-config (make-hash-table :test #'equal)
  "Hash-table to store the previous window configuration before maximizing. Unique for each perspective.")

(defun efs--toggle-maximize-buffer ()
  "Toggle the maximization of the current buffer. Plays nice with a treemacs buffer."
  (interactive)
  (unless (bound-and-true-p winner-mode)
    (winner-mode 1))
  (if (gethash persp-last-persp-name efs--toggle-maximized-buffer-state)
      ;; If in maximized state
      (progn
        (when (gethash persp-last-persp-name efs--toggle-maximized-buffer-prev-config)
          (set-window-configuration (gethash persp-last-persp-name efs--toggle-maximized-buffer-prev-config)) ;; Restore windows config
          (puthash persp-last-persp-name nil efs--toggle-maximized-buffer-state)))
    ;; If not in maximized state
    (progn
      ;; Reset the no-delete-other-windows parameter for all windows since buffers such as lsp-treemacs-errors-list buffers somehow can prevent other windows from being maximized
      (dolist (win (window-list))
        (set-window-parameter win 'no-delete-other-windows nil))
      (puthash persp-last-persp-name (current-window-configuration) efs--toggle-maximized-buffer-prev-config) ;; Save window config
      (delete-other-windows)
      (puthash persp-last-persp-name t efs--toggle-maximized-buffer-state))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package wgrep)

(use-package deadgrep
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
;; Vertico completion framework
(use-package vertico
  :init (vertico-mode)
  :config
  (setq vertico-count 15)
  (setq verico-resize t)
  ;; Prefix the current vertico candidate with “» ”. From https://github.com/minad/vertico/wiki#prefix-current-candidate-with-arrow
  (advice-add #'vertico--format-candidate :around
              (lambda (orig cand prefix suffix index _start)
                (setq cand (funcall orig cand prefix suffix index _start))
                (concat
                 (if (= vertico--index index)
                     (propertize "» " 'face 'vertico-current)
                   "  ")
                 cand)))
  ;; Do not allow the cursor in the minibuffer prompt
  (setq minibuffer-prompt-properties
        '(read-only t cursor-intangible t face minibuffer-prompt))
  (add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
  ;; Global keybindings
  (efs-leader
   "f f" '(find-file :wk "find file")
   "f A" '(find-alternate-file :wk "replace buffer with file")
   )
  ;; :hook (rfn-eshadow-update-overlay . vertico-directory-tidy) ; Correct file path when using a command for selecting a file
  )


;; Orderless back-end for minibuffer comletion ordering and sorting
(use-package orderless
  :after vertico
  :custom
  ;; (completion-styles '(orderless basic))
  (completion-styles '(basic orderless))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles basic partial-completion))))
  )

;; Marginalia for rich annotation in minibuffers
(use-package marginalia
  :after general
  :custom
  (marginalia-align 'right) ;; Alight text, e.g. if file path is too long to show in the minibuffer,
                            ;; prioritize to show the right-most side with the filename
  (marginalia-field-width 320)
  :init
  (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("C-<f1>" . 'marginalia-cycle) ; Cycle through marginalia modes while in minibuffer
              )
  )

;; Embark for executing commands on selected completion items (killing buffer, etc.)
(use-package embark
  :commands (embark-collect embark-collect-mark embark-collect-toggle-marks embark-collect-unmark embark-collect-toggle-view)
  :init
  ;; Replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :bind
  ("C-h B" . embark-bindings)
  ("C-c c" . embark-act) ;; "C-h" after embark-act to get help for embark-act commands
  ("C-c C" . embark-dwim)
  (:map minibuffer-local-map
        (("M-M" . embark-collect-toggle-marks)
         ("M-E" . embark-export)   ;; Export all minibuffer result to new buffer depending on type.
                                   ;; Run occur-edit-mode or wgrep-change-to-wgrep-mode (depending on what was in the minibuffer) with "i".
                                   ;; Run "C-c C-C" to finish occur mode, and "Z Z" to finish wgrep mode.
         ("M-C" . embark-collect)) ;; "m": mark, "u": unmark, "t": mark all.
        ))

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
)

;; Consult for enhanced completion commands
(use-package consult
  :ensure t
  :init
  ;; Configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  :config
  ;; Select which buffers to mark as hidden buffers
  (setq consult-buffer-filter
        '("\\` "
          "\\`\\*Completions\\*\\'"
          "\\`\\*Flymake log\\*\\'"
          "\\`\\*Semantic SymRef\\*\\'"
          "\\`\\*tramp/.*\\*\\'"
          "\\`\\*Async-native-compile-log\\*\\'"
          "\\`magit.*\\'"))

  ;; Where to place cursor after selecting a search match
  (setq consult-point-placement 'match-beginning)
  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))

  ;; Other setup
  ;; vertico-grid-annotate, vertico-flat-annotate
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   :preview-key 'any)

  ;; Configure the narrowing key.
  (setq consult-narrow-key "C-<") ;; "C-+"

  ;; Make narrowing help available in the minibuffer.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "h") #'consult-narrow-help)
  (define-key consult-narrow-map (kbd "M-h") #'consult-narrow-help)

  ;; By default `consult-project-function' uses `project-root' from project.el.
  ;; Optionally configure a different project root function.
  ;;;; 1. project.el (the default)
  ;; (setq consult-project-function #'consult--default-project--function)
  ;;;; 2. vc.el (vc-root-dir)
  ;; (setq consult-project-function (lambda (_) (vc-root-dir)))
  ;;;; 3. locate-dominating-file
  ;; (setq consult-project-function (lambda (_) (locate-dominating-file "." ".git")))
  ;;;; 4. projectile.el (projectile-project-root)
  (autoload 'projectile-project-root "projectile")
  (setq consult-project-function (lambda (_) (projectile-project-root)))
  ;;;; 5. No project support
  ;; (setq consult-project-function nil)

  (defun efs--consult-line-symbol-at-point ()
    "Call `consult-line' with the symbol under cursor as the initial search argument."
    (interactive)
    (let ((initial (thing-at-point 'symbol)))
      (consult-line initial)))

  ;; Global keybindings
  (efs-leader
   "b b" '(consult-buffer :wk "switch buffer") ;; Call consult-narrow-key to see narrowing options
   "b B" '(persp-switch-to-buffer :wk "switch persp buffer")
   "b P" '(consult-project-buffer :wk "switch project buffer")

   "f r" '(consult-recent-file :wk "recent files")
   "r y" '(consult-yank-from-kill-ring :wk "yank kill-ring")
   "r s" '(consult-register-store :wk "store register")
   "r r" '(consult-register :wk "select register")

   "s s" '(consult-line :wk "seach buffer")
   "s S" '(efs--consult-line-symbol-at-point :wk "search buffer at point")
   "s b" '(consult-line-multi :wk "seach all buffers")
   "s p" '(consult-ripgrep :wk "search project")
   "s r" '(rg-menu :wk "ripgrep-menu")
   "s d" '(deadgrep :wk "deadgrep")

   "p b" '(consult-project-buffer :wk "project buffers")
   "p i" '(consult-imenu :wk "project imenu")
   "p I" '(consult-imenu-multi :wk "project imenu-multi")

   "t m" '(consult-minor-mode-menu :wk "enable/disable minor-mode")
   )
  )

(use-package corfu
  :ensure t
  :after orderless
  :bind
  (:map corfu-map
        ("RET" . #'newline) ;; Prevent enter from completing candidates
        ("C-n"     . corfu-next)
        ("C-p"     . corfu-previous)
        ("C-c C-g" . #'corfu-quit)
        ("C-c C-d" . #'corfu-info-documentation) ;; Requires the corfu-info.el extension. See below use-package.
        ("C-c C-l" . #'corfu-info-location) ;; Requires the corfu-info.el extension. See below use-package.
        )
  :custom
  (corfu-auto t)        ; Don't only use `corfu' when calling `completion-at-point' or `indent-for-tab-command'
  (corfu-scroll-margin 4)
  (corfu-cycle nil)
  (corfu-separator ?\s) ;; Orderless field separator
  (corfu-quit-no-match t) ;; Quit if no match
  (corfu-quit-at-boundary nil) ;; Never quit at completion boundary

  ;; Set this in the `corfu' use-package to be
  ;; extra-safe that this is set when corfu-doc is loaded. I do not want
  ;; documentation shown in both the echo area and in the `corfu-doc' popup.
  ;; (corfu-echo-documentation nil)
  :init
  (global-corfu-mode)
  ;; (require 'corfu-info)
  :config
  ;; Load corfu extensions
  (with-eval-after-load 'corfu
    (add-to-list 'load-path "~/.config/emacs-from-scratch/elpaca/repos/corfu/extensions/")
    ;; corfu-info: Get the corfu-info-documentation and corfu-info-location functions
    (require 'corfu-info)
    ;; corfu-history: Sort candidates by their history position
    (require 'corfu-history)
    (corfu-history-mode 1)
    (savehist-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)
    ;; corfu-popupinfo
    (require 'corfu-popupinfo)
    (corfu-popupinfo-mode 1)
    (setq corfu-popupinfo-delay '(1.0 . 0.2))
    (setq corfu-popupinfo-max-height 30)
    )
)

;; Cape - Completion At Point Extensions
(defun efs--cape-capf-setup-verilog ()
  "Create completions backends for verilog-mode"
  ;; (let ((result))
  ;;   (dolist (element '(verilog-completion-at-point cape-symbol cape-dabbrev) result)
  ;;     (add-to-list 'completion-at-point-functions element))))
  (let (result)
    (dolist (element (list
                      (cape-super-capf #'verilog-completion-at-point #'cape-symbol #'cape-dabbrev))
                     result)
      (add-to-list 'completion-at-point-functions element))))

;; (defun efs--cape-capf-setup-lsp ()
;;   "Replace the default `lsp-completion-at-point' with its
;; `cape-capf-buster' version. Also add `cape-file' and
;; `company-yasnippet' backends."
;;   (setf (elt (cl-member 'lsp-completion-at-point completion-at-point-functions) 0)
;;         (cape-capf-buster #'lsp-completion-at-point))
;;   (add-to-list 'completion-at-point-functions (cape-company-to-capf #'company-yasnippet))
;;   (add-to-list 'completion-at-point-functions #'cape-dabbrev t))

(use-package cape
  ;; Bind dedicated completion commands
  :bind (("C-c o p" . completion-at-point) ;; capf
         ("C-c o t" . complete-tag)        ;; etags
         ("C-c o d" . cape-dabbrev)        ;; or dabbrev-completion
         ("C-c o h" . cape-history)
         ("C-c o f" . cape-file)
         ("C-c o k" . cape-keyword)
         ("C-c o s" . cape-symbol)
         ("C-c o a" . cape-abbrev)
         ("C-c o l" . cape-line)
         ("C-c o w" . cape-dict)
         ("C-c o \\" . cape-tex)
         ("C-c o _" . cape-tex)
         ("C-c o ^" . cape-tex)
         ("C-c o &" . cape-sgml)
         ("C-c o r" . cape-rfc1345))
  :init
  ;; Add `completion-at-point-functions', used by `completion-at-point'.
  ;; NOTE: The order matters!
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
  :hook
  (verilog-mode . efs--cape-capf-setup-verilog)
)

;; Icons for corfu completion buffer
(use-package nerd-icons-corfu
  :ensure t
  :after corfu
  :elpaca (:host github :repo "LuigiPiucco/nerd-icons-corfu")
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  )

;; (use-package corfu-popupinfo
;;   :ensure t
;;   :after corfu
;;   ;; :hook (corfu-mode . corfu-doc-mode)
;; ;;  :general (:keymaps 'corfu-map
;; ;;            ;; This is a manual toggle for the documentation popup.
;; ;;            [remap corfu-show-documentation] #'corfu-doc-toggle ; Remap the default doc command
;; ;;            ;; Scroll in the documentation window
;; ;;            "M-n" #'corfu-doc-scroll-up
;; ;;            "M-p" #'corfu-doc-scroll-down)
;;   :config
;;   (setq corfu-popupinfo-delay 0.5)
;;   ;; (corfu-doc-max-width 70)
;;   ;; (corfu-doc-max-height 20)
;;   )

;; (use-package corfu-history
;;   :ensure t
;;   :after corfu
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package all-the-icons
  :ensure t)

;; Get file and buffer icons in minibuffer
(use-package all-the-icons-completion
  :ensure t
  :after (vertico consult marginalia all-the-icons)
  :hook (marginalia-mode . all-the-icons-completion-marginalia-setup)
  :init
  (all-the-icons-completion-mode)
  )


;; (use-package helpful
;;   :commands (helpful-callable helpful-variable helpful-command helpful-key)
;;   :custom
;;   (counsel-describe-function-function #'helpful-callable)
;;   (counsel-describe-variable-function #'helpful-variable)
;;   :bind
;;   ([remap describe-function] . counsel-describe-function)
;;   ([remap describe-command] . helpful-command)
;;   ([remap describe-variable] . counsel-describe-variable)
;;   ([remap describe-key] . helpful-key))

;; (use-package helpful
;;   :commands (helpful-callable helpful-variable helpful-command helpful-key)
;;   :bind
;;   ([remap describe-command] . helpful-command)
;;   ([remap describe-key] . helpful-key))

(use-package helpful
  :commands (helpful-callable helpful-variable helpful-command helpful-key)
  :bind
  ("C-h f" . helpful-function)
  ([remap describe-function] . helpful-command)
  ([remap describe-macro] . helpful-macro)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))


(defun efs--org-mode-setup ()
  (org-indent-mode 0)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :defer nil
  ;; :elpaca nil ;; FIXME 11-10-2023: The native org package is used since the newest from MELPA(?) has compile errors for Emacs 27.1
  :ensure t
  :commands (org-capture org-agenda org-roam-capture deadgrep-org) ;; Make available before org is loaded
  :hook (org-mode . efs--org-mode-setup)
  :config
  (setq org-log-into-drawer '("LOOGBOOK")
        org-directory "~/org/"
        org-default-notes-file "~/org/notes.org" ;; Default file for templates that does not specify a file
        ;; Org Projectile
        org-projectile-file "TODOs.org"
        org-project-capture-projects-file "~/org/projects/misc-TODOs.org"
        org-project-capture-per-project-filepath "~/org/projects/TODOs-%s.org" ;; Per-project org-projectile files if org-projectile-projects-directory is nil ;; FIXME: 28-12-22: Cannot get it to work
        ;; Org Roam
        org-roam-directory "~/org/roam/"
        org-roam-dailies-directory "daily/" ;; Relative to org-roam-directory
        org-roam-db-location "~/.config/emacs-from-scratch/org-roam.db"
        ;; Org Agenda
        org-agenda-files '("~/org/tasks.org"
                           "~/org/notes.org"
                           "~/org/projects/TODOs.org"
                           "~/org/projects/weibel/TODOs.org"
                           "~/org/projects/misc-TODOs.org"
                           )
        )
  ;; Set face for done checkbox items to grey strike-through
  (defface org-checkbox-done-text
    '((t (:foreground "#71696A" :strike-through t)))
    "Face for the text part of a checked org-mode checkbox.")
  (font-lock-add-keywords
   'org-mode
   `(("^[ \t]*\\(?:[-+*]\\|[0-9]+[).]\\)[ \t]+\\(\\(?:\\[@\\(?:start:\\)?[0-9]+\\][ \t]*\\)?\\[\\(?:X\\|\\([0-9]+\\)/\\2\\)\\][^\n]*\n\\)"
      1 'org-checkbox-done-text prepend))
   'append)
  ;; Set the TODo item states and customize their face
  (setq org-todo-keywords
        '(;; Sequence for TASKS
          (sequence "TODO(t@/!)" "DOING(o@/!)" "WAITING(w@/!)" "|" "CANCELED(x@/!)" "DONE(d@/!)")
          ;; ;; Sequence for EVENTS
          ;; (sequence "VISIT(v@/!)" "|" "DIDNOTGO(z@/!)" "MEETING(m@/!)" "VISITED(y@/!)")
          ;; ;; Sequence for tasks for time-tracking. Add note with time when entering the state (@) and record only time when leaving the state (!)
          ;; (sequence "BACKLOG(b@/!)" "POSTPONED(p@/!)" "WAITING(w@/!)" "DOING(d@/!)" "|" "REVIEW(r@/!)" "DONE(c@/!)")
          ))
  ;; Define function deadgrep-org
  (defun deadgrep-org ()
    "Search files in org-directory"
    (interactive)
    (let ((default-directory "~/org/"))
      (call-interactively #'deadgrep)))
  ;; Define Org (not roam) capture templates
  (setq org-capture-templates
        '(("t" "TODO" entry
           (file "~/org/projects/TODOs.org")
           (file "~/.config/emacs-from-scratch/org-templates/todo.org")
           :empty-lines-before 1
           :unnarrowed nil)
          ("w" "Weibel TODO" entry
           (file "~/org/projects/weibel/TODOs.org")
           (file "~/.config/emacs-from-scratch/org-templates/weibel-todo.org")
           :empty-lines-before 1
           :unnarrowed nil)))
  (efs-leader
   "a o" '(:ignore t :wk "org")
   "a o r" '(:ignore t :wk "roam")
   "a o r f" '(org-roam-node-find :wk "node-find")
   "a o r c" '(org-roam-capture :wk "capture")
   "a o r d" '(:ignore t :wk "dailies")
   "a o r d t" '(org-roam-dailies-goto-today :wk "goto today")
   "a o r d d" '(org-roam-dailies-goto-date :wk "goto date")
   "o S A" '(org-archive-subtree-default :wk "archive subtree")
   "o S S" '(org-sort :wk "org sort")
   "o T T" '(org-todo :wk "org todo")
   "o T t" '(org-show-todo-tree :wk "org todo tree")
   ))

(use-package org-roam-ui
  :ensure t
  :after org
  :config
  (efs-leader
   "a o r u" '(org-roam-ui-open :wk "org-roam-ui-open")))

(unless (version<= emacs-version "28") ;; FIXME 11-10-2023: Don't use org-projectile on Emacs 27.1 as it doesn't compile
  (use-package org-projectile
    :after org
    :ensure t
    :config
    ;; Add org-projectile files to agenda view
    ;; Filter the list to only projects that exists
    (defun org-modern-config/org-projectile-todo-files ()
      "Fetch a list of org TO DO files for projects that actually exist."
      (seq-filter #'file-exists-p (org-projectile-todo-files)))
    (setq org-agenda-files
          (append org-agenda-files (org-modern-config/org-projectile-todo-files)))

    ;; Override org-projectile-get-project-todo-file to store project-specific TODOs in ~/org/projects/<project>/TODOs.org
    (defun org-projectile-get-project-todo-file (project-path)
      (concat "~/org/projects/" (file-name-nondirectory (directory-file-name project-path)) "/TODOs.org"))
    ))

;; For replacing keywords such as TODOs etc. with svg images
(use-package svg-tag-mode
  :ensure t
  :defer t)

(use-package org-modern
  :ensure t
  :after org
  :config
  (setq org-auto-align-tags nil
        org-tags-column 0
        org-catch-invisible-edits 'show-and-error
        org-special-ctrl-a/e t
        org-insert-heading-respect-content t
        org-export-backends '(ascii html icalendar latex md odt confluence)
        ;; Org styling, hide markup, etc.
        org-hide-emphasis-markers t
        org-pretty-entities t
        org-ellipsis "…" ; " ⛛"
        org-use-sub-superscripts nil ; Disable underscore-to-subscript beautifying
        ;; Agenda styling
        org-agenda-block-separator ?─
        org-agenda-start-with-log-mode t
        org-agenda-time-grid
        '((daily today require-timed)
          (800 1000 1200 1400 1600 1800 2000)
          " ┄┄┄┄┄ " "┄┄┄┄┄┄┄┄┄┄┄┄┄┄┄")
        org-agenda-current-time-string
        "⭠ now ─────────────────────────────────────────────────")
  ;; Add frame borders and window dividers
  (dolist (face '(window-divider
                  window-divider-first-pixel
                  window-divider-last-pixel))
    (face-spec-reset-face face)
    (set-face-foreground face (face-attribute 'default :background)))
  (set-face-background 'fringe (face-attribute 'default :background))

  ;; Set faces for TOD0 keywords
  ;; TODO: Is this still used? If so, move .config/emacs/packages/ to .config/emacs-from-scratch/
  (if nil
      ;; If true
      (progn ;; If true
        (use-package svg-tag-mode-config :load-path "~/.config/emacs/packages/")
        (add-hook 'org-mode-hook 'svg-tag-mode))
    ;; If false/nil
    (setq org-modern-todo-faces
          '(
            ("TODO"     :foreground "#b7742f" :background "#292b2e" :weight bold)
            ("DOING"    :foreground "yellow"  :background "#292b2e" :weight bold)
            ("WAITING"  :foreground "#00c1ff" :background "#292b2e" :weight normal)
            ("CANCELED" :foreground "#686868" :background "#292b2e" :weight bold)
            ("DONE"     :foreground "#686868" :background "#292b2e" :weight bold)
            ))
    )
  ;; Enable org-modern globally
  (global-org-modern-mode)
  (add-hook 'org-mode-hook 'org-modern-mode)
  (add-hook 'org-agenda-finilize-hook #'org-modern-agenda))

(use-package org-roam
  :after org
  :commands (org-roam-node-find org-roam-dailies-goto-today org-roam-dailies-goto-date)
  :config
  (org-roam-db-autosync-mode t) ;; Keep org-roam session automatically synchronized
  ;; Define Org-roam capture templates
  (setq org-roam-capture-templates
        '(
          ;; Default template
          ("d" "Default" plain
           (file "~/.config/emacs-from-scratch/org-roam-templates/default.org")
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "# -*- mode: org; eval: (setq-local org-download-image-dir \"images\" org-download-heading-lvl nil) -*-
#+title: ${title}
")
           :unnarrowed t)
          ;; Ølbrygning template
          ("o" "Ølbrygning" plain
           (file "~/.config/emacs-from-scratch/org-roam-templates/ølbrygning.org")
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "# -*- mode: org; eval: (setq-local org-download-image-dir \"images\" org-download-heading-lvl nil) -*-
#+title: ${title}
#+filetags: :ølbrygning:
#+date: %^t

")
           :unnarrowed t)
          ;; Weibel template
          ("w" "Weibel" plain
           (file "~/.config/emacs-from-scratch/org-roam-templates/weibel.org")
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "# -*- mode: org; eval: (setq-local org-download-image-dir \"images\" org-download-heading-lvl nil) -*-
#+title: ${title}
#+filetags: :weibel:
#+date: %^t

")
           :unnarrowed t)
          )
        )
  )

(use-package tree-sitter
  :ensure t
  :defer t)

(use-package tree-sitter-langs
  :ensure t
  :defer t)
;; Use org-download to insert images in org files
(use-package org-download
  :after org
  )

;; (defun efs--lsp-mode-setup ()
;;   (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
;;   (lsp-headerline-breadcrumb-mode))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package lsp-mode
  :defer t
  :commands (lsp lsp-deferred lsp-mode)
  :hook ((vhdl-mode . lsp-deferred) ;; FIXME: Or just "lsp"?
         (verilog-mode . lsp-deferred)
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
        lsp-completion-show-detail t
        lsp-completion-show-kind t
        lsp-completion-enable t
        lsp-completion-enable-additional-text-edit t
        lsp-completion-provider :none ;; Only ":(company-)capf" is supported
        ; Headerline
        lsp-headerline-breadcrumb-mode t
        lsp-headerline-breadcrumb-enable t
        ; SignatureHelp
        lsp-signature-render-all t
        lsp-signature-auto-activate t
        lsp-signature-render-documentation t
        lsp-signature-function 'lsp-signature-posframe ; Use posframe with SignatureHelp. default: lsp-lv-message
        ; Other options
        lsp-document-sync-method 'lsp--sync-incremental
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package python-mode
;;   :defer t
;;   ;; :hook (python-mode . lsp-deferred)
;;   :custom
;;   ;; NOTE: Set these if Python 3 is called "python3" on your system!
;;   ;; (python-shell-interpreter "python3")
;;   ;; (dap-python-executable "python3")
;;   (dap-python-debugger 'debugpy)
;;   :config
;;   (require 'dap-python))

;; (use-package pyvenv
;;   :defer t
;;   :after python-mode
;;   :config
;;   (pyvenv-mode 1))


(use-package projectile
  :diminish projectile-mode
  :init
  (setq projectile-generic-command "fd . -0 --type f --color=never") ;; NOTE: Needs override when fd version < 8.3.0 (https://github.com/bbatsov/projectile/pull/1798/files)
  (setq projectile-git-fd-args "-H -0 -E .git -tf") ;; NOTE: Needs override when fd version < 8.3.0 (https://github.com/bbatsov/projectile/pull/1798/files)
  ;; NOTE: Set this to the folder where you keep your Git repos
  (when (file-directory-p "~/github")
    (setq projectile-project-search-path '("~/github")))
  (setq projectile-switch-project-action #'projectile-dired)
  (setq projectile-sort-order 'recently-active)
  (setq projectile-per-project-compilation-buffer t)
  :custom
  (projectile-project-root-files '("rebar.config" "project.clj" "build.boot" "deps.edn" "SConstruct" "pom.xml" "build.sbt" "gradlew" "build.gradle" ".ensime" "Gemfile" "requirements.txt" "setup.py" "pyproject.toml" "tox.ini" "composer.json" "Cargo.toml" "mix.exs" "stack.yaml" "info.rkt" "DESCRIPTION" "configure.in" "configure.ac" "cscope.out"))
  (projectile-project-root-files-bottom-up '(".projectile" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs"))
  :config
  (projectile-mode)
  (setq projectile-globally-ignored-files
        ;; NOTE 03-09-2019: List does not get passed to rg. Place ignore patterns in .ignore in project root.
        (append '("*.backup.log"
                  "hg_info"
                  "*.tar"
                  "*.str"
                  "*.pyc"
                  "*.bak"
                  "*_bak"
                  "*.orig"
                  "GRTAGS"
                  "GTAGS"
                  "GPATH")
                projectile-globally-ignored-files)
        projectile-globally-ignored-directories
        (append '(".emacs.d"
                  ))
        )
  ;; Set projectile project name in frame title
  (setq frame-title-format
        '(""
          "%b"
          (:eval
           (let ((project-name (projectile-project-name)))
             (unless (string= "-" project-name)
               (format " in [%s]" project-name))))))
  (efs-leader
   "p c" '(projectile-commander :wk "projectile-commander")
   "p f" '(projectile-find-file :wk "find-file")
   "p m" '(projectile-command-map :wk "projectile-command-map")
   )
)

(use-package rg
  :ensure t
  :config
  (rg-enable-menu)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Version control config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
  :ensure t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (defvar efs--magit-status-prev-window-config-table (make-hash-table :test #'equal)
    "Hash-table to store the previous window configuration before running magit-status for different perspectives.")
  (defun efs--get-or-set-magit-prev-config ()
    "Get or Set the previous window configuration based on persp-last-persp-name."
    (let ((window-config (gethash persp-last-persp-name efs--magit-status-prev-window-config-table)))
      (unless window-config
        (setq window-config (current-window-configuration))
        (puthash persp-last-persp-name window-config efs--magit-status-prev-window-config-table))
      window-config))
  ;; FIXME 11-10.2023: Need to define this instead of using display-buffer-full-frame that is only introduced in Emacs 29
  (defun efs--display-buffer-full-window (buffer alist)
    "Display BUFFER in a full window. ALIST is the display-buffer's ALIST."
    (let ((window (display-buffer-use-some-window buffer alist)))
      (when window
        (delete-other-windows window)
        window)))
  ;; FIXME 11-10-2023: Need to define this expected function that is only introduced in Emacs 29 (https://github.com/magit/magit/issues/5011)
  (defun seq-keep (function sequence)
    "Apply FUNCTION to SEQUENCE and return the list of all the non-nil results."
    (delq nil (seq-map function sequence)))
  (defun efs--magit-status ()
    "Version of magit-status that opens in full frame and restores previous window config on quit."
    (interactive)
    ;; Save window configuration if not already saved. It will be reset to nil when quitting the magit-status buffer.
    (unless (bound-and-true-p winner-mode)
      (winner-mode 1))
    ;; Save the window config
    (efs--get-or-set-magit-prev-config)
    ;; Tell magit-status buffer to open in full frame
    (let ((display-buffer-alist
           '(("^magit: " 
              (efs--display-buffer-full-window)
              ))))
      (magit-status)))
  (defun efs--restore-magit-windows (orig-fun &rest args)
    "Restore window configuration to before magit-status was opened. Only restore if buffer is magit-status-mode."
    (interactive)
    (if (and (gethash persp-last-persp-name efs--magit-status-prev-window-config-table)
             (string-equal "magit-status-mode" (symbol-name major-mode)))
        (progn
          (set-window-configuration (gethash persp-last-persp-name efs--magit-status-prev-window-config-table))
          (remhash persp-last-persp-name efs--magit-status-prev-window-config-table)) ;; Clear the variable after quitting
      (apply orig-fun args)))
  (advice-add 'magit-mode-quit-window :around #'efs--restore-magit-windows)

  ;; Set global key bindings
  (efs-leader
   "g s" '(efs--magit-status :wk "magit status")
   ))

(use-package git-gutter
  :ensure t
  :after transient
  :diminish git-gutter-mode
  :config 
  (set-face-background 'git-gutter:modified "#4f97d7") ;; spacemacs blue
  (set-face-background 'git-gutter:added "#67b11d") ;; spacemacs green
  (set-face-background 'git-gutter:deleted "#f2241f") ;; spacemacs red
  (setq git-gutter:modified-sign " ") ;; One colored space (multiple characters would be ok)
  (setq git-gutter:added-sign " ")    ;; One colored space (multiple characters would be ok)
  (setq git-gutter:deleted-sign " ")  ;; One colored space (multiple characters would be ok)
  ;; (setq git-gutter:lighter " GG")     ;; Set git-gutter name in the modeline

  ;; Face definition for horizontal ruler
  (defface efs--horizontal-rule
    '((default :inherit 'org-hide)
      (((background light)) :strike-through "gray70")
      (t :strike-through "gray30"))
    "Face used for horizontal ruler.")
  ;; Define functions for toggling visibility of unmodified lines
  (setq efs--git-gutter-overlays nil)
  (defun efs--line-modified-p ()
    "Is the current line modified?"
    (let ((current-line (line-number-at-pos))
          (ranges (mapcar (lambda (hunk) 
                            (cons (git-gutter-hunk-start-line hunk) 
                                  (git-gutter-hunk-end-line hunk)))
                          git-gutter:diffinfos)))
      (seq-find (lambda (range)
                  (<= (car range) current-line (cdr range)))
                ranges)))
  (defun efs--toggle-modified-lines-visibility ()
    "Toggle hiding/showing git-gutter unmodified lines and replace them with [...]"
    (interactive)
    ;; overlays exist, remove them
    (if efs--git-gutter-overlays
        (progn
          (mapc 'delete-overlay efs--git-gutter-overlays)
          (setq efs--git-gutter-overlays nil))
      ;; no overlays, create them
      (save-excursion
        (goto-char (point-min))
        (let ((in-unmod-seq nil)
              (start-unmod-seq nil))
          (while (not (eobp))
            (if (not (efs--line-modified-p))
                (unless in-unmod-seq
                  (setq in-unmod-seq t)
                  (setq start-unmod-seq (line-beginning-position)))
              ;; (when in-unmod-seq
              ;;   (let ((overlay (make-overlay start-unmod-seq (line-end-position 0))))
              ;;     ;; (overlay-put overlay 'display "[...]")
              (when in-unmod-seq
                (let ((overlay (make-overlay start-unmod-seq (line-end-position 0))))
                  (overlay-put overlay 'display "                                             ")
                  (overlay-put overlay 'face 'efs--horizontal-rule)
                  (push overlay efs--git-gutter-overlays)
                  (setq in-unmod-seq nil))))
            (forward-line 1))))))
  (defun efs--revert-hidden-lines ()
    "Revert hidden lines when quitting git-gutter transient state."
    (when efs--git-gutter-overlays
      (efs--toggle-modified-lines-visibility)))
  ;; Run efs--revert-hidden-lines when transient-quit-all is called
  (advice-add 'transient-quit-all :after #'efs--revert-hidden-lines)
  ;; Define transient state for git-gutter
  (transient-define-prefix efs--git-gutter-transient ()
    "Git-gutter transient state"
    ["Git-gutter transient state"
     :class transient-columns
     ["Symbol navigation"
      ("n" git-gutter:next-hunk :transient t :description "next hunk")
      ("N" git-gutter:previous-hunk :transient t :description "prev hunk")]
     ["Other"
      ("p" git-gutter:popup-hunk :transient t :description "popup hunk")
      ("t" efs--toggle-modified-lines-visibility :transient t :description "toggle context lines")
      ("q" transient-quit-all :description "quit")]
     ])
  ;; Global keys
  (efs-leader
    "g g" '(efs--git-gutter-transient :wk "git-gutter transient")
    ))

;; Highlight version control differences in gutter
(use-package diff-hl)

(defun efs--set-vc-visualize ()
  "Choose either git-gutter-mode or diff-hl-mode based on VC backend."
  (interactive)
  (let ((backend (vc-backend buffer-file-name)))
    (cond
     ((eq backend 'Git)
      ;; If in a Git repo, enable git-gutter-mode and disable diff-hl-mode
      (git-gutter-mode 1)
      (when (bound-and-true-p diff-hl-mode)
        (diff-hl-mode -1)))
     ;; If not in a Git repo, enable diff-hl-mode and disable git-gutter-mode
     (t
      (diff-hl-mode 1)
      (when (bound-and-true-p git-gutter-mode)
        (git-gutter-mode -1))))))

;; Add the function to the `find-file-hook` to automatically select the appropriate mode
;; when a file is opened.
(add-hook 'find-file-hook 'efs--set-vc-visualize)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package evil-nerd-commenter
  :after evil
  :config
  (efs-leader
   "c l" '(evilnc-comment-or-uncomment-lines :wk "comment-or-uncomment-lines")
   "c p" '(evilnc-comment-or-uncomment-paragraphs :wk "comment-or-uncomment-paragraph")
   "c y" '(evilnc-copy-and-comment-lines :wk "copy-and-comment-lines")
   )
  )

(use-package rainbow-delimiters
  :defer t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package ialign
  :ensure t
  :after pcre2el
  :defer t)

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
    (let ((inhibit-message t)) ;; Suppress printing save-message to minibuffer
      (persp-save-state-to-file persp-auto-save-fname)))
  (run-with-timer 0 (* 5 60) 'efs--save-persp-state) ;; Save perspective every 5 minutes
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

;; ;; Force shells to open at the bottom
;; (add-to-list 'display-buffer-alist
;;              '("\\*e?shell\\*" display-buffer-in-direction
;;                (direction . bottom)
;;                (window . root)
;;                (window-height . 0.3)))

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
      ("q" transient-quit-all :description "quit")]
     ;; TODO: Bind navigation keys h/j/k/l to quit but do not show them in transient
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

;; Settings for horizontal/vertical scrolling
(setq scroll-margin     5              ;; Set top/bottom scroll margin in number of lines
      scroll-conservatively 1          ;; Set lines to top/bottom scroll
      hscroll-margin    15             ;; Set horizontal scroll margin in number of characters
      hscroll-step      1
      auto-hscroll-mode 'current-line) ;; Scroll horizontally on the selected line only (Emacs version 26.1 or larger)

;; Highlight text, press "S-<delimiter>" to surround text with delimiters.
;; Use "(" to include spaces around delimiters, use ")" for no spaces.
(use-package evil-surround
  :ensure t
  :after evil
  :config
  (global-evil-surround-mode 1)
  )

(use-package treemacs
  :ensure t
  :config
  (setq treemacs-git-mode 'simple)
  (setq treemacs-hide-gitignored-files-mode nil)
  ;; (setq treemacs-show-cursor t)
  (treemacs-fringe-indicator-mode 'only-when-focused) ;; FIXME: Must run this for indicator to show up:(with-selected-window (treemacs-get-local-window)
  ;   ----------------------------------------------------------------------------------------------     (set-window-fringes nil 100))
  ;; ???
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

;; Add verilog-mode and vhdl-mode to default-enabled flycheck modes
(use-package flycheck
  :ensure t
  :diminish flycheck-mode
  ;; :config
  ;; ; FIXME: Try removing these since they are part of lsp-mode
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
                (window-height . 0.25)))))
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Verilog settings
(use-package verilog-mode
  :ensure nil ;; verilog-mode is a native package
  :defer t
  :custom
  ;; LSP setup for verilog-mode
  ;; Disable use of hdl_checker first
  (lsp-clients-verilog-executable nil)
  ;; Configure svlangserver globally (Requires Verilator and Verible).
  ;; Can also be configured on a project basis in a .dir-locals.el
  (lsp-clients-svlangserver-launchConfiguration "verilator -sv --lint-only -Wall -Wno-fatal --assert --cdc")
  (lsp-clients-svlangserver-formatCommand "~/etc/verible/bin/verible-verilog-format")
  (lsp-clients-svlangserver-includeIndexing '["does-not-exist.sv"])
  (lsp-clients-svlangserver-excludeIndexing '["bbs/simulation/**/*.{v,vh,sv,svh}"
                                              "bbs/work*/**/*.{v,vh,sv,svh}"
                                              "bbs/design/afu/stratix10/pac_lc/axi_protocol_afu/**/*"
                                              "bbs/design/afu/stratix10/pac_lc/dummy_afu/**/*"
                                              "bbs/design/afu/stratix10/pac_lc/eth_afu/**/*"
                                              "bbs/design/afu/stratix10/pac_lc/hello_afu/**/*"
                                              "bbs/design/afu/stratix10/pac_lc/hello_afu_interrupt/**/*"
                                              "bbs/design/afu/stratix10/pac_lc/nlb_afu/**/*"
                                              "bbs/**/ip/**/*.{v,vh,sv,svh}"])
  ;; '(lsp-clients-svlangserver-workspace-additional-dirs '["/mnt/storage/projects/intel/ofs-platform-afu-bbb/"])
  (lsp-clients-svlangserver-lintOnUnsaved t)
  :config
  (setq verilog-auto-delete-trailing-whitespace t
        verilog-highlight-grouping-keywords nil
        verilog-highlight-p1800-keywords t
        verilog-highlight-modules t
        verilog-tab-always-indent t
        verilog-indent-level 2
        verilog-indent-level-behavioral 2
        verilog-indent-level-declaration 2
        verilog-indent-level-module 2
        verilog-indent-level-directive 2
        verilog-cexp-indent 4
        verilog-auto-lineup (quote all)
        verilog-auto-endcomments t
        verilog-auto-newline nil ;; Disable auto-newline on semicolon in Verilog
        verilog-linter "verilator -sv --lint-only -Wall --cdc --default-language 1800-2012"
        ;; verilog-linter "verilator -sv --lint-only -Wall --cdc +1800-2012ext+sv"
        )

  ;; Use 'verilator_bin' instead of 'verilator' which throws errors
  (setq
   ;; TODO: (flycheck-verilator-include-path ...)
   flycheck-verilog-verilator-executable "verilator_bin")

  ;; Any files that end in .v, .dv, .pv or .sv should be in verilog mode
  (add-to-list 'auto-mode-alist '("\\.[dsp]?va?h\\'" . verilog-mode)))


;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; tree-sitter for Verilog
;; ;; Requires 'tree-sitter-langs-install-grammars' to be executed first. tree-sitter-verilog is now part of that package.

;; (tree-sitter-require 'verilog)
;; (add-hook 'verilog-mode #'tree-sitter-mode)
;; (add-hook 'verilog-mode #'tree-sitter-hl-mode)
;; ;; (add-to-list 'tree-sitter-major-mode-language-alist #'(verilog-mode . verilog)) ;; FIXME (11-04-2022): Necessary until verilog-mode is added to the list in the tree-sitter-langs package: https://github.com/emacs-tree-sitter/tree-sitter-langs/pull/93

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom vhdl-mode settings
(use-package vhdl-mode
  :ensure nil ;; vhdl-mode is a native package
  :defer t
  :config
  (setq vhdl-array-index-record-field-in-sensitivity-list t
        vhdl-compiler "GHDL"
        vhdl-default-library "work"
        vhdl-hideshow-menu t
        vhdl-index-menu t ; Build file index for imenu when opened
        vhdl-intelligent-tab nil
        vhdl-makefile-default-targets (quote ("all" "clean" "library"))
        vhdl-source-file-menu t ; Add menu of all source files in current directory
        vhdl-speedbar-auto-open nil
        vhdl-speedbar-display-mode (quote files)
        vhdl-stutter-mode t ; Enable ".." -> "=>" and other shortcuts
        vhdl-use-direct-instantiation (quote standard) ; Only use direct instantiation of VHDL standard allows it (from '93)
        vhdl-end-comment-column 1000
        vhdl-standard '(8 nil)
        flycheck-vhdl-ghdl-executable "/usr/bin/ghdl"
        flycheck-ghdl-ieee-library "synopsys" ;;"standard"
        flycheck-ghdl-language-standard "08"
        ;; TODO: flycheck-ghdl-workdir "/home/chrbirks/github/dev_env/example_code/vhdl"
        )
  :hook
  ;; Consider underscores as part of word in vhdl-mode
  (vhdl-mode . (lambda ()
                 (modify-syntax-entry ?_ "w"))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Setup for VHDL language server

  ;; Set path to Rust VHDL-LS
  ;; (setq lsp-vhdl-server-path (file-truename "~/github/rust_hdl/target/debug/vhdl_ls")) ;; Only necessary if not in PATH
  (custom-set-variables
   '(lsp-vhdl-server 'vhdl-ls))
  ;; (setenv "VHDL_LS_CONFIG" (file-truename "~/github/dev_env/example_code/vhdl/vhdl_ls.toml"))

  ;; ;; Set path to hdl_checker
  ;; ;; See logfiles under /tmp/hdl_checker_*
  ;; (setq lsp-vhdl-server-path (file-truename "~/.local/bin/hdl_checker"))
  ;; (custom-set-variables
  ;;  '(lsp-vhdl-server 'hdl-checker))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Must be last
(elpaca-process-queues)
