;; -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Package management
;; elpaca

(defvar elpaca-installer-version 0.4)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil
                              :files (:defaults (:exclude "extensions"))
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
            (kill-buffer buffer)
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
        ;; Enable :elpaca use-package keyword.
        (elpaca-use-package-mode)
        ;; Assume :elpaca t unless otherwise specified.
        (setq elpaca-use-package-by-default t))

;; Block until current queue processed.
(elpaca-wait)

;; Install packages like this:
;; (use-package evil :demand t)
;; Expands to: (elpaca evil (use-package evil :demand t))

(use-package hydra
  :defer t
  :demand t)
(use-package use-package-hydra
  :demand t)

(add-hook 'elpaca-after-init-hook (lambda ()
                                    (defhydra hydra-elpaca-helper (:hint nil :color pink)
                                      "
_i_: info            _u_: update packages _r_: recipe copy
_l_: log             _f_: fetch updates
_m_: manager         _s_: status
_b_: browse packages _q_: quit
"
                                      ("i" elpaca-info :exit t)
                                      ("l" elpaca-log :exit t)
                                      ("s" elpaca-status :exit t)
                                      ("b" elpaca-browse :exit t)
                                      ("m" elpaca-manager :exit t)
                                      ("u" elpaca-update-all :exit t)
                                      ("r" elpaca-menu-item :exit t)
                                      ("f" elpaca-fetch-all :exit t)
                                      ("q" nil :color blue))
                                    ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Leader-key and key-map config
(use-package general
  :demand t
  :after evil
  :config
  (general-create-definer spacemacs-leader
                          :prefix "SPC"
                          :global-prefix "C-SPC"
                          :keymaps 'override ; Override other defines for SPC. This might be a bad solution?
                          :states '(normal visual motion emacs))
  ;; Global keybindings
  (spacemacs-leader
   ;:keymaps 'clojure-mode-map
   "SPC" 'execute-extended-command
   "'" '(vterm-toggle :which-key "vterm toggle")

   "TAB" '(indent-for-tab-command :which-key "indent-for-tab-command")

   "a" '(:ignore t :which-key "applications")
   "a k" 'hydra-elpaca-helper/body
   "a b" 'general-describe-keybindings
   "a o a" 'org-agenda
   "a o c" 'org-capture

   "b" '(:ignore t :which-key "buffers")
   "b n" '(next-buffer :which-key "next buffer")
   "b p" '(previous-buffer :which-key "previous buffer")
   "b d" 'kill-this-buffer

   "c" '(:ignore t :which-key "comments")

   "f" '(:ignore t :which-key "files")
   "f s" '(save-buffer :which-key "save buffer")
   "f S" '(evil-write-all :which-key "save all buffers")

   "g" '(:ignore t :which-key "version control")

   "j"  '(:ignore t :which-key "navigate")

   "l"  '(:ignore t :which-key "layouts")

   "p"  '(:ignore t :which-key "project")

   "r"  '(:ignore t :which-key "registers")

   "s"  '(:ignore t :which-key "search/symbol")

   "t"  '(:ignore t :which-key "toggles")
   "t l" '(toggle-truncate-lines :which-key "toggle-truncate-lines")
   "t t" '(consult-theme :which-key "choose theme")

   "q" '(:ignore t :which-key "quit")
   "q q" '(save-buffers-kill-terminal :which-key "quit emacs")

   "w" '(:ignore t :which-key "windows")
   "w d" '(delete-window :which-key "delete window")
   "w -" '(split-window-below :which-key "split below")
   "w /" '(split-window-right :which-key "split right")
   "w b" '(split-root-window-below :which-key "split below all")
   "w r" '(split-root-window-right :which-key "split right all")
   "w m" '(efs/toggle-maximize-buffer :which-key "maximize buffer")


   ;; "f d e" '(lambda () (interactive) (find-file (expand-file-name "~/.emacs.d/Emacs.org")))
 )
  )

(elpaca-wait)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq
 ;; No need to remind me what a scratch buffer is.
 initial-scratch-message nil
 ;; Never ding at me, ever.
 ring-bell-function 'ignore
 ;; Prompts should go in the minibuffer, not in a GUI.
 ;; use-dialog-box nil
 ;; Fix undo in commands affecting the mark.
 mark-even-if-inactive nil
 ;; Disable mouse acceleration
 mouse-wheel-progressive-speed nil
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

;; The default is 800 kilobytes.  Measured in bytes.
(setq gc-cons-threshold (* 50 1000 1000))

;; Prevent warning buffer from stealing focus on every new warning
(setq warning-minimum-level :warning)

(setq initial-scratch-message nil)

(setq-default
   debug-on-error t
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
(if (window-system) (set-frame-size (selected-frame) 140 82))

;; This helps to clear the clutter and increases reproducibility of the config
;; (let
;;     ((customization-file (expand-file-name "custom.el" user-emacs-directory)))
;;   (when (file-exists-p customization-file)
;;     (setq custom-file customization-file)
;;     (load custom-file 'noerror)))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(add-hook 'elpaca-after-init-hook (lambda () (load custom-file 'noerror)))

(setq inhibit-startup-message t)

(scroll-bar-mode -1)        ; Disable visible scrollbar
(tool-bar-mode -1)          ; Disable the toolbar
(tooltip-mode -1)           ; Disable tooltips
(set-fringe-mode 5)        ; Give some breathing room
(menu-bar-mode -1)          ; Disable the menu bar

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
(defvar efs/default-font-size 100)
(defvar efs/default-variable-font-size 100)

(set-face-attribute 'default nil :font "MesloLGS Nerd Font Mono" :height efs/default-font-size)

;; Set the fixed pitch face
(set-face-attribute 'fixed-pitch nil :font "MesloLGS Nerd Font Mono" :height efs/default-font-size)

;; Set the variable pitch face
;; TODO: Something else than mono
(set-face-attribute 'variable-pitch nil :font "MesloLGS Nerd Font Mono" :height efs/default-variable-font-size :weight 'regular)

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
; Add custom snippets dir
; TODO: chezmoi template
(setq yas-snippet-dirs '("~/.config/emacs/snippets" "~/etc/spacemacs.d/private/snippets/" "~/etc/spacemacs.d/layers/+completion/auto-completion/local/snippets" yasnippet-snippets-dir))

(use-package no-littering
  :demand t
  :defer nil)


(use-package evil
  :demand t
  :init
  (setq evil-want-integration t) ; set to t before loading evil-collections
  (setq evil-want-keybinding nil) ; Set to nil before loading evil-collections
  (setq evil-want-C-u-scroll t)
  (setq evil-want-C-i-jump t)
  (setq evil-want-fine-undo nil)
  :config
  (evil-mode 1)
  (if (version<= emacs-version "28")
      (evil-set-undo-system 'undo-tree)
    (evil-set-undo-system 'undo-redo)
    )
  (define-key evil-insert-state-map (kbd "C-g") 'evil-normal-state)
  (define-key evil-insert-state-map (kbd "C-h") 'evil-delete-backward-char-and-join)

  ;; Set cursor based on evil state
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
  :config
  (evil-collection-init) ;; Register evil binding for all modes at once instead of calling individual *-setup functions
  (setq evil-want-keybinding t)
  ;; Set Avy to use actual words instead of sequences of letters (requires Avy 0.5.0)
  (setq avy-style 'words)
  (spacemacs-leader
   "j j" '(evil-avy-goto-char-timer :which-key "jump to char")
   "j l" '(evil-avy-goto-line :which-key "jump to line")
   "j w" '(evil-avy-goto-word-or-subword-1 :which-key "jump to word")))

(use-package evil-escape
  :demand t
  :init
  (setq evil-escape-key-sequence "fd"
        evil-escape-delay 0.15)
  :config
  (evil-escape-mode))

(use-package evil-iedit-state
  :after evil
  :config
  (spacemacs-leader
   "s e" '(evil-iedit-state/iedit-mode :which-key "iedit-mode at point")
   ))

(if (version<= emacs-version "28")
    ;; Use undo-tree for Emacs version earlier than 28
    (use-package undo-tree
      :demand t
      :config
      (global-undo-tree-mode)
      (spacemacs-leader
       "a u" '(undo-tree-visualize :which-key "undo-tree-visualize"))
      ;; :bind (:map evil-normal-state-map
      ;;             ("j" . undo-tree-visualize-redo)
      ;;             ("k" . undo-tree-visualize-undo)
      ;;             ("h" . undo-tree-visualize-switch-branch-left)
      ;;             ("l" . undo-tree-visualize-switch-branch-right))
      ;; Do not save undo-tree files named .~undo-tree~ everywhere.
      (setq undo-tree-auto-save-history nil)
      ;; Or make place the files in /.emacs.d/undo instead
      ;; (setq undo-tree-history-directory-alist '(("." . "/.emacs.d/undo")))
  )

  ;; Use vundo supported for Emacs > v28
  (use-package vundo
    :demand t
    :config
    (setq vundo-glyph-alist vundo-unicode-symbols) ;; Use unicode symbols instead of default ASCII
    (spacemacs-leader
     "a u" '(vundo :which-key "vundo tree")) ;; "C-n": vundo-next, "C-p": vundo-previous, "h"; vundo-backwards, "l": vundo-forwards
    )
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Spaceline config

(use-package spaceline
  :config
  (require 'spaceline-config)
  (spaceline-spacemacs-theme) ; Set the theme
  ;; (spaceline-helm-mode) ; Special minor-mode for Helm
  ;; (spaceline-info-mode) ; Special minor-mode for info+
  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state) ; Color according to evil state
  (setq powerline-height 18)
  (setq powerline-default-separator 'arrow)
  (setq spaceline-workspace-numbers-unicode t) ; Get unicode numbers when using window-numbering-mode
  (setq spaceline-window-numbers-unicode t) ; Get unicode numbers when using eyebrowse-mode
)

;; TODO: Try doom-modeline instead

;; (use-package spaceline-all-the-icons
;;   :demand t
;;   :after spaceline
;;   :config
;;   (setq spaceline-all-the-icons-separator-type 'arrow)
;;   (setq spaceline-all-the-icons-file-name-highlight t)
;;   (setq spaceline-all-the-icons-highlight-file-name t)
;;   (setq spaceline-all-the-icons-window-number-always-visible t)
;;   (spaceline-all-the-icons--setup-git-ahead)
;;   (spaceline-toggle-all-the-icons-dedicated-on)
;;   (spaceline-toggle-all-the-icons-fullscreen-on)
;;   (spaceline-toggle-all-the-icons-buffer-position-on)
;;   (spaceline-all-the-icons-theme)
;;   )

(defvar toggle-maximized-buffer-state nil
  "State variable to track the maximization status of the buffer.")

(defvar toggle-maximized-buffer-prev-config nil
  "Variable to store the previous window configuration before maximizing.")

(defun efs/toggle-maximize-buffer ()
  "Toggle the maximization of the current buffer. Plays nice with a treemacs buffer."
  (interactive)
  (unless (bound-and-true-p winner-mode)
    (winner-mode 1))
  (if toggle-maximized-buffer-state
      (progn
        (when toggle-maximized-buffer-prev-config
          (set-window-configuration toggle-maximized-buffer-prev-config))
        (setq toggle-maximized-buffer-state nil))
    (progn
      (setq toggle-maximized-buffer-prev-config (current-window-configuration))
      (delete-other-windows) ;; Maybe use treemacs-delete-other-windows
      (setq toggle-maximized-buffer-state t))))

;; TODO: Add maximized indicator to modeline

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; nano-sidebar
;; (elpaca-use-package (nano-sidebar
(elpaca (nano-sidebar
  :demand t
  :host github
  :repo "rougier/nano-sidebar"))

;; (add-hook 'elpaca-after-init-hook (lambda () (
  ;; (require 'nano-sidebar)

  (defun ibuffer-advice (format)
    (with-current-buffer "*Ibuffer*"
      (save-excursion
        (let ((inhibit-read-only t))

          ;; Remove header and insert ours
          (goto-char (point-min))
          (search-forward "-\n" nil t)
          (delete-region 1 (point))
          (goto-char (point-min))
          (insert (concat
                   (propertize "\n" 'face '(:height 1.2))
                   (propertize " "  'display `(raise +0.25))
                   (propertize "  Buffers list (ibuffer)"
                               'face 'nano-faded)
                   (propertize " "  'display `(raise -0.35))
                   "\n"))
          (insert "")

          ;; Transform titles
          (goto-char (point-min))
          (while (re-search-forward "\\[ \\(.*\\) \\]" nil t)
            (let* ((title (match-string 0))
                   (property (get-text-property 0 'ibuffer-filter-group-name title)))
              (replace-match "\n")
              (insert (concat
                       (propertize
                        (format "   %s " (substring title 2 -2))
                        'ibuffer-filter-group-name property)
                       (propertize
                        (make-string (- 30 (length title)) ?—)
                        'face 'nano-faded)
                       "\n"))))))))


  (setq ibuffer-saved-filter-groups
        '(("home"
           ("Configuration" (or (filename . ".emacs.d")
                                (filename . "emacs-config")))
           ("Org" (or (mode . org-mode)
                      (filename . "OrgMode")))
           ("Code" (or  (derived-mode . prog-mode)
                        (mode . ess-mode)
                        (mode . compilation-mode)))
           ("Text" (and (derived-mode . text-mode)
                        (not  (starred-name))))
           ("TeX"  (or (derived-mode . tex-mode)
                       (mode . latex-mode)
                       (mode . context-mode)
                       (mode . ams-tex-mode)
                       (mode . bibtex-mode)))
           ("Help" (or (name . "\*Help\*")
                       (name . "\*Apropos\*")
                       (name . "\*info\*"))))))

  (setq ibuffer-show-empty-filter-groups nil)
  (setq ibuffer-display-summary nil)
  (setq ibuffer-use-header-line nil)
  (setq ibuffer-eliding-string (propertize "…" 'face 'nano-salient))
  (setq ibuffer-fontification-alist '((0 t nano-salient)))
  (setq ibuffer-formats
        '(("  "  mark " "(name 24 24 :left :elide) "  " modified)
          (mark " " (name 16 -1) " " filename)))

  (defun ibuffer-setup ()
    (ibuffer-switch-to-saved-filter-groups "home")
    (ibuffer-auto-mode 1))

  (defun nano-sidebar-init-ibuffer (frame sidebar)
    "Default sidebar initialization"

    (select-frame frame)
    (let ((buffer (current-buffer)))
      (ibuffer)
      (switch-to-buffer buffer))
    (select-frame sidebar)
    (switch-to-buffer "*Ibuffer*")
    (set-window-dedicated-p (get-buffer-window "*Ibuffer*") t)
    (hl-line-mode)
    (setq header-line-format nil)
    (setq mode-line-format nil))


  (setq nano-sidebar-default-init 'nano-sidebar-init-ibuffer)
  (advice-add 'ibuffer-update-title-and-summary :after #'ibuffer-advice)
  (add-hook 'ibuffer-mode-hook #'ibuffer-setup)
  ;; )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package which-key
  :defer 0
  :diminish which-key-mode
  :config
  (which-key-mode)
  (setq which-key-idle-delay 0.4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package wgrep)
(use-package deadgrep)

(use-package recentf
  :elpaca nil ;; recentf is a native package
  :demand t
  :init
  (run-at-time nil (* 5 60) 'recentf-save-list) ;; Save recent files every 5 minutes
  :config
  (setq recentf-save-file (expand-file-name "var/recentf-save.el" user-emacs-directory)
        ;; recentf-auto-cleanup 'never
        recentf-max-menu-items 25
        recentf-max-saved-items 25)
  (add-to-list 'recentf-exclude ".*/\\.config/emacs-from-scratch/var/persp-mode/.*")
  (add-to-list 'recentf-exclude ".*/\\.config/emacs-from-scratch/var/treemacs/persist\\.org$")
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
  (spacemacs-leader
   "f f" '(find-file :which-key "find file")
   "f A" '(find-alternate-file :which-key "replace buffer with file")
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
  (marginalia-align 'right)
  :init
  (marginalia-mode)
  :bind (:map minibuffer-local-map
              ("C-<f1>" . 'marginalia-cycle) ; Cycle through marginalia modes while in minibuffer
              )
  )

;; Embark for executing commands on selected completion items (killing buffer, etc.)
(use-package embark
  :init
  ;; Replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :bind
  ("C-h B" . embark-bindings)
  ("C-c c" . embark-act)
  ("C-c C" . embark-dwim)
  (:map minibuffer-local-map
        (("M-M" . embark-collect-toggle-marks)
         ("M-E" . embark-export) ;; Export all minibuffer result to new buffer. Run occur-edit-mode/wgrep-change-to-wgrep-mode with "i"
         ("M-C" . embark-collect)) ;; "m": mark, "u": unmark, "t": mark all
        )
  )

;; +| Mark a candidate   | m           | a SPC         |
;; +| Unmark a candidate | u           | a SPC         |
;; +| Unmark all         | U           | A SPC         |
;; +| Mark all [1]       | t           | A SPC         |
;; +| Toggle all marks   | t           | not available |

(use-package embark-consult
  :after (embark consult)
  :hook
  (embark-collect-mode . consult-preview-at-point-mode)
  ;; (embark-collect-mode . consult-fontify-buffer-lines)
)

;; Consult for enhanced completion commands
(use-package consult
  :init
  ;; (setq consult-narrow-key (kbd "C-c n"))

  ;; Configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  :config
  ;; Use `consult-completion-in-region' if Vertico is enabled.
  ;; Otherwise use the default `completion--in-region' function.
  (setq completion-in-region-function
        (lambda (&rest args)
          (apply (if vertico-mode
                     #'consult-completion-in-region
                   #'completion--in-region)
                 args)))

  ;; ;; List only buffers in current prespective ;; FIXME: Doesn't do anything
  ;; ;; (with-eval-after-load 'perspective
  ;; (with-eval-after-load 'persp
  ;;   (consult-customize
  ;;    consult--source-buffer :hidden t :default nil)
  ;;   (add-to-list 'consult-buffer-sources persp-consult-source))

  ;; Other setup
  ;; vertico-grid-annotate, vertico-flat-annotate
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   :preview-key 'any)

  ;; Configure the narrowing key.
  ;; Both < and C-+ work reasonably well.
  (setq consult-narrow-key "C-<") ;; "C-+"

  ;; Make narrowing help available in the minibuffer.
  ;; You may want to use `embark-prefix-help-command' or which-key instead.
  ;; (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'consult-narrow-help)
  (define-key consult-narrow-map (vconcat consult-narrow-key "?") #'which-key)

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
  (spacemacs-leader
   "b b" '(consult-buffer :which-key "switch buffer") ; TODO: How to sort dirs, files, *-buffers, etc.
   "b P" '(consult-project-buffer :which-key "switch project buffer")
   ; ("C-x 4 b" . consult-buffer-other-window) ;; orig. switch-to-buffer-other-window
   ; ("C-x 5 b" . consult-buffer-other-frame)  ;; orig. switch-to-buffer-other-frame

   "f r" '(consult-recent-file :which-key "recent files")
   "r y" '(consult-yank-from-kill-ring :which-key "yank kill-ring")
   "r s" '(consult-register-store :which-key "store register")
   "r r" '(consult-register :which-key "select register")

   "s s" '(consult-line :which-key "seach buffer")
   "s S" '(efs--consult-line-symbol-at-point :which-key "search buffer at point")
   ;; "s S" '(consult-line-multi :which-key "seach all buffers") ; TODO: Do not search virtual buffers
   "s p" '(consult-ripgrep :which-key "search project")
   "s r" '(rg-menu :which-key "ripgrep-menu")
   "s d" '(deadgrep :which-key "deadgrep")

   ;; "p f" '(counsel-projectile-find-file :which-key "find file")
   "p b" '(consult-project-buffer :which-key "project buffers")
   "p i" '(consult-imenu :which-key "project imenu")
   "p I" '(consult-imenu-multi :which-key "project imenu-multi")

   "t m" '(consult-minor-mode-menu :which-key "enable/disable minor-mode")
   )
  )

(use-package corfu
  :demand t
  :bind
  (:map corfu-map
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
)

;; Load corfu extensions
(with-eval-after-load 'corfu
  (add-to-list 'load-path "~/.config/scratch-emacs/elpaca/repos/corfu/extensions/")
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

;; Cape - Completion At Point Extensions
(defun efs/cape-capf-setup-verilog ()
  "Create completions backends for verilog-mode"
  ;; (let ((result))
  ;;   (dolist (element '(verilog-completion-at-point cape-symbol cape-dabbrev) result)
  ;;     (add-to-list 'completion-at-point-functions element))))
  (let (result)
    (dolist (element (list
                      (cape-super-capf #'verilog-completion-at-point #'cape-symbol #'cape-dabbrev))
                     result)
      (add-to-list 'completion-at-point-functions element))))

;; (defun efs/cape-capf-setup-lsp ()
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
  (verilog-mode . efs/cape-capf-setup-verilog)
)

;; Icons for corfu completion buffer
(use-package kind-icon
  :demand t
  :after corfu
  :custom
  (kind-icon-use-icons t)
  (kind-icon-default-face 'corfu-default) ; Have background color be the same as `corfu' face background
  (kind-icon-blend-background nil)  ; Use midpoint color between foreground and background colors ("blended")?
  (kind-icon-blend-frac 0.08)

  ;; NOTE 2022-02-05: `kind-icon' depends `svg-lib' which creates a cache
  ;; directory that defaults to the `user-emacs-directory'. Here, I change that
  ;; directory to a location appropriate to `no-littering' conventions, a
  ;; package which moves directories of other packages to sane locations.
  (svg-lib-icons-dir (no-littering-expand-var-file-name "svg-lib/cache/")) ; Change cache dir
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter) ; Enable `kind-icon'
  )

;; (use-package corfu-popupinfo
;;   :demand t
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
;;   :demand t
;;   :after corfu
;;   )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package all-the-icons
  :demand t)

;; Get file and buffer icons in minibuffer
(use-package all-the-icons-completion
  :demand t
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
  :custom
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  ("C-h f" . helpful-function)
  ([remap describe-symbol] . helpful-symbol)
  ([remap describe-variable] . helpful-variable)
  ([remap describe-command] . helpful-command)
  ([remap describe-key] . helpful-key))


(defun efs/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(use-package org
  :demand t
  ;; :pin org
  :commands (org-capture org-agenda)
  :hook (org-mode . efs/org-mode-setup)
  :config
  (setq org-log-into-drawer '("LOOGBOOK")
        org-directory "~/org/"
        org-default-notes-file "~/org/notes.org" ;; Default file for templates that does not specify a file
        ;; Org Projectile
        org-projectile-file "TODOs.org"
        org-projectile-projects-file "~/org/projects/misc-TODOs.org"
        org-projectile-per-project-filepath "~/org/projects/TODOs-%s.org" ;; Per-project org-projectile files if org-projectile-projects-directory is nil ;; FIXME: 28-12-22: Cannot get it to work
        ;; Org Roam
        org-roam-directory "~/org/roam/"
        org-roam-dailies-directory "daily/" ;; Relative to org-roam-directory
        org-roam-db-location "~/.config/emacs/org-roam.db"
        ;; Org Agenda
        org-agenda-files '("~/org/tasks.org"
                           "~/org/notes.org"
                           "~/org/projects/TODOs.org"
                           "~/org/projects/weibel/TODOs.org"
                           "~/org/projects/misc-TODOs.org"
                           )
        )
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
    ;; Set deadgrep search root to org-directory
    (setq deadgrep-project-root-overrides `(("~/" . ,org-directory)))

    (call-interactively #'deadgrep deadgrep-project-root-overrides)
    )
  ;; Define Org (not roam) capture templates
  (setq org-capture-templates
        '(("t" "TODO" entry
           (file "~/org/projects/TODOs.org")
           (file "~/.config/emacs/org-templates/todo.org")
           :empty-lines-before 1
           :unnarrowed nil)
          ("w" "Weibel TODO" entry
           (file "~/org/projects/weibel/TODOs.org")
           (file "~/.config/emacs/org-templates/weibel-todo.org")
           :empty-lines-before 1
           :unnarrowed nil)))
  (spacemacs-leader
   "a o" '(:ignore t :which-key "org")
   "a o r" '(:ignore t :which-key "roam")
   "a o r d" '(:ignore t :which-key "dailies")
   "a o r d t" '(org-roam-dailies-goto-today :which-key "goto today")
   "a o r d d" '(org-roam-dailies-goto-date :which-key "goto date")
   )
  )

(use-package org-roam-ui
  :ensure t
  :after org
  :config
  (spacemacs-leader
   "a o r u" '(org-roam-ui-open :which-key "org-roam-ui-open")))

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
  )

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
            ("WAITING"  :foreground "yellow"  :background "#292b2e" :weight normal)
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
  :config
  ;; Define Org-roam capture templates
  (setq org-roam-capture-templates
        '(
          ;; Default template
          ("d" "Default" plain
           (file "~/.config/emacs/org-roam-templates/default.org")
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "# -*- mode: org; eval: (setq-local org-download-image-dir \"images\" org-download-heading-lvl nil) -*-
#+title: ${title}
")
           :unnarrowed t)
          ;; Ølbrygning template
          ("o" "Ølbrygning" plain
           (file "~/.config/emacs/org-roam-templates/ølbrygning.org")
           :target (file+head "%<%Y%m%d%H%M%S>-${slug}.org"
                              "# -*- mode: org; eval: (setq-local org-download-image-dir \"images\" org-download-heading-lvl nil) -*-
#+title: ${title}
#+filetags: :ølbrygning:
#+date: %^t

")
           :unnarrowed t)
          ;; Weibel template
          ("w" "Weibel" plain
           (file "~/.config/emacs/org-roam-templates/weibel.org")
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


;; (defun efs/lsp-mode-setup ()
;;   (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
;;   (lsp-headerline-breadcrumb-mode))

;; (use-package lsp-mode
;;   :defer t
;;   :commands (lsp lsp-deferred)
;;   :hook (lsp-mode . efs/lsp-mode-setup)
;;   :init
;;   (setq lsp-keymap-prefix "C-c l")  ;; Or 'C-l', 's-l'
;;   :config
;;   (lsp-enable-which-key-integration t))

;; (use-package lsp-ui
;;   :hook (lsp-mode . lsp-ui-mode)
;;   :custom
;;   (lsp-ui-doc-position 'bottom))

;; (use-package lsp-treemacs
;;   :after lsp)

;; (use-package dap-mode
;;   ;; Uncomment the config below if you want all UI panes to be hidden by default!
;;   ;; :custom
;;   ;; (lsp-enable-dap-auto-configure nil)
;;   ;; :config
;;   ;; (dap-ui-mode 1)
;;   :commands dap-debug
;;   :config
;;   ;; Set up Node debugging
;;   (require 'dap-node)
;;   (dap-node-setup) ;; Automatically installs Node debug adapter if needed

;;   ;; ;; Bind `C-c l d` to `dap-hydra` for easy access
;;   ;; (general-define-key
;;   ;;   :keymaps 'lsp-mode-map
;;   ;;   :prefix lsp-keymap-prefix
;;   ;;   "d" '(dap-hydra t :wk "debugger"))
;; )


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
  (spacemacs-leader
   "p c" '(projectile-commander :which-key "projectile-commander")
   "p f" '(projectile-find-file-dwim :which-key "find file")
   "p m" '(projectile-command-map :which-key "projectile command map")
   )
)


(use-package rg
  :demand t
  :config
  (rg-enable-menu)
  )

(use-package magit
  :demand t
  ;; :commands magit-status
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  (spacemacs-leader
   "g s" '(magit-status :which-key "magit status")
   )
  )

(use-package git-gutter
  :ensure t
  :init
  :config (global-git-gutter-mode +1)
  (set-face-background 'git-gutter:modified "#4f97d7") ;; spacemacs blue
  (set-face-background 'git-gutter:added "#67b11d") ;; spacemacs green
  (set-face-background 'git-gutter:deleted "#f2241f") ;; spacemacs red
)
(use-package evil-nerd-commenter
  :after evil
  :config
  (spacemacs-leader
   "c l" '(evilnc-comment-or-uncomment-lines :which-key "comment-or-uncomment-lines")
   "c p" '(evilnc-comment-or-uncomment-paragraphs :which-key "comment-or-uncomment-paragraph")
   "c y" '(evilnc-copy-and-comment-lines :which-key "copy-and-comment-lines")
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
  :demand t
  :config
  (setq winum-auto-setup-mode-line nil) ;; Do not display window number in modeline since it's already included in spaceline
  (setq winum-ignored-buffers '("*Java Dependency List*" "*LSP Error List*" "*LSP Symbols List*" " *Treemacs-Framebuffer-5*" " *Treemacs-Framebuffer-4*" " *Treemacs-Framebuffer-3*" " *Treemacs-Framebuffer-2*" " *Treemacs-Framebuffer-1*" " *LV*" " *which-key*"))
  (setq winum-ignored-buffers-regexp '("\\*Treemacs-Scoped-Buffer-"))
  (winum-mode)
  (spacemacs-leader
   "1" '(winum-select-window-1 :which-key "select window 1")
   "2" '(winum-select-window-2 :which-key "select window 2")
   "3" '(winum-select-window-3 :which-key "select window 3")
   "4" '(winum-select-window-4 :which-key "select window 4")
   "5" '(winum-select-window-5 :which-key "select window 5")
   "6" '(winum-select-window-6 :which-key "select window 6")
   "7" '(winum-select-window-7 :which-key "select window 7")
   "8" '(winum-select-window-8 :which-key "select window 8")
   "9" '(winum-select-window-9 :which-key "select window 9")
   )
  )

;; Highlight words like todo, fixme, note, etc.
(use-package hl-todo
  :demand t
  :config
  (global-hl-todo-mode))

;; Highlight tabs ;; FIXME: Does not work
(setq whitespace-style '(face tabs))
(whitespace-mode)

(custom-set-variables
 '(tab-width 3)
 '(git-gutter:modified-sign " ") ;; One colored space (multiple characters would be ok)
 '(git-gutter:added-sign " ")    ;; One colored space (multiple characters would be ok)
 '(git-gutter:deleted-sign " ")  ;; One colored space (multiple characters would be ok)
 '(git-gutter:lighter " GG");; Set git-gutter name in the modeline
)

(custom-set-faces
 ;; Set face for persp-mode modeline for when open buffer is not in current perspective
 '(persp-face-lighter-buffer-not-in-persp ((t (:background "gold" :foreground "#00F" :weight bold))))
 ;; Use more faded colors for symbol-overlay
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;; (custom-set-faces
  '(symbol-overlay-face-1 ((t (:background "#689d6a"       :foreground "black"))))
  '(symbol-overlay-face-2 ((t (:background "#b08588"       :foreground "black"))))
  '(symbol-overlay-face-3 ((t (:background "#7c6f64"       :foreground "black"))))
  '(symbol-overlay-face-3 ((t (:background "#5c6f64"       :foreground "black"))))
  '(symbol-overlay-face-4 ((t (:background "#98971a"       :foreground "black"))))
  '(symbol-overlay-face-5 ((t (:background "#016286"       :foreground "black"))))
  '(symbol-overlay-face-6 ((t (:background "#d79921"       :foreground "black"))))
  '(symbol-overlay-face-7 ((t (:background "medium orchid" :foreground "black"))))
  '(symbol-overlay-face-8 ((t (:background "#fbe107"       :foreground "black"))))
  ;; )
)

;; FIXME: Buffers are shared between all perspectives
(use-package persp-mode
  :demand t
  :init
  (persp-mode)
  :config
  (defun efs/current-layout-name ()
    "Get name of the current perspective."
    (safe-persp-name (get-frame-persp)))

  (defun efs/layouts-ts-kill ()
    "Kill current perspective"
    (interactive)
    (persp-kill (efs/current-layout-name)))

;;   (defun spacemacs/layout-switch-by-pos (pos)
;;     "Switch to perspective of position POS.
;; If POS has no layout, create layout with auto-generated name. Otherwise,
;; ask the user if a new layout should be created."
;;     (let ((persp-to-switch
;;            (nth pos (persp-names-current-frame-fast-ordered))))
;;       (if persp-to-switch
;;           (persp-switch persp-to-switch)
;;         (let ((persp-reset-windows-on-nil-window-conf t)
;;               (generated-name (and dotspacemacs-auto-generate-layout-names
;;                                    (spacemacs//generate-layout-name pos))))
;;           (if generated-name
;;               (persp-switch generated-name) ; select an existing layout
;;             (persp-switch nil)              ; create a new layout
;;             (spacemacs/home-delete-other-windows))))))

;;   (defun spacemacs/layout-switch-to (pos)
;;     "Switch to perspective but ask for POS.
;; If POS has no layout, create layout with auto-generated name. Otherwise,
;; ask the user if a new layout should be created."
;;     (interactive "NLayout to switch to/create: ")
;;     (spacemacs/layout-switch-by-posos (1- pos)))

  ;; (add-hook 'kill-emacs-hook #'persp-state-save)
  (setq persp-auto-resume-time -1 ;; No autoload buffers
        ;; persp-save-dir ""
        persp-set-last-persp-for-new-frames nil
        persp-reset-windows-on-nil-window-conf t
        persp-autokill-buffer-on-remove t
        persp-add-buffer-on-after-change-major-mode t
        persp-sort 'created
        persp-show-modestring t
        persp-auto-save-opt 4
        persp-auto-save-persps-to-their-file-before-kill 'persp-file
        persp-kill-foreign-buffer-behaviour 'kill)
  ;; (define-key evil-normal-state-map (kbd "gt") 'persp-next)
  ;; (define-key evil-normal-state-map (kbd "gT") 'persp-prev)
  (with-eval-after-load "vterm"
    (persp-def-auto-persp "vterm"
                          :parameters '((dont-save-to-file . t))
                          :mode 'term-mode
                          :dyn-env '(after-switch-to-buffer-functions ;; prevent recursion
                                     (persp-add-buffer-on-find-file nil)
                                     persp-add-buffer-on-after-change-major-mode)
                          :hooks '(after-switch-to-buffer-functions)
                          :switch 'window))
  (spacemacs-leader
   ;; "l n" '(persp-add-new :which-key "new persp")
   "l l" '(persp-switch :which-key "switch persp")
   "l s" '(persp-save-state-to-file :which-key "save all to file")
   "l S" '(persp-save-to-file-by-names :which-key "save persp to file")
   "l L" '(persp-load-state-from-file :which-key "load from file")
   "l x" '(efs/layouts-ts-kill :which-key "kill layout")
   )
  )

(use-package vterm
  :preface
  (defun my/vterm-mode-hook ()
    (hl-line-mode -1)
    (display-line-numbers-mode -1)
    (display-fill-column-indicator-mode -1)
    (auto-fill-mode -1))
  :hook
  ((vterm-mode . my/vterm-mode-hook))
  :custom
  (vterm-kill-buffer-on-exit t)
  (vterm-max-scrollback 10000)
  (vterm-tramp-shells '(("ssh" "/bin/bash")))
  :init
  (which-key-add-key-based-replacements "C-c t" "term")
  :config
  (setq vterm-max-scrollback 10000)
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

(defun efs/symbol-overlay-put ()
  "Start symbol-overlay-transient-state."
  (interactive)
  (symbol-overlay-put)
  (efs/symbol-overlay-transient)
  )

(use-package symbol-overlay
  :demand t
  :after transient
  :config
  ;; Make transient state
  (transient-define-suffix efs--so-suffix-print-args (the-prefix-arg)
    "Report the PREFIX-ARG, prefix's scope, and infix values."
    :transient 'transient--do-call
    (interactive "P")
    (let ((args (transient-args (oref transient-current-prefix command)))
          (scope (oref transient-current-prefix scope)))
      (message "prefix-arg: %s \nprefix's scope value: %s \ntransient-args: %s"
               the-prefix-arg scope args)))
  (transient-define-infix efs--so-random-init-infix ()
    "Switch on and off."
    :argument "--switch"
    :shortarg "-s" ; will be used for :key when key is not set
    :description "switch"
    :init-value (lambda (obj)
                  (oset obj value
                        (eq 0 (random 2))))) ; write t with 50% probability
  (transient-define-argument efs--so-symbol-face-2 ()
    "This is a specialized infix for only selecting one of several values."
    :class 'transient-switches
    :argument-format "--%s-snowcone"
    :argument-regexp "\\(--\\(grape\\|orange\\|cherry\\|lime\\)-snowcone\\)"
    :choices '("grape" "orange" "cherry" "lime"))
  (transient-define-argument efs--so-symbol-face ()
    "This is a specialized infix for only selecting one of several values."
    :argument "face="
    :class 'transient-option
    :choices '("face1" "face2" "face3" "face4"))
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
    ;; :description '(lambda ()
    ;;                    (concat
    ;;                     "set sentence: "
    ;;                     (propertize
    ;;                      (format "%s" "XYZ")
    ;;                      'face 'transient-argument)))
    :transient t ;; Do not quit transient state
    ;; :key "p"
    (interactive)
    (symbol-overlay-jump-prev)
    )
  (transient-define-prefix efs/symbol-overlay-transient ()
    "Symbol overlay transient state"
    ;; :transient-suffix 'transient--do-stay ;; Do not quit transient state automatically
    ["Symbol overlay transient state"
     :class transient-columns
     ["Symbol navigation"
      ("n" efs--so-jump-next)
      ("N" efs--so-jump-prev)]
     ["All symbols"
      ("f" symbol-overlay-switch-forward :transient t :description "switch symbol forward")
      ("F" symbol-overlay-switch-backward :transient t :description "switch symbol backwards")
      ("o" symbol-overlay-put :transient t :description "toggle overlay") ;; TODO: Select random face when calling this
      ("O" symbol-overlay-remove-all :transient t :description "remove all overlays")]
     ["Scope"
      ("t" symbol-overlay-toggle-in-scope :transient t :description "scope")
      ("z" recenter-top-bottom :transient t :description "recenter")]
     ["Actions"
      ;; ("p" "print arguments" efs--so-suffix-print-args)
      ;; ("u" "symbol face" efs--so-symbol-face)
      ;; ("x" "xxx" efs--so-random-init-infix) 
      ;; ("-y" "yyy" efs--so-random-init-infix) 
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
  (spacemacs-leader
   "s o" '(efs/symbol-overlay-put :which-key "toggle symbol overlay")
   "s O" '(symbol-overlay-remove-all :which-key "remove symbol overlays")
   "t o" '(symbol-overlay-mode :which-key "symbol overlay mode")
   )
  )

(use-package transient
  :demand t
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
)

(use-package lispyville
  :hook (lispy-mode . lispyville-mode))

;; Make gc pauses faster by decreasing the threshold.
(setq gc-cons-threshold (* 2 1000 1000))

;; Settings for horizontal/vertical scrolling
(setq scroll-margin     5              ;; Set top/bottom scroll margin in number of lines
      scroll-conservatively 1          ;; Set lines to top/bottom scroll
      hscroll-margin    15             ;; Set horizontal scroll margin in number of characters
      hscroll-step      1
      auto-hscroll-mode 'current-line) ;; Scroll horizontally on the selected line only (Emacs version 26.1 or larger)

;; Highlight text, press "S-<delimiter>" to surround text with delimiters.
;; Use "(" to include spaces around delimiters, use ")" for no spaces.
(use-package evil-surround
  :demand t
  :after evil
  :config
  (global-evil-surround-mode 1)
  )

(use-package treemacs
  :demand t
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
  (spacemacs-leader
   "0" '(treemacs-select-window :which-key "treemacs window")
   "f t" '(treemacs :which-key "treemacs")
   )
  )

(use-package treemacs-all-the-icons
  :demand t
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

;; Must be last?
(elpaca-process-queues)

;; TODO: consult-yank-from-kill-ring should show line shifts
;; TODO: all-the-icons connecting to githubusercontent
;; TODO: occur in embark
;; TODO: eglot
;; TODO: matching parenthesies
;; TODO: Try replacing most :demand with :ensure
;; TODO: Rewrite elpaca hydra to transient
