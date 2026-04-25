;;; efs-ui.el --- Globals, GC, encodings, fonts, scrolling, theme, modeline -*- lexical-binding: t; -*-

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
;; and restore it to a more reasonable post-startup default. gcmh (below)
;; takes over from there with adaptive idle/active GC.
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 16 1024 1024))))

;; Adaptive GC: low threshold during interaction, high while idle.
(use-package gcmh
  :ensure t
  :diminish gcmh-mode
  :init (gcmh-mode 1)
  :config
  (setq gcmh-high-cons-threshold (* 128 1024 1024)))

;; Prevent warning buffer from stealing focus on every new warning
(setq warning-minimum-level :warning)

(setq-default
   debug-on-error nil
   ;; Don't show cursors in inactive windows
   cursor-in-non-selected-windows nil
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
(fringe-mode nil)    ; When nil or default use 8 pixel fringe

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

;; Window configuration
(setq switch-to-buffer-obey-display-actions t)
(setq switch-to-buffer-in-dedicated-window 'pop) ; Opening buffer in dedicated window causes it to pop up somewhere else instead of an error

;; Settings for horizontal/vertical scrolling
(setq scroll-margin     5              ;; Set top/bottom scroll margin in number of lines
      scroll-conservatively 101        ;; Set lines to top/bottom scroll
      hscroll-margin    15             ;; Set horizontal scroll margin in number of characters
      hscroll-step      1
      auto-hscroll-mode 'current-line) ;; Scroll horizontally on the selected line only (Emacs version 26.1 or larger)

;; Smooth pixel-precise scrolling (built-in, Emacs 29+)
(when (fboundp 'pixel-scroll-precision-mode)
  (pixel-scroll-precision-mode 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Theme

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

  ;; Define custom spaceline segment for maximized window indicator
  (spaceline-define-segment efs-maximized-indicator
    "Show maximized indicator when window is maximized"
    (when (and (boundp 'efs-maximized-mode) efs-maximized-mode)
      (propertize " Maximized " 'face '(:foreground "yellow" :weight bold))))

  ;; First load the standard spacemacs theme
  (spaceline-spacemacs-theme)

  ;; Manually compile a modeline that includes the custom segment
  (spaceline-compile
    ;; Modeline ID
    'main
    ;; Left side segments
    '(
      (persp-name workspace-number window-number)  ; Window/workspace numbers at the beginning
      (buffer-modified buffer-size)
      (buffer-id remote-host)
      (major-mode :when active)
      ((flycheck-error flycheck-warning flycheck-info) :when active)
      efs-maximized-indicator
      (minor-modes :when active)
      (treesit-inspect :when active)
      (process :when active)
      (version-control :when active))
    ;; Right side segments
    '((selection-info :when active)
      ((buffer-encoding-abbrev
        point-position
        line-column)
       :separator " | "
       :priority 96)
      (global :when active)
      hud))

  ;; Force the modeline to use our compiled version
  (setq-default mode-line-format '("%e" (:eval (spaceline-ml-main))))

  (setq spaceline-highlight-face-func 'spaceline-highlight-face-evil-state) ; Color according to evil state
  (setq powerline-default-separator 'arrow)
  (setq spaceline-workspace-numbers-unicode t) ; Get unicode numbers when using window-numbering-mode
  (setq spaceline-window-numbers-unicode t) ; Get unicode numbers when using eyebrowse-mode
)

(provide 'efs-ui)
;;; efs-ui.el ends here
