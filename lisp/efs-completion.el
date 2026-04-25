;;; efs-completion.el --- Vertico stack, corfu, cape, helpful -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Vertico completion framework
(use-package vertico
  :init (vertico-mode)
  :config
  (setq vertico-count 15)
  (setq vertico-resize t)
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
  ;; Emacs 28 and newer: Hide commands in M-x which do not work in the current
  ;; mode.  Vertico commands are hidden in normal buffers. This setting is
  ;; useful beyond Vertico.
  (setq read-extended-command-predicate #'command-completion-default-include-p)
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
  (completion-styles '(orderless basic))
  ;;(completion-styles '(orderless-fast basic))
  (completion-category-defaults nil)
  (completion-category-overrides '((file (styles partial-completion))))
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
  :demand t

  ;; Enable automatic preview at point in the *Completions* buffer. This is
  ;; relevant when you use the default completion UI.
  :hook (completion-list-mode . consult-preview-at-point-mode)

  :init
  ;; Configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0.5
        register-preview-function #'consult-register-format)

  ;; Tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref)

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
   consult-theme :preview-key '(:debounce 0.2 any)
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult-source-bookmark consult-source-file-register
   consult-source-recent-file consult-source-project-recent-file
   ;; :preview-key "M-."
   :preview-key '(:debounce 0.4 any))

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
)

;; Icons for corfu completion buffer
(use-package nerd-icons-corfu
  :after corfu
  :ensure (:host github :repo "LuigiPiucco/nerd-icons-corfu")
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package all-the-icons
  :ensure t
  :defer t)

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

(provide 'efs-completion)
;;; efs-completion.el ends here
