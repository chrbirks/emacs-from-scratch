;;; efs-treesit.el --- tree-sitter setup, treesit-fold, treesit-auto -*- lexical-binding: t; -*-

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tree-sitter setup
(when (>= emacs-major-version 29)
  (setq treesit-language-source-alist
        '((bash       "https://github.com/tree-sitter/tree-sitter-bash")
          (elisp      "https://github.com/Wilfred/tree-sitter-elisp")
          (go         "https://github.com/tree-sitter/tree-sitter-go")
          (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
          (json       "https://github.com/tree-sitter/tree-sitter-json")
          (make       "https://github.com/alemuller/tree-sitter-make")
          (markdown   "https://github.com/ikatyang/tree-sitter-markdown")
          (python     "https://github.com/tree-sitter/tree-sitter-python")
          (rust       "https://github.com/tree-sitter/tree-sitter-rust")
          (toml       "https://github.com/tree-sitter/tree-sitter-toml")
          (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
          (verilog    "https://github.com/tree-sitter/tree-sitter-verilog")
          ;; verilog-ts-mode (gmlarumbe) uses the SystemVerilog grammar, not the
          ;; plain Verilog one. Install both with `SPC a t i'.
          (systemverilog "https://github.com/gmlarumbe/tree-sitter-systemverilog")
          (vhdl       "https://github.com/alemuller/tree-sitter-vhdl")
          (yaml       "https://github.com/ikatyang/tree-sitter-yaml")))
  ;; Enable tree-sitter modes selectively for better control
  ;; We'll configure HDL modes separately to work alongside LSP
  (setq major-mode-remap-alist
        '((bash-mode . bash-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (python-mode . python-ts-mode)
          (yaml-mode . yaml-ts-mode)
          (vhdl-mode . vhdl-ts-mode)
          ))

  ;; Helper function to install tree-sitter grammars
  (defun efs-install-tree-sitter-grammars ()
    "Install all configured tree-sitter grammars."
    (interactive)
    (mapc #'treesit-install-language-grammar
          '(bash elisp go javascript json make markdown python rust toml typescript verilog systemverilog vhdl yaml))
    (message "Tree-sitter grammars installation initiated. Check *Messages* for details."))

  ;; Check which grammars are available
  (defun efs-list-tree-sitter-grammars ()
    "List available and missing tree-sitter grammars."
    (interactive)
    (let ((languages '(bash elisp go javascript json make markdown python rust toml typescript verilog systemverilog vhdl yaml))
          (available '())
          (missing '()))
      (dolist (lang languages)
        (if (treesit-language-available-p lang)
            (push lang available)
          (push lang missing)))
      (message "Available grammars: %s\nMissing grammars: %s"
               (mapconcat #'symbol-name (nreverse available) ", ")
               (mapconcat #'symbol-name (nreverse missing) ", "))))
  )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Tree-sitter enhanced packages
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; treesit-fold: Code folding using tree-sitter
(use-package treesit-fold
  :ensure (:type git :host github :repo "emacs-tree-sitter/treesit-fold")
  :after evil
  :diminish treesit-fold-mode
  :config
  ;; Require indicators module
  (require 'treesit-fold-indicators)

  ;; Enable treesit-fold indicators in the fringe
  (setq treesit-fold-indicators-fringe 'left-fringe)

  ;; Set priority for fringe overlay (default is 30)
  (setq treesit-fold-indicators-priority 100)

  ;; Add support for non-ts modes by creating parsers
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (when (treesit-language-available-p 'elisp)
                (treesit-parser-create 'elisp))))

  ;; Ensure VHDL files use vhdl-ts-mode and have parser
  (add-hook 'vhdl-mode-hook
            (lambda ()
              (when (and (treesit-language-available-p 'vhdl)
                         (not (eq major-mode 'vhdl-ts-mode)))
                (vhdl-ts-mode))))

  ;; Create parser for vhdl-ts-mode if needed
  (add-hook 'vhdl-ts-mode-hook
            (lambda ()
              (unless (treesit-parser-list)
                (when (treesit-language-available-p 'vhdl)
                  (treesit-parser-create 'vhdl)))))

  ;; Add file associations for VHDL to use ts-mode directly
  (add-to-list 'auto-mode-alist '("\\.vhd\\'" . vhdl-ts-mode))
  (add-to-list 'auto-mode-alist '("\\.vhdl\\'" . vhdl-ts-mode))

  ;; Debug function to check treesit-fold status
  (defun efs--debug-treesit-fold ()
    "Debug treesit-fold status in current buffer."
    (interactive)
    (message "Major mode: %s\nTreesit parser: %s\nTreesit-fold ready: %s\nTreesit-fold mode: %s\nTreesit-fold indicators: %s"
             major-mode
             (treesit-parser-list)
             (treesit-fold-ready-p)
             treesit-fold-mode
             treesit-fold-indicators-mode))

  ;; ;; Add more VHDL folding patterns
  ;; (with-eval-after-load 'treesit-fold
  ;;   (defun treesit-fold-range-vhdl-entity (node offset)
  ;;     "Fold VHDL entity declarations."
  ;;     (let* ((port-node (treesit-search-subtree node "port_clause"))
  ;;            (end-node (treesit-search-subtree node "entity_declarative_part\\|end_simple_name"))
  ;;            (beg (if port-node
  ;;                     (treesit-node-start port-node)
  ;;                   (treesit-node-end (treesit-node-child node 0))))
  ;;            (end (if end-node
  ;;                     (treesit-node-start end-node)
  ;;                   (treesit-node-end node))))
  ;;       (when (and beg end (< beg end))
  ;;         (treesit-fold--cons-add (cons beg end) offset))))

  ;;   (defun treesit-fold-range-vhdl-process (node offset)
  ;;     "Fold VHDL process blocks."
  ;;     (let* ((decl-node (treesit-search-subtree node "process_declarative_part"))
  ;;            (begin-node (treesit-search-subtree node "\\(begin\\)"))
  ;;            (beg (if decl-node
  ;;                     (treesit-node-start decl-node)
  ;;                   (when begin-node
  ;;                     (treesit-node-end begin-node))))
  ;;            (end (treesit-node-end node)))
  ;;       (when (and beg end (< beg end))
  ;;         (treesit-fold--cons-add (cons beg end) offset))))

  ;;   ;; Add the new folding rules to VHDL modes
  ;;   (dolist (mode '(vhdl-mode vhdl-ts-mode))
  ;;     (let ((current-rules (alist-get mode treesit-fold-range-alist)))
  ;;       (setf (alist-get mode treesit-fold-range-alist)
  ;;             (append current-rules
  ;;                     '((entity_declaration . treesit-fold-range-vhdl-entity)
  ;;                       (process_statement . treesit-fold-range-vhdl-process)))))))

  ;; Enable treesit-fold-mode and indicators in tree-sitter enabled modes
  (dolist (hook '(bash-ts-mode-hook
                  typescript-ts-mode-hook
                  json-ts-mode-hook
                  python-ts-mode-hook
                  yaml-ts-mode-hook
                  vhdl-ts-mode-hook
                  verilog-mode-hook))
    (add-hook hook #'treesit-fold-mode)
    (add-hook hook #'treesit-fold-indicators-mode))

  ;; Evil integration keybindings
  (general-define-key
   :states '(normal visual)
   :keymaps 'treesit-fold-mode-map
   "za" 'treesit-fold-toggle
   "zc" 'treesit-fold-close
   "zo" 'treesit-fold-open
   "zC" 'treesit-fold-close-all
   "zO" 'treesit-fold-open-all
   "zr" 'treesit-fold-open-recursively
   "zR" 'treesit-fold-open-all
   "zm" 'treesit-fold-close-all)
  )

;; treesit-auto: Automatic mode management and grammar installation
(use-package treesit-auto
  :ensure (:type git :host github :repo "renzmann/treesit-auto")
  :config
  ;; Enable global treesit-auto mode
  (global-treesit-auto-mode +1)

  ;; Set fallback behavior when tree-sitter fails
  (setq treesit-auto-install 'prompt)  ;; Prompt to install missing grammars

  ;; Configure additional language mappings
  (setq treesit-auto-langs
        '(bash c cpp css go html javascript json python rust typescript yaml))

  ;; Add custom language recipes if needed
  (with-eval-after-load 'treesit-auto
    ;; Custom configuration for any additional languages
    (treesit-auto-add-to-auto-mode-alist 'all)))

;; ;; evil-textobj-tree-sitter: Tree-sitter powered text objects for Evil mode
;; (use-package evil-textobj-tree-sitter
;;   :ensure (:type git :host github :repo "meain/evil-textobj-tree-sitter"
;;            :files (:defaults "queries" "treesit-queries"))
;;   :after evil
;;   :config
;;   ;; Bind text objects for various code structures
;;   (define-key evil-outer-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.outer"))
;;   (define-key evil-inner-text-objects-map "f" (evil-textobj-tree-sitter-get-textobj "function.inner"))
;;   (define-key evil-outer-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.outer"))
;;   (define-key evil-inner-text-objects-map "c" (evil-textobj-tree-sitter-get-textobj "class.inner"))
;;   (define-key evil-outer-text-objects-map "l" (evil-textobj-tree-sitter-get-textobj "loop.outer"))
;;   (define-key evil-inner-text-objects-map "l" (evil-textobj-tree-sitter-get-textobj "loop.inner"))
;;   (define-key evil-outer-text-objects-map "o" (evil-textobj-tree-sitter-get-textobj "conditional.outer"))
;;   (define-key evil-inner-text-objects-map "o" (evil-textobj-tree-sitter-get-textobj "conditional.inner"))
;;   (define-key evil-outer-text-objects-map "m" (evil-textobj-tree-sitter-get-textobj "comment.outer"))
;;   (define-key evil-inner-text-objects-map "m" (evil-textobj-tree-sitter-get-textobj "comment.inner"))
;;   (define-key evil-outer-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj "parameter.outer"))
;;   (define-key evil-inner-text-objects-map "a" (evil-textobj-tree-sitter-get-textobj "parameter.inner"))

;;   ;; Navigation between functions/classes
;;   (general-define-key
;;    :states '(normal visual)
;;    :keymaps 'prog-mode-map
;;    "]f" (lambda ()
;;           (interactive)
;;           (evil-textobj-tree-sitter-goto-textobj "function.outer"))
;;    "[f" (lambda ()
;;           (interactive)
;;           (evil-textobj-tree-sitter-goto-textobj "function.outer" t))
;;    "]c" (lambda ()
;;           (interactive)
;;           (evil-textobj-tree-sitter-goto-textobj "class.outer"))
;;    "[c" (lambda ()
;;           (interactive)
;;           (evil-textobj-tree-sitter-goto-textobj "class.outer" t)))

;;   ;; Add custom HDL text objects for Verilog/VHDL
;;   (defun efs--setup-hdl-text-objects ()
;;     "Set up HDL-specific text objects for Verilog and VHDL modes."
;;     (when (or (derived-mode-p 'verilog-mode)
;;               (derived-mode-p 'vhdl-mode)
;;               (derived-mode-p 'vhdl-ts-mode))
;;       ;; Custom text objects for HDL modules/entities
;;       (define-key evil-outer-text-objects-map "M"
;;         (lambda ()
;;           (interactive)
;;           (evil-textobj-tree-sitter-get-textobj
;;            "class.outer"
;;            '((verilog-mode . ((module_declaration) @class))
;;              (vhdl-mode . ((entity_declaration) @class))
;;              (vhdl-ts-mode . ((entity_declaration) @class))))))

;;       ;; Custom text objects for HDL processes/always blocks
;;       (define-key evil-outer-text-objects-map "P"
;;         (lambda ()
;;           (interactive)
;;           (evil-textobj-tree-sitter-get-textobj
;;            "function.outer"
;;            '((verilog-mode . ((always_construct) @function))
;;              (vhdl-mode . ((process_statement) @function))
;;              (vhdl-ts-mode . ((process_statement) @function))))))))

;;   ;; Enable HDL text objects in relevant modes
;;   (add-hook 'verilog-mode-hook #'efs--setup-hdl-text-objects)
;;   (add-hook 'vhdl-mode-hook #'efs--setup-hdl-text-objects)
;;   (add-hook 'vhdl-ts-mode-hook #'efs--setup-hdl-text-objects))

(provide 'efs-treesit)
;;; efs-treesit.el ends here
