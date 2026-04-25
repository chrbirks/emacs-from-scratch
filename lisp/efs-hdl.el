;;; efs-hdl.el --- HDL: Verilog, VHDL, vhdl-ts, TCL helpers -*- lexical-binding: t; -*-

;; Any .do .qsf .qpf and .sdc file should be in tcl-mode
(add-to-list 'auto-mode-alist '("\\.do\\'" . tcl-mode))
(add-to-list 'auto-mode-alist '("\\.sdc\\'" . tcl-mode))
(add-to-list 'auto-mode-alist '("\\.qpf\\'" . tcl-mode))
(add-to-list 'auto-mode-alist '("\\.qsf\\'" . tcl-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Verilog settings
(use-package verilog-mode
  :ensure nil ;; verilog-mode is a native package
  :defer t
  :custom
  ;; LSP setup for verilog-mode
  ;; Disable use of hdl_checker first
  (lsp-clients-verilog-executable nil)
  ;; ;; Configure svlangserver globally (Requires Verilator and Verible).
  ;; ;; Can also be configured on a project basis in a .dir-locals.el
  ;; (lsp-clients-svlangserver-launchConfiguration "verilator -sv --lint-only -Wall -Wno-fatal --assert --cdc")
  ;; (lsp-clients-svlangserver-formatCommand "~/etc/verible/bin/verible-verilog-format")
  ;; (lsp-clients-svlangserver-includeIndexing '["does-not-exist.sv"])
  ;; (lsp-clients-svlangserver-excludeIndexing '["bbs/simulation/**/*.{v,vh,sv,svh}"
  ;;                                             "bbs/work*/**/*.{v,vh,sv,svh}"
  ;;                                             "bbs/design/afu/stratix10/pac_lc/axi_protocol_afu/**/*"
  ;;                                             "bbs/design/afu/stratix10/pac_lc/dummy_afu/**/*"
  ;;                                             "bbs/design/afu/stratix10/pac_lc/eth_afu/**/*"
  ;;                                             "bbs/design/afu/stratix10/pac_lc/hello_afu/**/*"
  ;;                                             "bbs/design/afu/stratix10/pac_lc/hello_afu_interrupt/**/*"
  ;;                                             "bbs/design/afu/stratix10/pac_lc/nlb_afu/**/*"
  ;;                                             "bbs/**/ip/**/*.{v,vh,sv,svh}"])
  ;; '(lsp-clients-svlangserver-workspace-additional-dirs '["/mnt/storage/projects/intel/ofs-platform-afu-bbb/"])
  ;; (lsp-clients-svlangserver-lintOnUnsaved t)
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
        ;; verilog-linter "verilator -sv --lint-only -Wall --cdc --default-language 1800-2012"
        ;; verilog-linter "verilator -sv --lint-only -Wall --cdc +1800-2012ext+sv"
        )
  ;; Use 'verilator_bin' instead of 'verilator' which throws errors
  (setq
   ;; Include paths for project-specific headers (customize as needed)
   ;; flycheck-verilator-include-path '("./include" "../common/include")
   flycheck-verilog-verilator-executable "verilator_bin")

  ;; Any files that end in .v, .dv, .pv or .sv should be in verilog mode
  (add-to-list 'auto-mode-alist '("\\.[dsp]?va?h\\'" . verilog-mode))
  ;; Set up svls LSP server
  ;; (require 'lsp)
  ;; (lsp-register-client
  ;;  (make-lsp-client :new-connection (lsp-stdio-connection '("svls"))
  ;;                   :major-modes '(verilog-mode)
  ;;                   :priority -1
  ;;                   ))
  ;; :hook (verilog-mode . (lambda()
  ;;                         (lsp)
  ;;                         (flycheck-mode t)
  ;;                         (add-to-list 'lsp-language-id-configuration '(verilog-mode . "verilog"))))

  ;; Set up verible-verilog-ls LSP server
  (require 'lsp-mode)
  (add-to-list 'lsp-language-id-configuration '(verilog-mode . "verilog"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "verible-verilog-ls")
                    :major-modes '(verilog-mode)
                    :server-id 'verible-ls))

  (add-hook 'verilog-mode-hook 'lsp)

  )

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; tree-sitter for Verilog
;; Configure tree-sitter for enhanced syntax highlighting while keeping LSP for intelligence
;; (use-package verilog-mode
;;   :hook
;;   ;; Enable tree-sitter for better syntax highlighting
;;   ((verilog-mode . (lambda ()
;;                      (when (and (fboundp 'treesit-available-p)
;;                                (treesit-available-p)
;;                                (treesit-language-available-p 'verilog))
;;                        ;; Use tree-sitter for font-lock (syntax highlighting)
;;                        (treesit-parser-create 'verilog)
;;                        (setq-local treesit-font-lock-feature-list
;;                                    '((comment definition)
;;                                      (keyword string type)
;;                                      (assignment attribute constant number)
;;                                      (bracket delimiter error operator)))
;;                        (setq-local treesit-font-lock-level 3)
;;                        (treesit-major-mode-setup))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom vhdl-mode settings
(use-package vhdl-mode
  :ensure nil ;; vhdl-mode is a native package
  :defer t
  :hook (vhdl-mode . lsp-deferred)
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
        ;; flycheck-ghdl-workdir "~/projects/vhdl"  ; Set project-specific work directory if needed
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

;; VHDL Tree-Sitter Mode
;; Provides enhanced syntax highlighting and parsing for VHDL files
;; Choose one of the three configuration options below:

;; OPTION 1: Theme-based (Recommended) - Uses your color theme's defaults
;; Uncomment this for consistent colors with your theme
;; (use-package vhdl-ts-mode
;;   :config
;;   (setq vhdl-ts-indent-level 2))

;; OPTION 2: Minimal custom faces - Only highlights key structural elements
;; Uncomment this block and comment out Option 1 if you want subtle enhancements
;; (use-package vhdl-ts-mode
;;   :custom-face
;;   (vhdl-ts-font-lock-entity-face           ((t (:inherit font-lock-type-face :weight bold))))
;;   (vhdl-ts-font-lock-instance-face         ((t (:inherit font-lock-variable-name-face :weight bold))))
;;   (vhdl-ts-font-lock-port-connection-face  ((t (:inherit font-lock-constant-face))))
;;   (vhdl-ts-font-lock-translate-off-face    ((t (:inherit shadow :background unspecified))))
;;   :config
;;   (setq vhdl-ts-indent-level 2))

;; OPTION 3: Full custom faces - Maximum visual distinction (original configuration)
;; Uncomment this block and comment out Option 1 if you want all custom colors
(use-package vhdl-ts-mode
  :custom-face
  (vhdl-ts-font-lock-then-face             ((t (:foreground "#4c8dc8"))))
  (vhdl-ts-font-lock-punctuation-face      ((t (:foreground "burlywood"))))
  (vhdl-ts-font-lock-operator-face         ((t (:inherit 'vhdl-ts-font-lock-punctuation-face))))
  (vhdl-ts-font-lock-parenthesis-face      ((t (:foreground "dark goldenrod"))))
  (vhdl-ts-font-lock-brackets-content-face ((t (:foreground "yellow green"))))
  (vhdl-ts-font-lock-port-connection-face  ((t (:foreground "bisque2"))))
  (vhdl-ts-font-lock-entity-face           ((t (:foreground "green1"))))
  (vhdl-ts-font-lock-instance-face         ((t (:foreground "medium spring green"))))
  (vhdl-ts-font-lock-instance-lib-face     ((t (:foreground "gray70"))))
  (vhdl-ts-font-lock-translate-off-face    ((t (:background "gray20"))))
  :config
  (setq vhdl-ts-indent-level 2))

(provide 'efs-hdl)
;;; efs-hdl.el ends here
