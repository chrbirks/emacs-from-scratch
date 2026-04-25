;;; efs-projects.el --- Projectile, ripgrep, compile-multi, envrc -*- lexical-binding: t; -*-

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
                  "GPATH"
                  ;; FPGA toolchain logs / state files
                  "vivado*.jou"
                  "vivado*.log"
                  "webtalk*.log"
                  "xsim.log"
                  "hs_err_pid*"
                  "*.qsf.bak"
                  "*.wlf"
                  "*.vstf")
                projectile-globally-ignored-files)
        projectile-globally-ignored-directories
        (append '(".emacs.d"
                  ;; Vivado / Vitis
                  ".Xil"  "xsim.dir"  ".cache"  ".srcs"  ".sim"
                  ;; Quartus / ModelSim
                  "db"  "qdb"  "incremental_db"  "output_files"
                  "simulation"  "work"  "transcript"
                  ;; cocotb / pytest
                  "sim_build"  "__pycache__"  ".pytest_cache"
                  ;; Generic
                  "build"  "dist"  ".direnv"  "node_modules")
                projectile-globally-ignored-directories)
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
   "p c" '(compile-multi :wk "compile-multi")
   "p P" '(projectile-commander :wk "projectile-commander")
   "p f" '(projectile-find-file :wk "find-file")
   "p m" '(projectile-command-map :wk "projectile-command-map")
   )
)

(use-package rg
  :ensure t
  :config
  (rg-enable-menu)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; compile-multi: per-project run-this-command picker.
;; Targets are declared in each project's .dir-locals.el via
;; compile-multi-config — e.g. verilator-lint, verible-lint, vivado-synth,
;; ghdl-elab. See https://github.com/mohkale/compile-multi

(use-package compile-multi
  :ensure t
  :commands (compile-multi))

(use-package consult-compile-multi
  :ensure t
  :after (consult compile-multi)
  :demand t
  :config (consult-compile-multi-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; envrc: load per-project .envrc (direnv) into buffer-local environment.
;; Critical for FPGA work where each project may pull a different toolchain
;; (Quartus 20.4 vs 23.x, different Vivado releases, distinct Python venvs).

(use-package envrc
  :ensure t
  :hook (after-init . envrc-global-mode))

(provide 'efs-projects)
;;; efs-projects.el ends here
