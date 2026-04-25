;;; efs-org.el --- Org, Org Roam, Org Modern, Org Projectile -*- lexical-binding: t; -*-

(defun efs--org-mode-setup ()
  (org-indent-mode 0)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(require 'general)
(use-package org
  :after general
  :defer nil
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
  ;; Define org agenda groups based on priority
  (setq org-agenda-custom-commands
        '(("x" "A better agenda view"
           ((todo "DOING"
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "Doing:")))
           (tags "PRIORITY=\"A\""
                  ((org-agenda-skip-function '(org-agenda-skip-entry-if 'todo 'done))
                   (org-agenda-overriding-header "High-priority:")))
            (alltodo "")
            (agenda "")
            ))))
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
           :unnarrowed nil))))

(use-package org-roam-ui
  :ensure t
  :after org
  :config
  (efs-leader
   "a o r u" '(org-roam-ui-open :wk "org-roam-ui-open")))

(unless (version<= emacs-version "28") ;; 11-10-2023: Don't use org-projectile on Emacs 27.1 as it doesn't compile
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
  (setq org-modern-todo-faces
          '(
            ("TODO"     :foreground "#b7742f" :background "#292b2e" :weight bold)
            ("DOING"    :foreground "yellow"  :background "#292b2e" :weight bold)
            ("WAITING"  :foreground "#00c1ff" :background "#292b2e" :weight normal)
            ("CANCELED" :foreground "#686868" :background "#292b2e" :weight bold)
            ("DONE"     :foreground "#686868" :background "#292b2e" :weight bold)
            ))
  ;; Enable org-modern globally
  (global-org-modern-mode)
  (add-hook 'org-mode-hook 'org-modern-mode)
  (add-hook 'org-agenda-finalize-hook #'org-modern-agenda))

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

;; Use org-download to insert images in org files
(use-package org-download
  :after org
  )

;; Org keybindings - defined after org loads to ensure efs-leader exists
(with-eval-after-load 'org
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
   "a o a" '(org-agenda :wk "agenda")
   "a o c" '(org-capture :wk "capture")))

(provide 'efs-org)
;;; efs-org.el ends here
