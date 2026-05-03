;;; efs-bootstrap.el --- Elpaca bootstrap, use-package, diminish, delight -*- lexical-binding: t; -*-

;; Package management
;; elpaca
(defvar elpaca-installer-version 0.12)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-sources-directory (expand-file-name "sources/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1 :inherit ignore
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca-activate)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-sources-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (<= emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let* ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                  ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                  ,@(when-let* ((depth (plist-get order :depth)))
                                                      (list (format "--depth=%d" depth) "--no-single-branch"))
                                                  ,(plist-get order :repo) ,repo))))
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
    (let ((load-source-file-function nil)) (load "./elpaca-autoloads"))))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Override built-in compat (Emacs 30 ships 30.x; some packages require >= 31)
;; Remove from package--builtin-versions so elpaca installs the ELPA version (31.x).
(setq package--builtin-versions
      (assq-delete-all 'compat package--builtin-versions))
(elpaca compat)

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode)
  ;; Assume :elpaca t unless otherwise specified.
  (setq use-package-always-ensure t))

;; Hack for transient requiring seq >= 2.24
;(defun +elpaca-unload-seq (e) "Unload seq before continuing the elpaca build, then continue to build the recipe E."
;  (and (featurep 'seq) (unload-feature 'seq t))
;  (elpaca--continue-build e))
;(elpaca `(seq :build ,(append (butlast (if (file-exists-p (expand-file-name "seq" elpaca-builds-directory))
;                                          elpaca--pre-built-steps
;                                        elpaca-build-steps))
;                             (list '+elpaca-unload-seq 'elpaca--activate-package))))

;; Block until current queue processed.
(elpaca-wait)

;; Install packages like this:
;; (use-package evil :demand t)
;; Expands to: (elpaca evil (use-package evil :demand t))

(use-package diminish
  :ensure t
  :defer t
  :commands (diminish)
  :config
  ;; Diminish some common minor modes
  (diminish 'visual-line-mode)
  (diminish 'auto-revert-mode)
  (diminish 'eldoc-mode)
  (diminish 'buffer-face-mode)
  (diminish 'hs-minor-mode)
  (diminish 'vhdl-hs-minor-mode)
)

(use-package delight
  :defer t)

(use-package emacs
  :ensure nil
  :demand t
  :init
  (defun prot/keyboard-quit-dwim ()
    "Do-What-I-Mean behaviour for a general `keyboard-quit'.

The generic `keyboard-quit' does not do the expected thing when
the minibuffer is open.  Whereas we want it to close the
minibuffer, even without explicitly focusing it.

The DWIM behaviour of this command is as follows:

- When the region is active, disable it.
- When a minibuffer is open, but not focused, close the minibuffer.
- When the Completions buffer is selected, close it.
- In every other case use the regular `keyboard-quit'."
    (interactive)
    (cond
     ((region-active-p)
      (keyboard-quit))
     ((derived-mode-p 'completion-list-mode)
      (delete-completion-window))
     ((> (minibuffer-depth) 0)
      (abort-recursive-edit))
     (t
      (keyboard-quit))))
  :bind
  ("C-g" . prot/keyboard-quit-dwim)
  :config
  ;; Do not jump to the current line in `*occur*' buffers.  The reason
  ;; is that you are already on that line: you want to do `occur' to
  ;; get more than that (and, presumably, to do something with the
  ;; results such as to edit them with `occur-edit-mode').
  (setq list-matching-lines-jump-to-current-line nil)
  (setq completion-category-defaults nil))

(elpaca-wait)

(provide 'efs-bootstrap)
;;; efs-bootstrap.el ends here
