;;; efs-vc.el --- Magit, git-gutter, diff-hl -*- lexical-binding: t; -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Version control config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package magit
  :ensure t
  :custom
  (magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)
  :config
  ;; Simple display buffer function for magit
  (defun efs--display-buffer-full-window (buffer alist)
    "Display BUFFER in a full window. ALIST is the display-buffer's ALIST."
    (let ((window (display-buffer-use-some-window buffer alist)))
      (when window
        (delete-other-windows window)
        window)))
  ;; NOTE 11-10-2023: Need to define this expected function that is only introduced in Emacs 29 (https://github.com/magit/magit/issues/5011)
  (defun seq-keep (function sequence)
    "Apply FUNCTION to SEQUENCE and return the list of all the non-nil results."
    (delq nil (seq-map function sequence)))
  (defun efs--magit-status ()
    "Version of magit-status that opens in full frame and restores previous window config on quit."
    (interactive)
    ;; Save window configuration using unified system
    (unless (bound-and-true-p winner-mode)
      (winner-mode 1))
    ;; Only save if not already saved
    (unless (efs--has-window-config-p 'magit-status)
      (efs--save-window-config 'magit-status))
    ;; Tell magit-status buffer to open in full frame
    (let ((display-buffer-alist
           '(("^magit: "
              (efs--display-buffer-full-window)
              ))))
      (magit-status)))
  (defun efs--restore-magit-windows (orig-fun &rest args)
    "Restore window configuration to before magit-status was opened. Only restore if buffer is magit-status-mode."
    (interactive)
    (if (and (efs--has-window-config-p 'magit-status)
             (string-equal "magit-status-mode" (symbol-name major-mode)))
        (efs--restore-window-config 'magit-status t) ;; Restore and delete
      (apply orig-fun args)))
  (advice-add 'magit-mode-quit-window :around #'efs--restore-magit-windows)

  (defun efs--magit-status-active-p ()
    "Check if a magit-status buffer is currently visible in any window."
    (seq-some (lambda (window)
                (with-current-buffer (window-buffer window)
                  (derived-mode-p 'magit-status-mode)))
              (window-list)))

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
  (defvar efs--git-gutter-context-lines 2
    "Number of context lines to show before and after modified hunks.")

  (defun efs--get-modified-ranges-with-context ()
    "Get all modified line ranges with context lines added."
    (when git-gutter:diffinfos
      (let ((ranges (mapcar (lambda (hunk)
                              (cons (max 1 (- (git-gutter-hunk-start-line hunk)
                                             efs--git-gutter-context-lines))
                                    (+ (git-gutter-hunk-end-line hunk)
                                       efs--git-gutter-context-lines)))
                            git-gutter:diffinfos)))
        ;; Merge overlapping ranges
        (let ((sorted-ranges (sort ranges (lambda (a b) (< (car a) (car b)))))
              (merged-ranges nil))
          (dolist (range sorted-ranges)
            (if (and merged-ranges
                     (<= (car range) (1+ (cdar merged-ranges))))
                ;; Merge with previous range
                (setcdr (car merged-ranges) (max (cdr range) (cdar merged-ranges)))
              ;; Add as new range
              (push range merged-ranges)))
          (nreverse merged-ranges)))))

  (defun efs--create-invisible-overlay (start end)
    "Create an overlay that makes the region between START and END invisible."
    (let ((overlay (make-overlay start end)))
      (overlay-put overlay 'invisible t)
      (overlay-put overlay 'evaporate t)
      overlay))

  (defun efs--create-horizontal-bar-overlay (start end line-count)
    "Create an overlay that displays a horizontal bar between START and END.
LINE-COUNT is the number of lines being hidden."
    (let* ((overlay (make-overlay start end))
           (window-width (window-width))
           (bar-string (concat " " (make-string (- window-width 2) ?─) " \n")))
      (overlay-put overlay 'display bar-string)
      (overlay-put overlay 'face 'efs--horizontal-rule)
      (overlay-put overlay 'evaporate t)
      overlay))

  (defun efs--toggle-modified-lines-visibility ()
    "Toggle hiding/showing git-gutter unmodified lines with extra context lines."
    (interactive)
    (if efs--git-gutter-overlays
        ;; Remove all overlays
        (progn
          (mapc 'delete-overlay efs--git-gutter-overlays)
          (setq efs--git-gutter-overlays nil)
          (message "Showing all lines"))
      ;; Create overlays for unmodified regions
      (if (not git-gutter:diffinfos)
          (message "No git modifications in this buffer")
        (let ((modified-ranges (efs--get-modified-ranges-with-context))
              (overlays nil)
              (last-end 1))
          (save-excursion
            ;; Create overlays for gaps between modified regions
            (dolist (range modified-ranges)
              (let ((range-start (car range))
                    (range-end (cdr range)))
                ;; Create overlay for unmodified region before this range
                (when (< last-end range-start)
                  (goto-char (point-min))
                  (forward-line (1- last-end))
                  (let ((start-pos (point))
                        (hidden-lines (- range-start last-end)))
                    (forward-line hidden-lines)
                    (when (> (point) start-pos)
                      (if (= hidden-lines 1)
                          ;; Single line - just make it invisible
                          (push (efs--create-invisible-overlay start-pos (point))
                                overlays)
                        ;; Multiple lines - show horizontal bar
                        (push (efs--create-horizontal-bar-overlay start-pos (point) hidden-lines)
                              overlays)))))
                (setq last-end (1+ range-end))))
            ;; Handle remaining lines after last modified range
            (goto-char (point-min))
            (forward-line (1- last-end))
            (when (< (point) (point-max))
              (let ((start-pos (point))
                    (hidden-lines (count-lines (point) (point-max))))
                (if (= hidden-lines 1)
                    ;; Single line - just make it invisible
                    (push (efs--create-invisible-overlay start-pos (point-max))
                          overlays)
                  ;; Multiple lines - show horizontal bar
                  (push (efs--create-horizontal-bar-overlay start-pos (point-max) hidden-lines)
                        overlays))))
          (setq efs--git-gutter-overlays overlays)
          (message "Hiding unmodified lines (showing %d modified regions with %d context lines)"
                   (length modified-ranges) efs--git-gutter-context-lines))))))


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
    )
  ) ;; End of use-package git-gutter

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

(provide 'efs-vc)
;;; efs-vc.el ends here
