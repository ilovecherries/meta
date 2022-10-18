;;; blank-line-indent -- Overrides default indenter to indent blank lines.
;;; Author: 12Me21
;;; Date: September 16th, 2022
;;; Commentary:
;;; Code:

(require 'electric)

(defcustom indent-blank-lines nil
  "if nil, blank lines will not be indented"
  :type 'boolean
  :group 'indent)
(make-variable-buffer-local 'indent-blank-lines)
(put 'indent-blank-lines 'safe-local-variable 'booleanp)

(defun indent-region-line-by-line (start end)
  (save-excursion
    (setq end (copy-marker end))
    (goto-char start)
    (let ((pr (unless (minibufferp)
                (make-progress-reporter "Indenting region..." (point) end))))
      (while (< (point) end)
        (if (or indent-blank-lines (not (and (bolp) (eolp))))
				(indent-according-to-mode))
        (forward-line 1)
        (and pr (progress-reporter-update pr (point))))
      (and pr (progress-reporter-done pr))
      (move-marker end nil))))

(defun electric-indent-post-self-insert-function ()
  "Function that `electric-indent-mode' adds to `post-self-insert-hook'.
This indents if the hook `electric-indent-functions' returns non-nil,
or if a member of `electric-indent-chars' was typed; but not in a string
or comment."
  ;; FIXME: This reindents the current line, but what we really want instead is
  ;; to reindent the whole affected text.  That's the current line for simple
  ;; cases, but not all cases.  We do take care of the newline case in an
  ;; ad-hoc fashion, but there are still missing cases such as the case of
  ;; electric-pair-mode wrapping a region with a pair of parens.
  ;; There might be a way to get it working by analyzing buffer-undo-list, but
  ;; it looks challenging.
  (let (pos)
    (when (and
           electric-indent-mode
           ;; Don't reindent while inserting spaces at beginning of line.
           (or (not (memq last-command-event '(?\s ?\t)))
               (save-excursion (skip-chars-backward " \t") (not (bolp))))
           (setq pos (electric--after-char-pos))
           (save-excursion
             (goto-char pos)
             (let ((act (or (run-hook-with-args-until-success
                             'electric-indent-functions
                             last-command-event)
                            (memq last-command-event electric-indent-chars))))
               (not
                (or (memq act '(nil no-indent))
                    ;; In a string or comment.
                    (unless (eq act 'do-indent) (nth 8 (syntax-ppss))))))))
      ;; If we error during indent, silently give up since this is an
      ;; automatic action that the user didn't explicitly request.
      ;; But we don't want to suppress errors from elsewhere in *this*
      ;; function, hence the `condition-case' and `throw' (Bug#18764).
      (catch 'indent-error
        ;; For newline, we want to reindent both lines and basically
        ;; behave like reindent-then-newline-and-indent (whose code we
        ;; hence copied).
        (let ((at-newline (<= pos (line-beginning-position))))
          (when at-newline
            (let ((before (copy-marker (1- pos) t)))
              (save-excursion
                (unless
                    (or (memq indent-line-function
                              electric-indent-functions-without-reindent)
                        electric-indent-inhibit)
                  ;; Don't reindent the previous line if the
                  ;; indentation function is not a real one.
                  (goto-char before)
                  (condition-case-unless-debug ()
                      (indent-according-to-mode)
                    (error (throw 'indent-error nil))))
                (unless (eq electric-indent-inhibit 'electric-layout-mode)
                  ;; Unless we're operating under
                  ;; `electric-layout-mode' (Bug#35254), the goal here
                  ;; will be to remove the trailing whitespace after
                  ;; reindentation of the previous line because that
                  ;; may have (re)introduced it.
                  (goto-char before)
                  ;; We were at EOL in marker `before' before the call
                  ;; to `indent-according-to-mode' but after we may
                  ;; not be (Bug#15767).
                  (when (and (eolp) (not indent-blank-lines))
                    (delete-horizontal-space t))))))
          (unless (and electric-indent-inhibit
                       (not at-newline))
            (condition-case-unless-debug ()
                (indent-according-to-mode)
              (error (throw 'indent-error nil)))))))))

(provide 'blank-line-indent)

;;; blank-line-indent.el ends here
