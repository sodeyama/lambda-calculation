;; beta-mode
(defun beta-mode ()
  (interactive)
  (setq major-mode 'beta-mode
    mode-name "beta mode")
  (setq beta-local-map (make-keymap))
  (define-key beta-local-map "\C-cb" 'beta-reduction)
  (use-local-map beta-local-map))

(defun beta-reduction ()
  (interactive)
  (save-excursion
    (let ((in_str (buffer-string)))
      (start-process "beta-process" "*beta-buffer*" "/home/sode/git/lambda-calculation/beta" in_str)
      (let ((buffer (get-buffer "*beta-buffer*")))
        (switch-to-buffer-other-window buffer)
        (goto-char (point-max))))))

