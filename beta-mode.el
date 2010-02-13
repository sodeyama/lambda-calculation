;; beta-mode
(defvar betabin "/home/sode/git/lambda-calculation/beta")

(defun beta-mode ()
  (interactive)
  (setq major-mode 'beta-mode
    mode-name "beta mode")
  (setq beta-local-map (make-keymap))
  (define-key beta-local-map "\C-cb" 'beta-reduction)
  (define-key beta-local-map "\C-cn" 'beta-reduction-show-native-tree)
  (use-local-map beta-local-map))

(defun beta-reduction ()
  (interactive)
  (beta-reduction-inner "false"))

(defun beta-reduction-show-native-tree ()
  (interactive)
  (beta-reduction-inner "true"))

(defun beta-reduction-inner (is_tree)
  (save-excursion
    (let ((in_str (buffer-string)))
      (start-process "beta-process" "*beta-buffer*" betabin in_str is_tree)
      (let ((buffer (get-buffer "*beta-buffer*")))
        (switch-to-buffer-other-window buffer)
        (goto-char (point-max))))))




