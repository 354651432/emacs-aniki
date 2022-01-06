(defun aniki-trans()
  (interactive)
  (shell-command
   (format
	"trans %s"
	(if (region-active-p)
		(buffer-substring-no-properties (region-beginning) (region-end))
	  (thing-at-point 'word)))))

(defun aniki-ctrl-w()
  (interactive)
  (if (region-active-p)
	  (kill-region (region-beginning) (region-end))
	(let (char)
	  (setq char (char-syntax (char-before)))
	  (while (and (not (bobp)) (eq char (char-syntax (char-before))))
		(kill-backward-chars 1))) ))

(defvar aniki-map
  (let ((map (make-sparse-keymap)))
	(define-key map "t" '("shell trans" . aniki-trans))
	(define-key map "f" 'fzf)
	(define-key map "r" 'revert-buffer)
	(define-key map "o" 'find-org)
	(define-key map "c" 'org-roam-capture)
	map))

(defun find-org()
  (interactive)
  (let ((default-directory "~/org/"))
	(call-interactively 'find-file)))

(defun dired-current()
  (interactive)
  (dired default-directory))

(provide 'aniki)
