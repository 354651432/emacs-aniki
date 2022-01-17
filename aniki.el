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
	(define-key map "a" 'org-agenda)
	map))

(defun find-org()
  (interactive)
  (let ((default-directory "~/org/"))
	(call-interactively 'find-file)))

(defun dired-current()
  (interactive)
  (dired default-directory))

;; https://sachachua.com/blog/2008/07/emacs-keyboard-shortcuts-for-navigating-code/
(defun sacha/isearch-yank-current-word ()
  "Pull current word from buffer into search string."
  (interactive)
  (save-excursion
    (skip-syntax-backward "w_")
    (isearch-yank-internal
     (lambda ()
       (skip-syntax-forward "w_")
       (point)))))
(define-key isearch-mode-map (kbd "C-x") 'sacha/isearch-yank-current-word)

(defface aniki-org-list
  '((t :foreground "#5eafef"))
  "face for org list")

(provide 'aniki)
