(require 'discrete) ;; path-join

(defvar check-emacs-setting-files '( "~/.emacs.d/init.el")
  "List of file path of your setting files. These must be in the order they are loaded at startup.")

(defvar check-emacs-setting-last-load-dir "~/.emacs.d/last-load"
  "Directory path-string to save setting files last loaded.")

(defvar check-emacs-setting-diff-pgm nil
  "External diff viewer to use when check failed. e.g. \"/WHERE/IS/Meld\"")
(defvar check-emacs-setting-cmp-pgm nil
  "External file-compare tool to know modified files. e.g. \"cmp\"")

(defun check-emacs-setting-get-modified-files ()
  "Return modifiled files formed like '((new1 . old1) (new2 . old2) ...)"
  (let ((mods '()))
    (dolist (curr check-emacs-setting-files)
      (let ((old (path-join check-emacs-setting-last-load-dir (file-name-nondirectory curr))))
        (if (and (file-exists-p curr) (file-exists-p old))
            (unless (string= (shell-command-to-string (format "cmp %s %s" old curr)) "")
              (add-to-list 'mods (cons curr old)))
          (message "not found: %s " old))))
    mods))

(defun check-emacs-setting-1 ()
  (let* ((cmd (format "emacs -batch -debug-init -l %s" (first check-emacs-setting-files)))
         (last-line (car (last (delete "" (split-string (shell-command-to-string cmd) "\n"))))))
    last-line))

(defun check-emacs-setting ()
  (message "Checking ...")
  (let ((last-line (check-emacs-setting-1)))
    (if (string= last-line msg-succeeded-dot-emacs)
        t             ;; ok --> tell `kill-emacs-query-functions' to allow proceed kill-emacs
      ;; error
      (message-box "Setting Error\n\n %s" last-line)
      (when (and check-emacs-setting-diff-pgm
                 (file-exists-p check-emacs-setting-diff-pgm))
        (dolist (pair (check-emacs-setting-get-modified-files))
          (let ((new (expand-file-name (car pair)))
                (old (expand-file-name (cdr pair))))
            (start-process "diff" nil check-emacs-setting-diff-pgm old new))))
      nil)))

(add-to-list 'kill-emacs-query-functions 'check-emacs-setting)

(defun check-emacs-setting-backup-last-load-files ()
  (unless (file-directory-p check-emacs-setting-last-load-dir)
    (make-directory check-emacs-setting-last-load-dir t))
  (dolist (f check-emacs-setting-files)
    (let ((dest (path-join check-emacs-setting-last-load-dir (file-name-nondirectory f))))
      (copy-file f dest t))))

(unless noninteractive
  ;; NOT backup in batch mode
  (add-hook 'emacs-startup-hook #'check-emacs-setting-backup-last-load-files))

(provide 'check-emacs-setting)
