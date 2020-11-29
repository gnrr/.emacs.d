;; install:
;;
;;   (require 'check-emacs-setting)
;;   (setq check-emacs-setting-files '("~/.emacs.d/init.el"
;;                                     "~/.emacs.d/my-elisp/foo.el")
;;   (setq check-emacs-setting-cmp-pgm "cmp")
;;   (setq check-emacs-setting-diff-pgm "/Applications/Meld.app/Contents/MacOS/Meld")
;;
;;   (add-hook 'emacs-startup-hook (lambda ()
;;     ...
;;     ...
;;     (setq check-emacs-setting-load-success-flag t)   ;; add to end of emacs-startup-hook
;;   ))
;;
;; add "ckeck-emacs-setting.log" to your .gitignore if your .emacs.d directory is version controled by git
;;
;; notes:
;; - this package do NOT check contents in `emacs-startup-hook'
;; 
;; 
;; 
;; 
;; 
;; 
(unless noninteractive
;; ------------------------------------------------- NOT in batch-mode
(defun path-join (&rest paths)
  "thx to https://tototoshi.hatenablog.com/entry/20110520/1305906664"
  (cl-reduce #'(lambda (x y) (concat (file-name-as-directory x) y)) paths))

(defvar check-emacs-setting-files '( "~/.emacs.d/init.el")
  "List of file path of your setting files. These must be in the order they are loaded at startup.")

(defvar check-emacs-setting-dir (locate-user-emacs-file ".check-emacs-setting"))

(defvar check-emacs-setting-log-file (path-join check-emacs-setting-dir "log.txt")
  "Log file of check-emacs-setting.el")

(defvar check-emacs-setting-last-load-dir (path-join check-emacs-setting-dir "last-load")
  "Directory path string to save setting files last loaded normally.")

(defvar check-emacs-setting-cmp-pgm nil
  "External file-compare tool to know modified files. e.g. \"cmp\"")

(defvar check-emacs-setting-diff-pgm nil
  "Non-nil value opens External diff viewer to use when check failed. e.g. \"/WHERE/IS/Meld\"")

(defvar check-emacs-setting-load-success-flag nil)

(defvar check-emacs-setting-script-file (locate-user-emacs-file ".check-emacs-setting/batch-script.el"))

(defun check-emacs-setting-write-script ()
  "Write out elisp script file prior the checking running in batch-mode."
  (with-temp-buffer
    (insert
     "(defconst check-emacs-setting-files '" (format "%S" check-emacs-setting-files) ")\n"
     "(defconst check-emacs-setting-log-file " (format "%S" check-emacs-setting-log-file) ")\n"
     "(load \"" (path-join check-emacs-setting-dir "checker.el") "\")")
    (cl-flet ((message () nil))
      (write-file check-emacs-setting-script-file nil))))

;; fixme use (remove-if xxxxxxxxx)
(defun check-emacs-setting-get-files-not-found ()
  "Return list of non-existed file in the `check-emacs-setting-files'."
  (let ((l '()))
    (dolist (f check-emacs-setting-files)
      (unless (file-exists-p f)
        (add-to-list 'l f)))
    l))

(defun check-emacs-setting-get-modified-files ()
  "Return modifiled files formed like '((new1 . old1) (new2 . old2) ...)"
  (cl-flet ((create-empty-file (path) "Create new empty file whether it exists."
                               (write-region 1 1 path)))
    (let ((mods '()))
      (dolist (new check-emacs-setting-files)
        (let ((old (path-join check-emacs-setting-last-load-dir (file-name-nondirectory new))))
          (if (file-exists-p new)
              (progn
                (unless (file-exists-p old) (create-empty-file old))
                (unless (string= (shell-command-to-string (format "cmp %s %s" old new)) "")
                  (add-to-list 'mods (cons new old))))
            (message "Not found: %s " new))))
      mods)))

(defun check-emacs-setting-get-suspisious ()
  "Return symbol of `file-base-name' that seems to be the cause of error."
  (let ((now-loading-list '()))
    (defun enter (el)
      (add-to-list 'now-loading-list el))
    (defun exit (el)
      (setq now-loading-list (remove el now-loading-list)))
    (load check-emacs-setting-log-file)
    (fmakunbound 'enter)
    (fmakunbound 'exit)
    (car now-loading-list)))

(defun check-emacs-setting-run-checker ()
  "Run Emacs in new process to check it's settings and then return the result.
The return value is nil if no errors. Otherwise it is error message."
  (cl-flet ((create-empty-file (path) "Create new empty file whether it exists."
                               (write-region 1 1 path)))
    (create-empty-file check-emacs-setting-log-file))
  (let ((cmd (format "%s --batch -l %s"
               (expand-file-name invocation-name invocation-directory) ;; executable's path of currently running Emacs
               check-emacs-setting-script-file)))
    (shell-command-to-string cmd))  ;; For Mac, Can not launch current Emacs executable in new process
                                    ;; with option '--batch' by `call-process' for some reason.
                                    ;; Thus using `shell-command-to-string'
  (with-temp-buffer
    (insert-file check-emacs-setting-log-file)
    (goto-char (point-max))
    (message "%d %d" (point) (point-max))
    (if (re-search-backward "^;; \\(.+\\)$" nil t)
        (match-string 1)
      nil)))

(defun check-emacs-setting ()
  "This function checks your Emacs configuration files which are defined `check-emacs-setting-files'."
  (if check-emacs-setting-cmp-pgm
      (let ((mods (check-emacs-setting-get-modified-files)))
        (if mods
            (progn
              (message "Checking ...")
              (check-emacs-setting-write-script)
              (let ((result (check-emacs-setting-run-checker)))
                (if (null result)
                    t         ;; ok --> allow proceed kill-emacs
                  ;; error
                  (when check-emacs-setting-diff-pgm
                    (dolist (m mods)
                      (let ((old (expand-file-name (cdr m)))
                            (new (expand-file-name (car m))))
                        (start-process "check-emacs-setting-diff-process" nil
                                       check-emacs-setting-diff-pgm old new))))
                  (with-temp-buffer
                    (insert result "\n\n"
                            "Suspected: "
                            (symbol-name (check-emacs-setting-get-suspisious)) ".el\n"
                            "-----------------\n")
                    (insert-file check-emacs-setting-log-file)
                    (message-box (buffer-string)))
                  nil)))      ;; NG --> cancel kill-emacs
          t))                 ;; ok --> allow proceed kill-emacs
    (message "check-emacs-setting needs `check-emacs-setting-cmp-pgm'")
    nil))                     ;; NG --> cancel kill-emacs

(add-to-list 'kill-emacs-query-functions 'check-emacs-setting)

(defun check-emacs-setting-backup-last-load-files ()
  "Backup the user setting files such as 'init.el' defined by `check-emacs-setting-files'
 into `check-emacs-setting-last-load-dir'. This function should be called just after loaded
 user setting files at launching emacs without any errors, thus `emacs-startup-hook' is used."
  (unless (file-directory-p check-emacs-setting-last-load-dir)
    (make-directory check-emacs-setting-last-load-dir t))
  (dolist (f check-emacs-setting-files)
    (let ((dest (path-join check-emacs-setting-last-load-dir (file-name-nondirectory f))))
      (copy-file f dest t))))

;; backup when success to launch emacs and not in batch mode
(add-hook 'emacs-startup-hook #'check-emacs-setting-backup-last-load-files)

)

(provide 'check-emacs-setting)
