;; notes:
;;   This elisp file is a part of check-emacs-setting.el.
;;   Do not modify this file.

(when noninteractive
;; ------------------------------------------------- in batch-mode
(defconst check-emacs-setting-buf " *check-emacs-setting-log*")
(get-buffer-create check-emacs-setting-buf)

(defun check-emacs-setting-log (from pos target)
  (with-current-buffer check-emacs-setting-buf
    (insert (cond ((eq pos 'start) (format "(enter '%s)\t;; %s\n" target from))
                  ((eq pos 'end)   (format "(exit  '%s)\t;; %s\n" target from))
                  (t (format ";; unknown target (%s)\t;; %s\n" target from))))))

(defadvice load (around check-emacs-setting-log-load activate)
  (let ((target-p (member (file-name-nondirectory file)
                          (mapcar #'(lambda (x) (file-name-nondirectory x)) check-emacs-setting-files))))
    (when target-p (check-emacs-setting-log 'load 'start (file-name-base file)))
    ad-do-it
    (when target-p (check-emacs-setting-log 'load 'end (file-name-base file)))))

(defadvice require (around check-emacs-setting-log-require activate)
  (let ((target-p (memq feature
                        (mapcar #'(lambda (x) (intern (file-name-base x))) check-emacs-setting-files))))
    (when target-p (check-emacs-setting-log 'require 'start feature))
    ad-do-it
    (when target-p (check-emacs-setting-log 'require 'end feature))))

(condition-case err
    (load (car check-emacs-setting-files))
  (error
   (let ((err-msg (error-message-string err)))
     (with-current-buffer check-emacs-setting-buf
       (insert ";; " err-msg)

       ;; ;; fixme: not work by flet, cl-flet, cl-letf
       ;; (cl-letf (((message (&rest args) nil)))
       ;;   (write-file check-emacs-setting-log-file nil)))

       ;; ;; fixme: not work by func-swapping, is `message' wrong target???
       ;; (fset 'orig-fun (symbol-function 'message))
       ;; (defun message (&rest _) nil)
       ;; (write-file check-emacs-setting-log-file nil)
       ;; (fset 'message (symbol-function 'orig-fun))
       ;; (fmakunbound 'orig-fun)))

       (write-file check-emacs-setting-log-file nil)
     (error err-msg))))
)
