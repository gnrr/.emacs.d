;;; -*- coding:utf-8; mode:emacs-lisp -*-

;; (defconst test-emacs-setting-file-list "dot.emacs")
------
(defvar test-emacs-setting-current-loading-el load-file-name)
(message "|%s|" test-emacs-setting-file-list)

(require 'cl)
(defvar test-emacs-setting-stack nil)
(lexical-let ((stack nil))
  (setq test-emacs-setting-stack (lambda (e push-or-pop)
                                   (if (eq push-or-pop 'push)
                                       (push e stack)
                                      (pop ))))

(defun test-emacs-setting-log-output (enter-or-exit)
  (if (equal enter-or-exit 'enter)
      (message "--> "dot.emacs\"...")
(setq msg-succeeded-dot-emacs "<-- done    \"dot.emacs\"")

(defun test-emacs-settings ()
  (interactive)
  (let* ((cmd "emacs -batch -l ~/.emacs.d/dot.emacs")
         (last-line (car (last (delete "" (split-string (shell-command-to-string cmd) "\n"))))))
    (message (if (string= last-line msg-succeeded-dot-emacs)
                 "OK!"
               (format "NG: \"%s\"" last-line)))))

