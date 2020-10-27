;;; -*- coding:utf-8; mode:emacs-lisp -*-
;;;
;;; ~/.emacs.d/dot.emacs
;;;

;;; ** Windows **
;;; $ cp ~/.emacs.d/dot.emacs ~/.emacs
;;;   and
;;; Make new USER ENVIROMENT VARIABLE "HOME" --> "C:\Users\XXXX"
;;;
;;; ** Mac/Linux **
;;; $ ln -s ~/.emacs.d/dot.emacs ~/.emacs
;;;

(message "--> loading \"dot.emacs\"...")
(setq msg-succeeded-dot-emacs "<-- done    \"dot.emacs\"")
  
;;;
;;; test-emacs-settings
;;;
(defvar test-emacs-settings-target-list '("dot.emacs" "init.el" "discrete.el" "my-backup.el"))
(defvar test-emacs-settings-top "~/.emacs.d/dot.emacs")
(defvar test-emacs-settings-ok-string msg-succeeded-dot-emacs)

(defun test-emacs-settings-1 ()
  (let* ((cmd (concat "emacs -batch -l " test-emacs-settings-top))
         (last-line (car (last (delete "" (split-string (shell-command-to-string cmd) "\n"))))))
    (if (string= last-line test-emacs-settings-ok-string)
        "OK!"
      (format "NG: \"%s\"" last-line))))

(defun test-emacs-settings ()
  (interactive)
  (let ((fn (file-name-nondirectory (buffer-file-name))))
    (message "Checking \"%s\"..." fn))
  (message (test-emacs-settings-1)))

(defun test-emacs-settings-after ()
  (let ((fn (file-name-nondirectory (buffer-file-name))))
    (when (and (equal major-mode 'emacs-lisp-mode)
               (member fn test-emacs-settings-target-list))
      (message "Checking \"%s\"..." fn)
      (message (test-emacs-settings-1)))))

(add-hook 'after-save-hook  'test-emacs-settings-after)

;;
;; package setting
;;
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq package-user-dir "~/.emacs.d/packages")
(package-initialize)
(unless (require 'use-package nil t)
  (defmacro use-package (&rest args)))

;;
;; functionalities to backup/restore init files
;;
(defun backup-init-files-backup ()
  (let ((s "")
        (bak ""))
    (dolist (f backup-init-files-file-list)
      (unless (file-exists-p f)
        (setq s (concat s " " f))))
    (if (null (string= s ""))
        (message "not exists: %s" s)

      (dolist (f backup-init-files-file-list)
        (setq bak (concat f backup-init-files-ext-bak))
        (copy-file f bak t)
        (message "backed up: %s --> %s" f bak)))))

(defun backup-init-files-restore ()
  (interactive)
  (let ((s "")
        (bak "")
        (old ""))
    (dolist (f backup-init-files-file-list)
      (setq bak (concat f backup-init-files-ext-bak))
      (unless (file-exists-p bak)
        (setq s (concat s " " bak))))
    (if (null (string= s ""))
        (message "not exists: %s" s)
      (dolist (f backup-init-files-file-list)
        (setq bak (concat f backup-init-files-ext-bak))
        (setq old (concat f backup-init-files-ext-old))
        (copy-file f old t t)
        (copy-file bak f t)
        (setq s (concat s " " f)))
      (message "restored:%s" s))))

(defvar backup-init-files-ext-bak ".last-load")
(defvar backup-init-files-ext-old ".err")
(defvar backup-init-files-file-list '("~/.emacs.d/dot.emacs" "~/.emacs.d/init.el"))

(defalias 'revert-init-files 'backup-init-files-restore)

;;
;; load ~/.emacs.d/init.d
;;
(load "~/.emacs.d/init.el" nil t)

(backup-init-files-backup)                ; backup init files when loaded normaly

;; Use `M-x backup-init-files-restore' to restore init files if error occurs while startup Emacs.

;;
;; customize setting
;;
(setq custom-file "~/.emacs.d/custom.el") ; write custom settings into external file instead of init.el
(load custom-file nil t)

(message msg-succeeded-dot-emacs)

;;
;; dot.emacs ends here
;;
