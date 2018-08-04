;;
;; ~/.emacs.d/dot.emacs
;;
;; $ ln -s ~/.emacs.d/dot.emacs ~/.emacs
;;

;; functionalities to backup/restore init files
(defun backup-init-files-backup ()
  (let ((s "")
        (bak ""))
    (dolist (f backup-init-files-file-list)
      (unless (file-exists-p f)
        (setq s (concat s " " f))))
    (if (null (string= s ""))
        (message "not exists:%s" s)

      (dolist (f backup-init-files-file-list)
        (setq bak (concat f backup-init-files-ext-bak))
        (copy-file f bak t t)
        (setq s (concat s " " f)))
      (message "backed up:%s" s))))

(defun backup-init-files-restore ()
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
        (copy-file bak f t t)
        (setq s (concat s " " f)))
      (message "restored:%s" s))))

(defvar backup-init-files-ext-bak ".last-load")
(defvar backup-init-files-ext-old ".old")
(defvar backup-init-files-file-list '("~/.emacs.d/init.el"))

;;
;; load ~/.emacs.d/init.d
;;

(load "~/.emacs.d/init.el")

(backup-init-files-backup)

;; call following function to restore init files if error occurs while startup Emacs.
;; (backup-init-files-restore)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("78496062ff095da640c6bb59711973c7c66f392e3ac0127e611221d541850de2" default)))
 '(package-selected-packages
   (quote
    (helm-descbinds neotree gist hiwin helm-swoop rainbow-mode smartparens telephone-line helm-gtags scratch-log markdown-mode expand-region helm-ag dashboard use-package tabbar ag ido-yes-or-no helm atom-one-dark-theme evil-escape evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "#141619")))))
