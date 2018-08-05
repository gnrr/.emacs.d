;;; kivy-mode-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "kivy-mode" "kivy-mode.el" (0 0 0 0))
;;; Generated autoloads from kivy-mode.el

(add-to-list 'auto-mode-alist '("\\.kv$" . kivy-mode))

(autoload 'kivy-mode "kivy-mode" "\
Simple mode to edit kivy.

\\{kivy-mode-map}

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "kivy-mode" '("kivy-")))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; kivy-mode-autoloads.el ends here
