;;; dash-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "dash" "dash.el" (0 0 0 0))
;;; Generated autoloads from dash.el

(autoload 'dash-fontify-mode "dash" "\
Toggle fontification of Dash special variables.

Dash-Fontify mode is a buffer-local minor mode intended for Emacs
Lisp buffers.  Enabling it causes the special variables bound in
anaphoric Dash macros to be fontified.  These anaphoras include
`it', `it-index', `acc', and `other'.  In older Emacs versions
which do not dynamically detect macros, Dash-Fontify mode
additionally fontifies Dash macro calls.

See also `dash-fontify-mode-lighter' and
`global-dash-fontify-mode'.

\(fn &optional ARG)" t nil)

(defvar global-dash-fontify-mode nil "\
Non-nil if Global Dash-Fontify mode is enabled.
See the `global-dash-fontify-mode' command
for a description of this minor mode.
Setting this variable directly does not take effect;
either customize it (see the info node `Easy Customization')
or call the function `global-dash-fontify-mode'.")

(custom-autoload 'global-dash-fontify-mode "dash" nil)

(autoload 'global-dash-fontify-mode "dash" "\
Toggle Dash-Fontify mode in all buffers.
With prefix ARG, enable Global Dash-Fontify mode if ARG is positive;
otherwise, disable it.  If called from Lisp, enable the mode if
ARG is omitted or nil.

Dash-Fontify mode is enabled in all buffers where
`dash--turn-on-fontify-mode' would do it.
See `dash-fontify-mode' for more information on Dash-Fontify mode.

\(fn &optional ARG)" t nil)

(autoload 'dash-register-info-lookup "dash" "\
Register the Dash Info manual with `info-lookup-symbol'.
This allows Dash symbols to be looked up with \\[info-lookup-symbol].

\(fn)" t nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "dash" '(#("dash-" 0 5 (fontified nil)) #("-keep" 0 1 (fontified nil) 1 5 (fontified nil)) #("-butlast" 0 1 (fontified nil) 1 8 (fontified nil)) #("-zip" 0 1 (fontified nil) 1 4 (fontified nil)) #("-e" 0 1 (fontified nil) 1 2 (fontified nil)) #("->" 0 1 (fontified nil) 1 2 (fontified nil)) #("-gr" 0 1 (fontified nil) 1 3 (fontified nil)) #("-when-let" 0 1 (fontified nil) 1 9 (fontified nil)) #("-d" 0 1 (fontified nil) 1 2 (fontified nil)) #("-l" 0 1 (fontified nil) 1 2 (fontified nil)) #("-s" 0 1 (fontified nil) 1 2 (fontified nil)) #("-m" 0 1 (fontified nil) 1 2 (fontified nil)) #("-u" 0 1 (fontified nil) 1 2 (fontified nil)) #("-value-to-list" 0 1 (fontified nil) 1 14 (fontified nil)) #("-t" 0 1 (fontified nil) 1 2 (fontified nil)) #("--" 0 1 (fontified nil) 1 2 (fontified nil)) #("-r" 0 1 (fontified nil) 1 2 (fontified nil)) #("-juxt" 0 1 (fontified nil) 1 5 (fontified nil)) #("-no" 0 1 (fontified nil) 1 3 (fontified nil)) #("-o" 0 1 (fontified nil) 1 2 (fontified nil)) #("-a" 0 1 (fontified nil) 1 2 (fontified nil)) #("-i" 0 1 (fontified nil) 1 2 (fontified nil)) #("-c" 0 1 (fontified nil) 1 2 (fontified nil)) #("-f" 0 1 (fontified nil) 1 2 (fontified nil)) #("-p" 0 1 (fontified nil) 1 2 (fontified nil)) #("!cons" 0 2 (fontified nil) 2 5 (fontified nil)) #("!cdr" 0 2 (fontified nil) 2 4 (fontified nil)))))

;;;***

;;;### (autoloads nil nil ("dash-functional.el" "dash-pkg.el") (0
;;;;;;  0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dash-autoloads.el ends here
