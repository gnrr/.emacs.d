;;; -*- coding:utf-8; mode:emacs-lisp -*-
;;;
;;; init.el
;;;
(add-to-list 'load-path (locate-user-emacs-file "elisp"))
(setq custom-theme-directory (locate-user-emacs-file "themes"))
(set-default-coding-systems 'utf-8-unix)

;; to hide message "ad-handle-definition: ‘vc-revert’ got redefined"
(setq ad-redefinition-action 'accept)

;; ----------------------------------------------------------------------
;; my-elisp
(require 'discrete)
(require 'my-backup)
(setq my-backup-directory "~/bak")

;; check-emacs-setting
(require 'check-emacs-setting)
(setq check-emacs-setting-files '("~/.emacs.d/init.el"
                                  "~/.emacs.d/elisp/discrete.el"
                                  "~/.emacs.d/elisp/_mac.el"
                                  "~/.emacs.d/elisp/_ubuntu.el"
                                  "~/.emacs.d/elisp/_windows.el"))

;; ----------------------------------------------------------------------
(defun mycolor (name)
  (let ((colors '((white       . "#f9f9f9")
                  (light-gray  . "#a4a2a2")
                  (gray        . "#7c7a7a")
                  (dark-gray   . "#555555")
                  (dark-gray2  . "#3e3e3e")
                  (black       . "#000000")
                  (red         . "#ff6b7f")
                  (blue        . "#61afef")
                  (dark-blue2  . "#1684DF")
                  (dark-blue   . "#126EBA")
                  (dark-blue3  . "#0F5895")
                  (green       . "#98be65")
                  (pink        . "#eb7bc0")
                  (purple      . "#c678dd")
                  (orange      . "#e3b23c")
                  ;; (charcoal . "#3d363e"))))
                  ;; (charcoal . "#362f37")))
                  (charcoal    . "#2b262c"))))
    (cdr (assoc name colors))))

;; e.g. (mycolor 'red) => "#ff6b7f"

(defun myfont (type)
  (let* ((fonts '((default  . "Source Han Code JP N")
                  (default2 . "Consolas")
                  (default3 . "Cica")
                  (default4 . "Hack")
                  (ui       . "x14y24pxHeadUpDaisy")
                  (ui2      . "Krungthep")
                  (ui3      . "Squarea")
                  (posframe . "UbuntuMono Nerd Font Mono")
                  ))
         (name (cdr (assoc type fonts))))
    (if window-system
        (if (x-list-fonts name)
            name
          (progn
            (message (format "ERROR: Font not found: %s" name))
            nil))
      (unless noninteractive
        ;; NOT occurs error in batch mode (= while checking)
        (message "ERROR: Specifying font can only work under any window-system."))
      nil)))

;; e.g. (myfont 'default) => "Source Han Code JP N"

;; ----------------------------------------------------------------------
;; host independent
(require
 (cond ((eq system-type 'windows-nt) '_windows)
       ((eq system-type 'gnu/linux)  '_linux)
       ((eq system-type 'darwin)     '_mac)
       (t (error "Unknown system-type: %s" system-type))))

(defun my-adv-load-theme--font-change (&rest _)
 (let ((font (myfont 'ui)))
   (when font
     (set-face-attribute 'mode-line          nil :family font)
     (set-face-attribute 'mode-line-inactive nil :family font)
     (set-face-attribute 'minibuffer-prompt  nil :family font)

     (set-face-attribute 'line-number              nil :family font :height my-face-adj-line-number-height)
     (set-face-attribute 'line-number-current-line nil :family font :height my-face-adj-line-number-height))))

(advice-add 'load-theme :after #'my-adv-load-theme--font-change)

;; ----------------------------------------------------------------------
;; defaults
(setq-default inhibit-startup-screen t)           ; Disable start-up screen

(add-hook 'emacs-startup-hook (lambda ()
 (message "--> startup-hook")

 (setq-default
  auto-window-vscroll nil                          ; Lighten vertical scroll
  ;; confirm-kill-emacs 'yes-or-no-p                  ; Confirm before exiting Emacs
  delete-by-moving-to-trash t                      ; Delete files to trash
  display-time-default-load-average nil            ; Don't display load average
  display-time-format "%H:%M"                      ; Format the time string
  fill-column 80                                   ; Set width for automatic line breaks
  indent-tabs-mode nil                             ; Stop using tabs to indent
  initial-scratch-message ""                       ; Empty the initial *scratch* buffer
  left-margin-width 1 right-margin-width 1         ; Add left and right margins
  select-enable-clipboard t                        ; Merge system's and Emacs' clipboard
  sentence-end-double-space nil                    ; End a sentence after a dot and a space
  show-trailing-whitespace t                       ; Display trailing whitespaces
  uniquify-buffer-name-style 'forward              ; Uniquify buffer names
  window-combination-resize t                      ; Resize windows proportionally

  bidi-display-reordering nil                      ; 右から左に読む言語に対応させないことで描画を高速化
  vc-follow-symlinks t
  ring-bell-function 'ignore
  parens-require-spaces nil
  transient-mark-mode nil
  tab-width 4
  tab-stop-list nil
  comment-column 60

  ;; display-line-numbers-grow-only t
  ;; display-line-numbers-width-start 10
  ;; line-number-display-width 10
  display-line-numbers-width 4

  ;; 1行スクロール
  ;; (setq scroll-conservatively most-positive-fixnum)
  scroll-margin 3
  next-screen-context-lines 3
  scroll-preserve-screen-position t

  next-line-add-newlines nil                  ; バッファ末尾に余計な改行コードを防ぐための設定
  idle-update-delay 0.3

  electric-pair-mode nil

  ;;
  ;; backup files
  ;; https://masutaka.net/chalow/2014-05-11-1.html
  ;; http://yohshiy.blog.fc2.com/blog-entry-319.html
  ;;
  ;; backup to `hoge.txt~'
  backup-directory-alist '((".*" . "~/.Trash"))
  version-control     t  ;; 番号付けによる複数保存 存実行の有無
  kept-new-versions   5  ;;                   最新の保持数
  kept-old-versions   1  ;;                   最古の保持数
  delete-old-versions t  ;;                   範囲外を削除

  ;; backup to `#hoge.txt#'
  auto-save-file-name-transforms '(("~/\\([^/]*/\\)*\\([^/]*\\)$" "~/.Trash/\\2" t))
                                        ;             '((".*" "~/.Trash" t))

  auto-save-default nil                  ; disabled

  ;; backup to `~/.emacs.d/auto-save-list/.saves-xxxx'
  auto-save-list-file-prefix nil         ; disabled

  ;; lock file to `.#hoge'
  create-lockfiles nil                   ; disabled

  ) ;; setq-default

;; ----------------------------------------------------------------------
 (fset 'yes-or-no-p 'y-or-n-p)                     ; Replace yes/no prompts with y/n
 (tool-bar-mode -1)
 (menu-bar-mode 0)                                 ; Disable the menu bar
 (add-hook 'focus-out-hook #'garbage-collect)
 (electric-indent-mode)

 (setq cursor-type 'box)
 (blink-cursor-mode 0)

 ;; margin
 (setq-default left-margin-width 0 right-margin-width 0) ; Define new widths.
 (set-window-buffer nil (current-buffer))                ; Use them now.

 (set-face-background 'trailing-whitespace (mycolor 'red))
 ;; disable show-trailing-whitespace in any case
 (add-hook 'minibuffer-setup-hook (lambda () (setq-local show-trailing-whitespace nil)))
 (add-hook 'counsel-mode-hook (lambda () (setq-local show-trailing-whitespace nil)))

 ;; save-place
 (setq save-place-file "~/.emacs.d/.emacs-places")
 (save-place-mode 1)                                     ; Enable save-place

 ;; ミニバッファの履歴を保存する
 (savehist-mode 1)
 (setq history-length 1000)

 (global-auto-revert-mode -1)                            ; disable auto-revert-mode
 (setq indent-line-function 'indent-relative-maybe)

 ;; mode-line
 (column-number-mode t)
 (set-face-attribute 'mode-line          nil :box nil :height 1.0)   ; モードラインを非3D化
 (set-face-attribute 'mode-line-inactive nil :box nil :height 1.0 :background (mycolor 'charcoal) :foreground "#5f5f6f")

 ;; モードラインの割合表示を総行数表示に
 (defvar my-mode-line-position-format "%%3c:%%l/%d")
 (setq mode-line-position '(:eval (format my-mode-line-position-format
                                          (count-lines (point-max) (point-min)))))

 ;; タイトルバーにファイルのフルパス表示
 (defmacro replace-home-directory-string (file-name)
   `(if ,file-name
        (let ((regexp (cond ((eq system-type 'windows-nt) "^C:\\Users\\[^\\]\\+")
                            ((eq system-type 'gnu/linux)  "^/home/[^/]+/")
                            (t                            "^/Users/[^/]+/"))))
          (replace-regexp-in-string regexp "~/" ,file-name))
      nil))

 ;; (defun emacs-version-briefly ()
 ;;   (let ((lst (split-string (emacs-version))))
 ;;     (concat (nth 1 lst) (nth 2 lst))))

 (setq frame-title-format '(format "%s"
                                   (:eval (or (replace-home-directory-string (buffer-file-name))
                                              (buffer-name)))))

 (set-face-background 'region (mycolor 'dark-blue3))

 ;; =====================================================================
 ;; key unbinding / binding
 (keyboard-translate ?\C-h ?\C-?)                        ; c-h

 (global-unset-key (kbd "M-,"))                          ; xref
 (global-unset-key (kbd "M-."))                          ; xref
 (global-unset-key (kbd "C-z"))                          ; suspend-frame
 (global-unset-key (kbd "C-x C-z"))                      ; suspend-frame
 (global-unset-key (kbd "C-x o"))                        ; other-window
 (global-unset-key (kbd "M-t"))                          ; transpose-word
 (global-unset-key (kbd "M-'"))                          ; abbrev-prefix-mark
 (global-unset-key (kbd "M-c"))                          ; capitalize-word     why also assigned to M-RET ??
 (global-unset-key (kbd "M-i"))                          ; tab-to-tab-stop
 (global-unset-key [f11])                                ; toggle-frame-fullscreen
 (global-unset-key [f12])                                ; "M-c"

 (global-set-key (kbd "C-x C-x") #'nop)                  ; exchange-point-and-mark

 ;; (global-set-key "(" 'my-insert-paren)                   ; ()
 (global-set-key "{" 'my-insert-brace)                   ; {}
 (global-set-key "[" 'my-insert-bracket)                 ; []
 ;; (global-set-key "<" 'my-insert-angle)                   ; <>
 ;; (global-set-key "'" 'my-insert-squote)                  ; ''
 (global-set-key "\"" 'my-insert-dquote)                 ; ""

 (global-set-key (kbd "C-m") 'newline-and-indent)             ; Returnキーで改行＋オートインデント
 (global-set-key (kbd "C-0") 'delete-window)
 (global-set-key (kbd "C-1") 'delete-other-windows)
 (global-set-key (kbd "C-2") 'split-window-below)
 (global-set-key (kbd "C-3") 'split-window-right)
 (global-set-key (kbd "C-o") 'other-window)

 (global-set-key (kbd "M-9") 'insert-parentheses)
 (global-set-key (kbd "M-[") 'my-insert-brace2)
 (global-set-key (kbd "M-g") 'goto-line)
 (global-set-key (kbd "M-P") 'beginning-of-buffer)
 (global-set-key (kbd "M-N") 'end-of-buffer)

 (global-set-key (kbd "C-x t") 'revert-buffer)
 (global-set-key (kbd "C-x C-t") 'toggle-truncate-lines)
 (global-set-key (kbd "C-x n f") 'narrow-to-defun)

 (define-key isearch-mode-map (kbd "C-b") 'isearch-delete-char)

;; ----------------------------------------------------------------------
 (defun my-func ()
   "called \'my-func\'")

 (global-set-key [f2] '(lambda () (interactive) (message "%S" (funcall 'my-func))))

;; ----------------------------------------------------------------------
;; which-func-mode
 (setq which-func-unknown "-"
       which-func-modes '(emacs-lisp-mode lisp-interaction-mode c-mode python-mode ruby-mode)
       which-func-format '(:propertize which-func-current face which-func))

 (which-function-mode 0)        ;; global

 ;; ----------------------------------------------------------------------
 (setq truncate-partial-width-windows nil)
 (setq-default truncate-lines t)

 ;; kill-ringに同じ内容を重複して入れない
 (defadvice kill-new (before ys:no-kill-new-duplication activate)
   (setq kill-ring (delete (ad-get-arg 0) kill-ring)))

 ;; prevent annoying message "Text is read only" at mimibuffer
 (plist-put minibuffer-prompt-properties
            'point-entered 'minibuffer-avoid-prompt)

 ;; enable completion in `eval-expression' (M-:)
 (define-key read-expression-map (kbd "TAB") 'lisp-complete-symbol)

 (defun indent-or-insert-tab ()
   (interactive)
   (let ((pos (point)))
     (funcall indent-line-function)
     (when (= pos (point))
       (insert "\t"))))

 (global-set-key "\C-i" 'indent-or-insert-tab)

 (custom-set-faces
  ;; custom-set-faces was added by Custom. If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance. If there is more than one, they won't work right.
  '(hl-line ((t (:background "#141619")))))

 ;; (global-hl-line-mode 1)

 ;; ----------------------------------------------------------------------
 ;; command aliases
 (defalias 'reb 're-builder)
 (defalias 'a 'counsel-apropos)

 (defalias 'dv 'describe-variable)
 (defalias 'dfun 'describe-function)
 (defalias 'dface 'describe-face)
 (defalias 'dk 'describe-key)

 (defalias 'l 'display-line-numbers-mode)
 (defalias 'hl 'hl-line-mode)
 (defalias 'calc 'quick-calc)
 (defalias 'package-uninstall 'package-delete)

 ;; ----------------------------------------------------------------------
 (defvar exclude-face-list '(rainbow-delimiters-base-face
                             rainbow-delimiters-depth-1-face
                             rainbow-delimiters-depth-2-face
                             rainbow-delimiters-depth-3-face
                             rainbow-delimiters-depth-4-face
                             rainbow-delimiters-depth-5-face
                             rainbow-delimiters-depth-6-face
                             rainbow-delimiters-depth-7-face
                             rainbow-delimiters-depth-8-face
                             rainbow-delimiters-depth-9-face
                             rainbow-delimiters-mismatched-face
                             rainbow-delimiters-unmatched-face
                             sp-show-pair-match-face
                             mode-line-buffer-id
                             mode-line-emphasis
                             mode-line-highlight
                             mode-line-inactive
                             mode-line))

 (my-font-lighter (remove-if (lambda (x) (member x exclude-face-list)) (face-list)))

 ;; (zerodark-setup-modeline-format)
 (my-load-frame)

 (defun my-font-lock-add-keywords-elisp ()
   (font-lock-add-keywords nil
     '(("(\\(lambda\\|cons\\|car\\|cdr\\|nth\\|eq\\|equal\\|null\\|remove\\|delete
\\|mapc\\|mapcar\\|fset\\|set
\\|memq\\|member\\|delq\\|funcall\\|fboundp\\|list\\|add-to-list\\|concat\\|call-interactively
\\|assoc\\|rassoc\\|add-hook\\|remove-hook\\|define-key\\|global-set-key\\|local-set-key\\|define-key
\\|ad-activate\\|ad-enable-advice\\|ad-disable-advice\\|propertize\\|run-hooks\\)[ \t\n]" . font-lock-keyword-face))))

 (add-hook 'emacs-lisp-mode-hook #'my-font-lock-add-keywords-elisp)
 (add-hook 'emacs-lisp-mode-hook #'flymake-mode)
 (add-hook 'lisp-interaction-mode-hook #'my-font-lock-add-keywords-elisp)
 (add-hook 'lisp-interaction-mode-hook #'flymake-mode)

 (lisp-interaction-mode)                            ;; workaround for scratch-log

 ;; ここからモードライン設定
 ;; https://tsuu32.hatenablog.com/entry/2019/08/04/160316
 (set 'eol-mnemonic-dos (propertize "\u24d3" 'face `(:family ,(myfont 'default3))))  ;; d
 (set 'eol-mnemonic-unix (propertize "\u24e4" 'face `(:family ,(myfont 'default3)))) ;; u
 (set 'eol-mnemonic-mac (propertize "\u24dc" 'face `(:family ,(myfont 'default3))))  ;; m
 (set 'eol-mnemonic-undecided (propertize "?" 'face `(:family ,(myfont 'default3)))) ;; ?
 (defun my-coding-system-name-mnemonic ()
   (let* ((base (coding-system-base buffer-file-coding-system))
          (name (symbol-name base)))
     (cond ((string-prefix-p "utf-8" name) "U8")
           ((string-prefix-p "utf-16" name) "U16")
           ((string-prefix-p "utf-7" name) "U7")
           ((string-prefix-p "japanese-shift-jis" name) "SJIS")
           ((string-match "cp\\([0-9]+\\)" name) (match-string 1 name))
           ((string-match "japanese-iso-8bit" name) "EUC")
           (t "???")
           )))

 (defun my-mode-line-vc-string ()
   (if vc-mode
       (concat (propertize "\ue725" 'face `(:family ,(myfont 'default3))) (second (split-string vc-mode ":")))   ;; nf-dev-git_branch
     ""))

 (defun my-mode-line-num ()
   (format "%3s:%-s/%d"
           (format-mode-line "%c")
           (format-mode-line "%l")
           (line-number-at-pos (point-max))))

(defun moon-flymake-mode-line ()
  "https://emacs-china.org/t/flymake-mode-line/7878"
  (let* ((known (hash-table-keys flymake--backend-state))
         (running (flymake-running-backends))
         (disabled (flymake-disabled-backends))
         (reported (flymake-reporting-backends))
         (diags-by-type (make-hash-table))
         (all-disabled (and disabled (null running)))
         (some-waiting (cl-set-difference running reported)))
    (maphash (lambda (_b state)
               (mapc (lambda (diag)
                       (push diag
                             (gethash (flymake--diag-type diag)
                                      diags-by-type)))
                     (flymake--backend-state-diags state)))
             flymake--backend-state)
    (apply #'concat
           (mapcar (lambda (args)
                     (apply (lambda (num str face)
                              (propertize
                               (format str num) 'face face))
                               ;; (format str num) 'face `(:family `(myfont 'default3))))
                            args))
                   ;; `((,(length (gethash :error diags-by-type)) "\uf79f%d " error)    ;; nf-mdi-ghost
                     ;; (,(length (gethash :warning diags-by-type)) "\uf071%d " warning)    ;; nf-fa-warning
                     ;; (,(length (gethash :note diags-by-type)) "\uf05a%d" success))))))   ;; nf-fa-info_circle
                   `((,(length (gethash :error diags-by-type)) "%d " error)
                     (,(length (gethash :warning diags-by-type)) "%d " warning)
                     (,(length (gethash :note diags-by-type)) "%d" success))))))

 ;; -----------
;;  (defun simple-mode-line-render (left right)
;;    "Return a string of `window-width' length.
;; Containing LEFT, and RIGHT aligned respectively."
;;    (let ((available-width
;;           (- (window-total-width)
;;              (+ (string-width (format-mode-line left))
;;                 (string-width (format-mode-line right))))))
;;      (append left
;;              (list (format (format "%%%ds" available-width) ""))
;;              right)))

;;  (setq-default
;;   mode-line-format
;;   '((:eval
;;      (simple-mode-line-render
;;       ;; Left.
;;       (quote ("%e "
;;               mode-line-buffer-identification
;;               " %l : %c"
;;               evil-mode-line-tag
;;               "[%*]"))
;;       ;; Right.
;;       (quote ("%p "
;;               mode-line-frame-identification
;;               mode-line-modes
;;               mode-line-misc-info))))))

 ;;---------
 (defun my-mode-line--form ()
   (let* ((left-part (concat
                      evil-mode-line-tag
                      " "
                      (if (buffer-modified-p)
                          (propertize (format-mode-line "%b") 'face `(:foreground ,(mycolor 'red)))
                        (format-mode-line "%b"))
                      " "
                      (propertize (if buffer-read-only "\uf023" " ") 'face `(:family ,(myfont 'default3) :foreground ,(mycolor 'red))) ;; nf-fa-lock
                      " "))
          (right-part (concat
                       (my-coding-system-name-mnemonic)
                       (mode-line-eol-desc)
                       " "
                       (my-mode-line-num)
                       " "
                       (my-mode-line-vc-string)
                       ;; (format-mode-line "%l:%c")
                       " "
                       (if flymake-mode (moon-flymake-mode-line) "")
                       " "
                       (propertize mode-name 'face '(:weight bold))
                       " "
                       ;; mode-line-misc-info
                       ;; " "
                       ;; (vc-mode vc-mode)
                       ;; mode-line-modes))
                       ))
          (margin-env 8)    ;; windows
          (margin
           (propertize " "
                       'display `(space :align-to (- (+ scroll-bar scroll-bar) ,(string-width right-part) ,margin-env)))))
     (concat left-part margin right-part)))
 (setq-default mode-line-format '(:eval (my-mode-line--form)))
 ;; (kill-local-variable 'mode-line-format)

 ;; ;; modeline の右端が切れる問題 East Asian Ambiguous Width
 ;; ;; https://note.com/5mingame2/n/nc8010be0c32e
 ;; (set-language-environment "English") ;; call this explicity
 ;; (prefer-coding-system 'utf-8)
 ;; (set-default-coding-systems 'utf-8)
 ;; (set-terminal-coding-system 'utf-8)
 ;; (set-keyboard-coding-system 'utf-8)
 ;; (set-buffer-file-coding-system 'utf-8-unix)
 ;; (setq locale-coding-system 'utf-8)
 ;; (setq file-name-coding-system 'utf-8)
 ;; (setq default-process-coding-system '(utf-8-unix . utf-8-unix))
 ;; (require 'my-utf-8-eaw-fullwidth)

 (setq enable-local-variables nil)  ;; disable "emacs the local variables list in..." when find-file

 (message "<-- startup-hook")

 ;; show emacs version and startup time in mini-buffer
 (message "%s / %.3f sec"
          (substring (version) 0 14)
          (float-time (time-subtract after-init-time before-init-time)))
)) ;; emacs-startup-hook function ends here

;; ======================================================================
;; auto-insert
(add-hook 'find-file-hook 'auto-insert)
(setq auto-insert-directory "~/.emacs.d/templates")
(defvar auto-insert-alist nil)
(setq auto-insert-alist (cons '("\\.mq4" . "mq4")
                                auto-insert-alist))

;; ----------------------------------------------------------------------
;; im-ctl
;; (defun im-ctl (on) (do-depends-on-each-os))

(defun im-on ()
  (interactive)
  (if (fboundp 'im-ctl)
      (im-ctl t)
    (message "Error: Not defined function \"im-ctl\"")))

(defun im-off ()
  (interactive)
  (if (fboundp 'im-ctl)
      (im-ctl nil)
    (message "Error: Not defined function \"im-ctl\"")))

(add-hook 'minibuffer-exit-hook #'im-off)
;; (add-hook 'focus-out-hook #'im-off)
;; (add-hook 'evil-insert-state-exit-hook #'im-off)

;; fixme need this?
;; ----------------------------------------------------------------------
;; utility for use-package
(defun my-font-exists-p ($font-name)
  (if (null (x-list-fonts $font-name))
      nil t))

;; ----------------------------------------------------------------------
;;
;; package setting
;;
(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;; (add-to-list 'package-archives '("marmalade" . "https://marmalade-repo.org/packages/") t)
(setq package-user-dir "~/.emacs.d/packages")
(package-initialize)
(unless (require 'use-package nil t)
  (defmacro use-package (&rest args)))

;; ----------------------------------------------------------------------
(use-package cl-lib
  :init
  (setq byte-compile-warnings '(not cl-functions obsolete))     ;; temporary

  :config
  (eval-when-compile (require 'cl)) ;; for `lexical-let' macro
  (defun compose-cl (f g)
    (lexical-let ((f f)
                  (g g))
      (lambda (x)
        (funcall f (funcall g x)))))
)

;; ----------------------------------------------------------------------
(use-package diminish
  :config
  (defmacro diminish-minor-mode (file mode &optional new-name)
    "https://github.com/larstvei/dot-emacs/blob/master/init.org"
    `(with-eval-after-load ,file
       (diminish ,mode ,new-name)))

  (defmacro diminish-major-mode (hook new-name)
    `(add-hook ,hook #'(lambda ()
                         (setq mode-name ,new-name))))

  ;; minor mode
  (diminish-minor-mode "undo-tree" 'undo-tree-mode)
  (diminish-minor-mode "eldoc" 'eldoc-mode)
  (diminish-minor-mode "abbrev" 'abbrev-mode)
  (diminish-minor-mode "cwarn" 'cwarn-mode)
  (diminish-minor-mode "company" 'company-mode)
  (diminish-minor-mode "super-save" 'super-save-mode)
  (diminish-minor-mode "git-gutter" 'git-gutter-mode)
  (diminish-minor-mode "ivy" 'ivy-mode)
  (diminish-minor-mode "yasnippet" 'yas-minor-mode)
  (diminish-minor-mode "symbol-overlay" 'symbol-overlay-mode)
  (diminish-minor-mode "hideif" 'hide-ifdef-mode)


  ;; major mode
  (diminish-major-mode 'emacs-lisp-mode-hook "Elisp")
  (diminish-major-mode 'lisp-interaction-mode-hook "LispInt")

  (defun flymake--transform-mode-line-format (ret)
    "Change the output of `flymake--mode-line-format'."
    (setf (seq-elt (car ret) 1) " ")
    (setf (seq-elt (second ret) 1) "")
    (setf (seq-elt (sixth ret) 1) "")
    ret)
  (advice-add #'flymake--mode-line-format
              :filter-return #'flymake--transform-mode-line-format)
)

;; ----------------------------------------------------------------------
(use-package undo-tree
  :config
  (define-key undo-tree-map (kbd "C-?") 'nil)
  (define-key undo-tree-map (kbd "C-r") 'nil)    ;; undo-tree-redo      FIXME: not work
  )

;; ----------------------------------------------------------------------
(use-package evil
  :init
  (setq evil-want-integration nil)
  (setq evil-kill-on-visual-paste nil)

  :config
  (evil-mode 1)
  (evil-set-initial-state 'help-mode 'emacs)
  (evil-set-initial-state 'slime-editing-mode 'emacs)

  (defalias #'forward-evil-word #'forward-evil-symbol)

  (evil-ex-define-cmd "q[uit]" #'kill-this-buffer)

  ;; インサートモードではEmacsキーバインド
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map [escape] #'evil-normal-state)

  ;; for package-mode
  (evil-add-hjkl-bindings package-menu-mode-map 'emacs
    (kbd "/")       'evil-search-forward
    (kbd "n")       'evil-search-next
    (kbd "N")       'evil-search-previous)

  ;; modeline
  (delq 'w32-ime-mode-line-state-indicator mode-line-format)
  (add-hook 'emacs-startup-hook (lambda ()
     (setq evil-mode-line-format '(before . mode-line-front-space))

     (let ((bg (face-background 'mode-line)))
       (setq evil-normal-state-tag   (propertize " N " 'face `((:background ,(mycolor 'blue)    :foreground ,bg :weight bold)))
             evil-emacs-state-tag    (propertize " E " 'face `((:background ,(mycolor 'orange)  :foreground ,bg :weight bold)))
             evil-insert-state-tag   (propertize " I " 'face `((:background ,(mycolor 'red)     :foreground ,bg :weight bold)))
             evil-motion-state-tag   (propertize " M " 'face `((:background ,(mycolor 'purple)  :foreground ,bg :weight bold)))
             evil-visual-state-tag   (propertize " V " 'face `((:background ,(mycolor 'green)   :foreground ,bg :weight bold)))
             evil-operator-state-tag (propertize " O " 'face `((:background ,(mycolor 'pink)    :foreground ,bg :weight bold)))))))

  ;; ----------
  ;; currently not use
  (defmacro define-key-evil-visual (vsel key cmd)
    ;; FIXME giving (kbd "c") to arg key occurs not work
    ;; FIXME giving function like (defun hoge (beg end) (interactive "r") ..) to arg cmd occurs not work
    `(define-key evil-visual-state-map ,key
       #'(lambda() (interactive)
           (if (eq evil-visual-selection ,vsel)
               (funcall ,cmd)
             ;; (funcall ,(lookup-key evil-visual-state-map (kbd "c")))))))
             (funcall ,(lookup-key evil-visual-state-map key))))))

  ;; ----------
  (defun my-adv-evil-change--without-kill-new (orig-fn beg end &optional type _ &rest args)
    "\"c\" (evil-change) without kill-new"
    (apply orig-fn beg end type ?_ args))

  (advice-add 'evil-change :around 'my-adv-evil-change--without-kill-new)

  ;; ----------
  (defun my-beginning-of-line ()
      (interactive)
      (if (bolp)
          (back-to-indentation)
          (beginning-of-line)))

  ;; ----------
  (defun my-end-of-line ()
    (interactive)
    (cl-labels ((my-eolp () (if (or (evil-normal-state-p) (evil-motion-state-p))
                                (= (1+ (point)) (line-end-position))
                              (eolp))))
      (if (my-eolp)
          (progn
            (beginning-of-line)
            (when (comment-search-forward (line-end-position) t)
              (re-search-backward comment-start-skip (line-beginning-position) t)
              (skip-syntax-backward "< " (line-beginning-position))))
        (end-of-line))))

  ;; ----------
  (defun my-copy-whole-buffer ()
    "Copy whole buffer to kill ring."
    (interactive)
    (kill-new (buffer-substring-no-properties (point-min) (point-max)))
    (message "Copied whole buffer to kill-ring"))

  ;; ----------
  ;; motion-state-map
  (define-key evil-motion-state-map (kbd "!") #'nop)                            ; unmap
  (define-key evil-motion-state-map (kbd "@") #'nop)                            ; unmap
  (define-key evil-motion-state-map (kbd "#") #'nop)                            ; unmap
  (define-key evil-motion-state-map (kbd "$") #'nop)                            ; unmap
  (define-key evil-motion-state-map (kbd "%") #'nop)                            ; unmap
  ;; (define-key evil-motion-state-map (kbd "^") #'nop)                            ; unmap
  (define-key evil-motion-state-map (kbd "&") #'nop)                            ; unmap
  (define-key evil-motion-state-map (kbd "*") #'nop)                            ; unmap
  (define-key evil-motion-state-map (kbd "(") #'nop)                            ; unmap
  (define-key evil-motion-state-map (kbd ")") #'nop)                            ; unmap
  (define-key evil-motion-state-map (kbd "3") #'evil-search-word-backward)      ; works as #
  (define-key evil-motion-state-map (kbd "8") #'evil-search-word-forward)       ; works as *

  (define-key evil-motion-state-map (kbd "i")   #'nop)                          ; unmap
  ;; (define-key evil-motion-state-map (kbd "V")   #'nop)                          ; unmap
  ;; (define-key evil-motion-state-map (kbd "C-v") #'nop)                          ; unmap
  (define-key evil-motion-state-map (kbd "M-v") #'nop)                          ; unmap
  (define-key evil-motion-state-map (kbd "C-f") nil)
  (define-key evil-motion-state-map (kbd "C-b") nil)
  (define-key evil-motion-state-map (kbd "C-o") nil)		; evil-jump-backward
  (define-key evil-motion-state-map (kbd "C-e") nil)
  (define-key evil-motion-state-map (kbd "C-d") nil)        ; evil-scroll-down
  (define-key evil-motion-state-map (kbd "'") nil)          ; evil-goto-mark-line
  (define-key evil-motion-state-map (kbd "\"") nil)         ; evil-use-register
  ;; (define-key evil-motion-state-map (kbd "C-p") nil)
  (define-key evil-motion-state-map (kbd "C-h") nil)
  (define-key evil-motion-state-map (kbd "C-y") nil)        ; evil-scrollline-up
  (define-key evil-motion-state-map (kbd "M-h") nil)

  (define-key evil-motion-state-map (kbd "m") #'evil-scroll-page-down)
  (define-key evil-motion-state-map (kbd "M") #'evil-scroll-page-up)
  (define-key evil-motion-state-map (kbd "j") #'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "k") #'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "0") #'my-beginning-of-line)
  (define-key evil-motion-state-map (kbd "4") #'my-end-of-line)
  (define-key evil-motion-state-map (kbd "6") #'evil-first-non-blank)
  ;; (define-key evil-motion-state-map (kbd "v") #'my-evil-visual-cycle)
  (define-key evil-motion-state-map (kbd "M-w") #'my-forward-word)
  ;; (define-key evil-motion-state-map (kbd "g g") #'my-evil-beginning-of-buffer)
  ;; (define-key evil-motion-state-map (kbd "g e") #'my-evil-end-of-buffer)
  (define-key evil-motion-state-map (kbd "g h") 'evil-jump-backward)
  ;; (define-key evil-motion-state-map (kbd "Y") #'my-evil-yank-whole-buffer)
  (define-key evil-motion-state-map (kbd "TAB") #'evil-indent-line)
  (define-key evil-motion-state-map "/" #'evil-search-forward)
  (define-key evil-motion-state-map "?" #'evil-search-backward)
  (define-key evil-motion-state-map (kbd ":") #'nop)        ; unmap :
  (define-key evil-motion-state-map (kbd ";") #'evil-ex)    ; works as :

  ;; normal-state-map
  (define-key evil-normal-state-map (kbd "q") nil)
  (define-key evil-normal-state-map (kbd "m") nil)
  (define-key evil-normal-state-map (kbd "M-.") nil)        ; evil-repeat-pop-next
  (define-key evil-normal-state-map (kbd "C-p") nil)        ; evil-paste-pop
  (define-key evil-normal-state-map (kbd "M-j") nil)        ; outline-move-sutree-*
  (define-key evil-normal-state-map (kbd "M-k") nil)        ; outline-move-sutree-*
  (define-key evil-normal-state-map (kbd "u") #'undo-tree-undo)
  (define-key evil-normal-state-map (kbd "U") #'undo-tree-redo)
  (define-key evil-normal-state-map (kbd "M-p") #'counsel-yank-pop)
  ;; (define-key evil-normal-state-map (kbd "SPC") #'evil-force-normal-state)
  (define-key evil-normal-state-map (kbd "g f") #'my-beginning-of-defun)
  (define-key evil-normal-state-map (kbd "A") #'nop)                 ; unmap A
  (define-key evil-normal-state-map (kbd "a") #'evil-append-line)    ; works as A
  (define-key evil-normal-state-map (kbd "1 1") #'show-overlay-and-prop-and-face-at)
  (define-key evil-normal-state-map (kbd "' a") #'my-copy-whole-buffer)
  (define-key evil-normal-state-map "x" 'delete-forward-char)       ; "x" command without kill-new
  (define-key evil-normal-state-map "X" 'delete-backward-char)      ; "X" command without kill-new

  ;; insert-state-map
  (define-key evil-insert-state-map (kbd "C-h") #'delete-backward-char)
  (define-key evil-insert-state-map (kbd "M-h") #'my-backward-kill-word)
  (define-key evil-insert-state-map (kbd "TAB") #'(lambda () (interactive) (insert-tab)))

  ;; visual-state-map
  (define-key evil-visual-state-map (kbd "e") #'my-evil-visual-eval-region)
  (define-key evil-visual-state-map (kbd "w") #'my-evil-visual-write-region)
  (define-key evil-visual-state-map (kbd "a") #'my-evil-visual-align-region)
  (define-key evil-visual-state-map (kbd "c") #'my-evil-visual-comment-region)
  (define-key evil-visual-state-map (kbd "i") #'my-evil-visual-indent-region)

  ;; ;; not work, fixme
  ;; (add-hook 'macrostep-mode-hook #'(lambda ()
  ;;   (evil-define-key 'normal macrostep-keymap (kbd "RET") 'macrostep-expand)
  ;;   (evil-define-key 'normal macrostep-keymap (kbd "=") 'macrostep-expand)
  ;;   (evil-define-key 'normal macrostep-keymap (kbd "e") 'macrostep-expand)
  ;;   (evil-define-key 'normal macrostep-keymap (kbd "DEL") 'macrostep-collapse)
  ;;   (evil-define-key 'normal macrostep-keymap (kbd "u") 'macrostep-collapse)
  ;;   (evil-define-key 'normal macrostep-keymap (kbd "c") 'macrostep-collapse)
  ;;   (evil-define-key 'normal macrostep-keymap (kbd "TAB") 'macrostep-next-macro)
  ;;   (evil-define-key 'normal macrostep-keymap (kbd "n") 'macrostep-next-macro)
  ;;   (evil-define-key 'normal macrostep-keymap (kbd "M-TAB") 'macrostep-prev-macro)
  ;;   (evil-define-key 'normal macrostep-keymap (kbd "p") 'macrostep-prev-macro)
  ;;   (evil-define-key 'normal macrostep-keymap (kbd "q") 'macrostep-collapse-all)
  ;;   (evil-define-key 'normal macrostep-keymap (kbd "C-c C-c") 'macrostep-collapse-all)))

  ;; ----------
  (defun my-evil-visual-eval-region (beg end)
    (interactive "r")
    (if (eq evil-visual-selection 'line)
        (progn
          (message "eval-region: %s"
                   (cond ((eq major-mode 'emacs-lisp-mode)
                          (eval-region beg end))
                         ((eq major-mode 'lisp-mode)
                          (slime-eval-region beg end))))
          (evil-exit-visual-state))
      (evil-forward-word-end)))

  (defun my-evil-visual-write-region (beg end)
    (interactive "r")
    (let ((fn (read-file-name "write-region:")))
      (write-region beg end fn)))

  (defun my-evil-visual-align-region (beg end)
    (interactive "r")
    (when (eq evil-visual-selection 'line)
      (align beg end)))

  (defun my-evil-visual-comment-region (beg end)
    (interactive "r")
    (if (eq evil-visual-selection 'line)
        (progn
          (comment-or-uncomment-region beg end)
          (evil-exit-visual-state))
      (evil-change beg end)))

  (defun my-evil-visual-indent-region (beg end)
    (interactive "r")
    (when (eq evil-visual-selection 'line)
      (indent-region beg end)
      (evil-exit-visual-state)))

  ;; ----------
  (defun my-evil-visual-narrow-to-region (beg end)
    "Narrow to region from BEG END."
    (when (or (eq evil-visual-selection 'char)
              (eq evil-visual-selection 'line))
      (narrow-to-region beg end)
      (evil-exit-visual-state)
      (my-evil-operator-narrow-to-region-mode 1)))

  (define-minor-mode my-evil-operator-narrow-to-region-mode
    "Buffer local minor mode of narrow-to-region operator for Evil."
    :lighter ""
    :keymap (make-sparse-keymap)
    :after-hook (when my-evil-operator-narrow-to-region-mode (goto-char (point-min)))
    (evil-normalize-keymaps)
    (defun my-evil-visual-narrow-to-region-exit ()
      (interactive)
      (widen)
      (my-evil-operator-narrow-to-region-mode 0)
      (evil-normal-state)))

    (evil-define-key 'normal my-evil-operator-narrow-to-region-mode-map
                             "q" #'my-evil-visual-narrow-to-region-exit)

  ;; ----------
  ;; (defun evil-return-insert-mode-after-save ()
  ;;   (when evil-insert-state-minor-mode
  ;;     (funcall (evil-escape--escape-normal-state))))

  ;; (add-hook 'after-save-hook #'evil-return-insert-mode-after-save)

  ;; ----------
  (defun my-evil-paste (&optional arg)
    (interactive  "P")
    (if (memq last-command '(evil-paste-before evil-paste-after))
        (call-interactively #'evil-paste-pop)
      (call-interactively (if arg #'evil-paste-before #'evil-paste-after))))
;;  (define-key evil-normal-state-map (kbd "p") #'my-evil-paste)

  ;; ----------
  (defun my-evil-search-dummy-func ()
    (remove-hook 'isearch-mode-hook #'my-evil-search-dummy-func)
    (setq unread-command-events (listify-key-sequence (kbd "RET"))))

  (defun my-evil-search-dummy ()
    "workaround for `my-evil-search-from-region-next'. swapping search direction is prevent by calling this function prior 'my-evil-search-from-region-next'."
    (add-hook 'isearch-mode-hook #'my-evil-search-dummy-func)
    (call-interactively 'evil-search-forward))

  (defvar my-evil-search-first-time t)

  (defun my-evil-search-from-region-next (beg end)
    "under the evil-visual-state, jump next immediately after selecting region and pressing specified key."
    (when my-evil-search-first-time
      (my-evil-search-dummy)
      (setq my-evil-search-first-time nil))
    (my-evil-search-from-region-1 beg end t))

  (defun my-evil-search-from-region-prev (beg end)
    "under the evil-visual-state, jump previous immediately after selecting region and pressing specified key."
    (interactive "r")
    (unless (save-excursion (goto-char beg) (search-forward "\n" end t))
      (when my-evil-search-first-time
        (my-evil-search-dummy)
        (setq my-evil-search-first-time nil))
      (my-evil-search-from-region-1 beg end nil)))

  (defun my-evil-search-from-region-1 (beg end forward-p)
    "pull string from region as search string then jump"
    (let ((s (buffer-substring-no-properties beg end)))
      (delete-duplicates (push s (if evil-regexp-search regexp-search-ring search-ring))
                         :test 'string= :from-end t)
      (evil-normal-state nil)
      (evil-search s forward-p)))

  (defun my-evil-visual-narrow-or-jump (beg end)
    (interactive "r")
    (if (save-excursion (goto-char beg) (search-forward "\n" end t)) ;; multiple lines?
        (my-evil-visual-narrow-to-region beg end)
      (my-evil-search-from-region-next beg end)))

  (define-key evil-visual-state-map (kbd "n") #'my-evil-visual-narrow-or-jump)
  (define-key evil-visual-state-map (kbd "N") #'my-evil-search-from-region-prev)

  ;; ----------
  (defvar my-evil-visual-surround-paired '((?\" . ?\") (?\' . ?\') (?\( . ?\)) (?\{ . ?\}) (?\[ . ?\]) (?\< . ?>)))
  (defun my-evil-visual-surround-add (beg end)
    "Surround selected string with specified character."
    (let* ((c (read-char "Surround with:"))
           (pair (my-evil-visual-surround-get-pair c))
           head tail)
      (if pair
          (setq head (car pair) tail (cdr pair))
        (setq head c tail c))
      (save-excursion
        (goto-char end)
        (insert (char-to-string tail))
        (goto-char beg)
        (insert (char-to-string head)))))

  (defun my-evil-visual-surround-change (beg end)
    "Change surrounded character. Delete surround if you input RET."
    (let* ((c (read-char "Re-surround with:"))
           (pair (my-evil-visual-surround-get-pair c))
           head tail)
      (if pair
          (setq head (car pair) tail (cdr pair))
        (setq head c tail c))
    (save-excursion
      (goto-char (1- end))
      (delete-char 1)
      (unless (eq tail #xd) (insert (char-to-string tail)))
      (goto-char beg)
      (delete-char 1)
      (unless (eq head #xd) (insert (char-to-string head))))))

  (defun my-evil-visual-surround-get-tail (head)
    (cdr (assoc head my-evil-visual-surround-paired)))

  (defun my-evil-visual-surround-get-pair (head-or-tail)
    (or (assoc head-or-tail my-evil-visual-surround-paired)
        (rassoc head-or-tail my-evil-visual-surround-paired)))

  (defun my-evil-visual-surround (beg end)
    (interactive "r")
    (let* ((s (buffer-substring-no-properties beg end))
           (head (aref s 0))
           (tail (aref s (1- (length s)))))
      (cond ((or (eq head tail) (eq tail (my-evil-visual-surround-get-tail head)))
             (my-evil-visual-surround-change beg end))
            (t (my-evil-visual-surround-add beg end)))))

  (define-key evil-visual-state-map "s" 'my-evil-visual-surround)

  ;; ----------
  ;; (defun my-evil-beginning-of-buffer ()
  ;;   (interactive)
  ;;   (if (eq last-command this-command)
  ;;       (exchange-point-and-mark t)
  ;;     (push-mark (point) t)
  ;;     (goto-char (point-min))))

  ;; (defun my-evil-end-of-buffer ()
  ;;   (interactive)
  ;;   (if (eq last-command this-command)
  ;;       (exchange-point-and-mark t)
  ;;     (push-mark (point) t)
  ;;     (goto-char (point-max))))

  (defun my-evil-yank-whole-buffer ()
    (interactive)
    (evil-yank-line (point-min) (point-max))
    (message "Copied whole buffer"))

  ;; ----------
  (defvar-local my-evil-visual-cycle-state nil)

  (lexical-let (pos-init)
    (defun my-evil-visual-cycle ()
      "Cycle evil-visual like: V-CHAR -> V-LINE -> V-BLOCK -> V-CHAR ..."
      (interactive)
      (cl-labels ((= (state) (eq my-evil-visual-cycle-state state))
                  (-> (state) (setq my-evil-visual-cycle-state state))
                  (set-tag (s) (setq evil-visual-state-tag (propertize (concat " " s " ") 'face
             `((:background ,(mycolor 'green) :foreground ,(face-background 'mode-line) :weight bold))))))
        (cond ((= nil) (-> 'char)
               (setq pos-init (point))
               (evil-visual-char) (set-tag "VISUAL"))
              ((= 'char) (-> 'line)
               (evil-visual-contract-region)
               (evil-visual-line) (set-tag "V-LINE"))
              ((= 'line) (-> 'block)
               (evil-visual-make-region pos-init evil-visual-point 'block)
               (add-hook 'minibuffer-setup-hook #'my-evil-visual-cycle-emulate-evil-block)
               (let ((suggest-key-bindings nil))
                 (call-interactively 'execute-extended-command))
               (evil-visual-block) (set-tag "V-BLOCK"))
              ((= 'block) (-> 'char)
               (evil-visual-contract-region)
               (evil-visual-char) (set-tag "VISUAL"))
              (t (error "Invalid state: %s" my-evil-visual-cycle-state))))))

  (add-hook 'evil-visual-state-exit-hook #'(lambda () (setq-local my-evil-visual-cycle-state nil)))
  (defun my-evil-visual-cycle-emulate-evil-block ()
    (remove-hook 'minibuffer-setup-hook #'my-evil-visual-cycle-emulate-evil-block)
    (insert "evil-visual-block")
    (setq unread-command-events (listify-key-sequence (kbd "RET"))))

  ;; ----------
  (lexical-let (so)
    (defun my-evil-visual-state-entry-hook-symbol-overlay-off ()
      (setq so (if symbol-overlay-mode +1 -1))
      (symbol-overlay-mode -1))
    (defun my-evil-visual-state-exit-hook-symbol-overlay-on ()
      (symbol-overlay-mode so)))

  (add-hook 'evil-visual-state-entry-hook #'my-evil-visual-state-entry-hook-symbol-overlay-off)
  (add-hook 'evil-visual-state-exit-hook #'my-evil-visual-state-exit-hook-symbol-overlay-on)

  ;; ----------
  (defvar my-evil-paste-rgn '())

  (defun my-adv-evil-paste-before--save-rgn (orig-fun &rest _arg)
    (let ((beg (point)))
      (apply orig-fun _arg)
      (setq my-evil-paste-rgn (cons (1+ (point)) beg))))

  (defun my-adv-evil-paste-after--save-rgn (orig-fun &rest _arg)
    (let ((beg (point)))
      (apply orig-fun _arg)
      (setq my-evil-paste-rgn (cons beg (1+ (point))))))

  (defun my-adv-counsel-yank-pop--oeverwrite (orig-fun &rest _arg)
    "Delete the region before inserting poped string."
    (cond ((and evil-mode (eq 'visual evil-state))
           (let ((beg (copy-marker (region-beginning) t))
                 (end (copy-marker (region-end) t)))
             (apply orig-fun _arg)
             (delete-region beg end)))
          ((and evil-mode
                (or (eq last-command 'evil-paste-before) (eq last-command 'evil-paste-after)))
           (goto-char (line-end-position))
           (apply orig-fun _arg)
           (delete-region (car my-evil-paste-rgn) (cdr my-evil-paste-rgn)))
          (t (apply orig-fun _arg))))

  (advice-add 'evil-paste-before :around #'my-adv-evil-paste-before--save-rgn)
  (advice-add 'evil-paste-after  :around #'my-adv-evil-paste-after--save-rgn)
  (advice-add 'counsel-yank-pop  :around #'my-adv-counsel-yank-pop--oeverwrite)

  ;; ----------
  ;; exclude new line when v$
  ;; https://github.com/emacs-evil/evil/issues/897
  (setq evil-v$-gets-eol nil)

  ;; re-difined
  (evil-define-motion evil-end-of-line (count)
    "Move the cursor to the end of the current line.
If COUNT is given, move COUNT - 1 lines downward first."
    :type inclusive
    (move-end-of-line count)
    (when evil-track-eol
      (setq temporary-goal-column most-positive-fixnum
            this-command 'next-line))
    ;; (unless (evil-visual-state-p)
    (unless (and (evil-visual-state-p) evil-v$-gets-eol)    ;; mod
      (evil-adjust-cursor)
      (when (eolp)
        ;; prevent "c$" and "d$" from deleting blank lines
        (setq evil-this-type 'exclusive))))

  ;; ----------
  (add-hook 'evil-visual-state-entry-hook #'(lambda () (show-paren-mode -1)))
  (add-hook 'evil-visual-state-exit-hook  #'(lambda () (show-paren-mode 1)))
)

;; ----------------------------------------------------------------------
(use-package evil-collection
  ;; :disabled
  :after evil
  :config
  ;; (evil-collection-init '(edebug dired neotree slime help re-builder)) ;; fixme
  (evil-collection-init '(edebug dired neotree slime help))

  (evil-define-key 'normal help-mode-map (kbd "C-o") 'other-window)
  (evil-define-key 'normal help-mode-map (kbd "C-0") 'delete-window)
  )

;; ----------------------------------------------------------------------
(use-package evil-escape
  :disabled
  :after evil
  :diminish evil-escape-mode
  :config
  (evil-escape-mode 1)
  (setq-default evil-esc-delay 0)
  (setq-default evil-escape-delay 0.3)
  (setq-default evil-escape-key-sequence "jj")
  (setq-default evil-escape-excluded-states '(normal visual multiedit emacs motion))
  )

;; ----------------------------------------------------------------------
(use-package evil-lion
  :ensure t
  :after evil
  :config
  (evil-lion-mode)
)

;; ----------------------------------------------------------------------
(use-package evil-matchit
  :ensure t
  :after evil
  :config
  (setq evilmi-shortcut "]")
  (global-evil-matchit-mode 1)

  )

;; ----------------------------------------------------------------------
(use-package common-header-mode-line
  :disabled
  :config
  (common-mode-line-mode 1)
  (common-header-line-mode 1)
  ;; (setq common-header-mode-line-update-delay 0.5)
  )

;; ----------------------------------------------------------------------
(use-package solarized-matreal-dark-theme
  :disabled
  :load-path "~/.emacs.d/themes"
  :config
  (load-theme 'solarized-matreal-dark t)
  )
;; ----------------------------------------------------------------------
(use-package my-zerodark-theme
  ;; :disabled
  :load-path "~/.emacs.d/themes"
  :config
  (load-theme 'my-zerodark t)

  ;; (global-tab-line-mode 1) ; for emacs27
  )

;; ----------------------------------------------------------------------
(use-package tabbar
  ;; :disabled
  :hook ((after-save   . tabbar-on-saving-buffer)
         (first-change . tabbar-on-modifying-buffer))
  :config
  (tabbar-mode)

  (defun my-adv-load-theme--tabbar-reset-appearance (&rest _)
  (set-face-attribute 'tabbar-default nil
                      :height 0.9
                      :family (myfont 'ui)
                      :background (face-background 'mode-line)
                      :slant 'normal
                      :weight 'light
                      :box nil
                      :overline (face-background 'mode-line)
                      )

  (set-face-attribute 'tabbar-selected nil
                      ;; :inherit 'tabbar-default
                      :foreground (face-background 'mode-line)
                      :background (face-foreground 'line-number-current-line)
                      :slant 'normal
                      :weight 'light
                      :box nil
                      :overline (face-foreground 'line-number-current-line)
                      )

  (set-face-attribute 'tabbar-unselected nil
                      ;; :inherit 'tabbar-default
                      :background (face-foreground 'tabbar-selected)
                      :foreground (face-background 'tabbar-selected)
                      :slant 'normal
                      :weight 'light
                      :box nil
                      :overline (face-foreground 'tabbar-selected)
                      )

  (set-face-attribute 'tabbar-selected-modified nil
                      ;; :inherit 'tabbar-default
                      :background (face-background 'tabbar-selected)
                      :foreground (face-foreground 'tabbar-selected)
                      :slant 'normal
                      :weight 'light
                      :box nil
                      :overline "orange"
                      )

  (set-face-attribute 'tabbar-modified nil
                      ;; :inherit 'tabbar-default
                      :background (face-attribute 'tabbar-unselected :background)
                      :foreground (face-attribute 'tabbar-unselected :foreground)
                      :slant 'normal
                      :weight 'light
                      :box nil
                      :overline "orange"
                      )

  (set-face-attribute 'tabbar-separator nil
                      ;; :inherit 'tabbar-default
                      :background (face-attribute 'tabbar-selected :background))

  ;; (setq tabbar-separator '(0.2))
  )

  (my-adv-load-theme--tabbar-reset-appearance)
  (advice-add 'load-theme :after #'my-adv-load-theme--tabbar-reset-appearance)

  (global-set-key (kbd "M-j") 'tabbar-backward-tab)
  (global-set-key (kbd "M-k") 'tabbar-forward-tab)

  (tabbar-mwheel-mode nil)                  ;; マウスホイール無効
  (setq tabbar-buffer-groups-function nil)  ;; グループ無効             ;; モードラインがちらつく原因
  (setq tabbar-buffer-groups-function '(lambda () (list "")))
  (setq tabbar-use-images nil)              ;; 画像を使わない

  ;;----- 左側のボタンを消す
  (dolist (btn '(tabbar-buffer-home-button
                 tabbar-scroll-left-button
                 tabbar-scroll-right-button))
    (set btn (cons (cons "" nil)
                   (cons "" nil))))

  (defun my-tabbar-buffer-list ()
    (delq nil
          (mapcar #'(lambda (b)
                      (cond
                       ;; Always include the current buffer.
                       ((eq (current-buffer) b) b)
                       ((string= (buffer-name b) (file-name-nondirectory org-default-notes-file)) nil)  ; hide "notes.org"
                       ((string-match "^CAPTURE-[0-9]*-*.+\.org$" (buffer-name b)) nil)   ; hide org-capture
                       ((buffer-file-name b) b)
                       ((char-equal ?\  (aref (buffer-name b) 0)) nil)
                       ((equal "*scratch*" (buffer-name b)) b)              ; *scratch*バッファは表示する
                       ((char-equal ?* (aref (buffer-name b) 0)) nil)       ; それ以外の * で始まるバッファは表示しない
                       ((string-match "^magit" (buffer-name b)) nil)        ; magit が開くバッファは表示しない
                       ((buffer-live-p b) b)))
                  (buffer-list))))

  (setq tabbar-buffer-list-function 'my-tabbar-buffer-list)

  ;; mod
  (defun tabbar-buffer-tab-label (tab)
    "Return a label for TAB.
That is, a string used to represent it on the tab bar."
    (let ((label  (if tabbar--buffer-show-groups
                      (format " [%s] " (tabbar-tab-tabset tab))
                    (format " %s " (tabbar-tab-value tab)))))
      ;; Unless the tab bar auto scrolls to keep the selected tab
      ;; visible, shorten the tab label to keep as many tabs as possible
      ;; in the visible area of the tab bar.
      (if tabbar-auto-scroll-flag
          label
        (tabbar-shorten
         label (max 1 (/ (window-width)
                         (length (tabbar-view
                                  (tabbar-current-tabset)))))))))

  (defun tabbar-on-saving-buffer ()
    (tabbar-set-template tabbar-current-tabset nil)
    (tabbar-display-update))

  (defun tabbar-on-modifying-buffer ()
    (set-buffer-modified-p (buffer-modified-p))
    (tabbar-set-template tabbar-current-tabset nil)
    (tabbar-display-update))

  (defun tabbar-after-modifying-buffer (begin end length)
    (set-buffer-modified-p (buffer-modified-p))
    (tabbar-set-template tabbar-current-tabset nil)
    (tabbar-display-update))

  )

;; ----------------------------------------------------------------------
(use-package centaur-tabs
  :disabled
  :demand
  :config
  (setq centaur-tabs-set-close-button nil
        centaur-tabs-set-icons nil
        centaur-tabs-set-modified-marker nil
        centaur-tabs-left-edge-margin nil
        centaur-tabs-right-edge-margin nil)

  (set-face-attribute 'centaur-tabs-default nil
                      :height 0.8
                      :family (myfont 'ui)
                      :foreground (face-background 'mode-line)
                      :background (face-background 'mode-line)
                      :slant 'normal
                      :weight 'normal
                      :box nil
                      :overline (face-background 'mode-line)
                      )
  (set-face-attribute 'centaur-tabs-selected nil
                      :foreground (face-background 'mode-line)
                      :background (face-foreground 'line-number-current-line)
                      :slant 'normal
                      :weight 'normal
                      :box nil
                      :overline (face-foreground 'line-number-current-line)
                      )
  (set-face-attribute 'centaur-tabs-unselected nil
                      :background (face-foreground 'centaur-tabs-selected)
                      :foreground (face-background 'centaur-tabs-selected)
                      :slant 'normal
                      :weight 'normal
                      :box nil
                      :overline (face-foreground 'centaur-tabs-selected)
                      )
  (set-face-attribute 'centaur-tabs-selected-modified nil
                      ;; :inherit 'tabbar-default
                      :background (face-background 'centaur-tabs-selected)
                      :foreground (face-foreground 'centaur-tabs-selected)
                      :slant 'normal
                      :weight 'normal
                      :box nil
                      :overline '(:color "orange" :style 'wave)
                      ;; :overline "orange"
                      )
  (set-face-attribute 'centaur-tabs-unselected-modified nil
                      ;; :inherit 'tabbar-default
                      :background (face-background 'centaur-tabs-unselected)
                      :foreground (face-foreground 'centaur-tabs-unselected)
                      :slant 'normal
                      :weight 'normal
                      :box nil
                      :overline '(:color "orange" :style 'wave)
                      ;; :overline "orange"
                      )
  (set-face-attribute 'centaur-tabs-modified-marker-selected nil
                      :background (face-foreground 'centaur-tabs-selected)
                      :foreground (face-background 'centaur-tabs-selected)
                      :slant 'normal
                      :weight 'normal
                      :box nil
                      :overline '(:color "orange" :style 'wave)
                      )

  (setq centaur-tabs-buffer-groups-function '(lambda () (list "-")))
  ;; (setq centaur-tabs-buffer-groups-function nil)

  (defun my-centaur-tabs-hide-func (b)
    (not (cond
          ;; Always include the current buffer.
          ((eq (current-buffer) b) b)
          ((string= (buffer-name b) (file-name-nondirectory org-default-notes-file)) nil)  ; hide "notes.org"
          ((string-match "^CAPTURE-[0-9]*-*.+\.org$" (buffer-name b)) nil)   ; hide org-capture
          ((buffer-file-name b) b)
          ((char-equal ?\  (aref (buffer-name b) 0)) nil)
          ((equal "*scratch*" (buffer-name b)) b)              ; *scratch*バッファは表示する
          ((char-equal ?* (aref (buffer-name b) 0)) nil)       ; それ以外の * で始まるバッファは表示しない
          ((string-match "^magit" (buffer-name b)) nil)        ; magit が開くバッファは表示しない
          ((buffer-live-p b) b)
          (t b))))

  (setq centaur-tabs-hide-tab-function 'my-centaur-tabs-hide-func)

  ;; from tabbar
  (defun centaur-shorten (str width)
    "Return a shortened string from STR that fits in the given display WIDTH.
WIDTH is specified in terms of character display width in the current
buffer; see also `char-width'.  If STR display width is greater than
WIDTH, STR is truncated and an ellipsis string \"...\" is inserted at
end or in the middle of the returned string, depending on available
room."
    (let* ((n  (length str))
           (sw (string-width str))
           (el "...")
           (ew (string-width el))
           (w  0)
           (i  0))
      (cond
       ;; STR fit in WIDTH, return it.
       ((<= sw width)
        str)
       ;; There isn't enough room for the ellipsis, STR is just
       ;; truncated to fit in WIDTH.
       ((<= width ew)
        (while (< w width)
          (setq w (+ w (char-width (aref str i)))
                i (1+ i)))
        (substring str 0 i))
       ;; There isn't enough room to insert the ellipsis in the middle
       ;; of the truncated string, so put the ellipsis at end.
       ((zerop (setq sw (/ (- width ew) 2)))
        (setq width (- width ew))
        (while (< w width)
          (setq w (+ w (char-width (aref str i)))
                i (1+ i)))
        (concat (substring str 0 i) el))
       ;; Put the ellipsis in the middle of the truncated string.
       (t
        (while (< w sw)
          (setq w (+ w (char-width (aref str i)))
                i (1+ i)))
        (setq w (+ w ew))
        (while (< w width)
          (setq n (1- n)
                w (+ w (char-width (aref str n)))))
        (concat (substring str 0 i) el (substring str n)))
       )))

  ;; mod
  (defun centaur-tabs-buffer-tab-label (tab)
    "Return a label for TAB.
That is, a string used to represent it on the tab bar."
    (let ((label  (if centaur-tabs--buffer-show-groups
                      (format " [%s] " (centaur-tabs-tab-tabset tab))
                    (format " %s " (centaur-tabs-tab-value tab)))))
      ;; Unless the tab bar auto scrolls to keep the selected tab
      ;; visible, shorten the tab label to keep as many tabs as possible
      ;; in the visible area of the tab bar.
      (if centaur-tabs-auto-scroll-flag
          label
        (centaur-shorten
         label (max 1 (/ (window-width)
                         (length (centaur-tabs-view
                                  (centaur-tabs-current-tabset)))))))))

  ;; (defun tabbar-on-saving-buffer () centaur-tabs-line-tab)

  (centaur-tabs-headline-match)
  (centaur-tabs-mode t)

  :bind
  ("M-j" . centaur-tabs-backward)
  ("M-k" . centaur-tabs-forward)
  )

;; ----------------------------------------------------------------------
(use-package doom-modeline
  :disabled
  :ensure t
  :after evil
  :hook (after-init . doom-modeline-mode)

  :custom
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-height 18)

  :config
  (set-face-attribute 'doom-modeline-project-dir nil :foreground (mycolor 'blue) :weight 'light)
  (set-face-attribute 'doom-modeline-buffer-file nil :foreground (mycolor 'blue) :weight 'bold)

  (let ((bg (face-background 'mode-line)))
    (setq evil-normal-state-tag   (propertize " NORMAL " 'face `((:background ,(mycolor 'blue)    :foreground ,bg :weight bold)))
          evil-emacs-state-tag    (propertize " EMACS  " 'face `((:background ,(mycolor 'orange)  :foreground ,bg :weight bold)))
          evil-insert-state-tag   (propertize " INSERT " 'face `((:background ,(mycolor 'red)     :foreground ,bg :weight bold)))
          evil-motion-state-tag   (propertize " MOTION " 'face `((:background ,(mycolor 'purple)  :foreground ,bg :weight bold)))
          evil-visual-state-tag   (propertize " VISUAL " 'face `((:background ,(mycolor 'green)   :foreground ,bg :weight bold)))
          evil-operator-state-tag (propertize " OPERATOR " 'face `((:background ,(mycolor 'pink)    :foreground ,bg :weight bold)))))

  (doom-modeline-def-segment evil-state
    "The current evil state.  Requires `evil-mode' to be enabled."
    (when (bound-and-true-p evil-local-mode)
      ;; (s-trim-right (evil-state-property evil-state :tag t))))
      (when (doom-modeline--active)
          (evil-state-property evil-state :tag t))))

  (doom-modeline-def-segment linum-colnum
    "Display current linum/colnum"
    (propertize (format " %4s/%d,%-3s"
                        (format-mode-line "%l")
                        (line-number-at-pos (point-max))
                        (format-mode-line "%c"))))

  ;; ;; fixme, doesn't work
  ;; (doom-modeline-def-segment linum-colnum
  ;;   "Display current linum/colnum"
  ;;   (if (and (bound-and-true-p evil-local-mode) (eq 'visual evil-state))
  ;;       (prog1
  ;;         (cond ((eq (evil-visual-type) 'block)
  ;;                (format " [H%4d, W%3d]" (count-lines (region-beginning) (min (1+ (region-end)) (point-max)))
  ;;                        (1+ (abs (- (save-excursion (goto-char (region-beginning)) (current-column))
  ;;                                    (save-excursion (goto-char (region-end)) (current-column)))))))
  ;;               ((eq (evil-visual-type) 'line)
  ;;                (format " [LINE %-4d]  " (count-lines (region-beginning) (min (1+ (region-end)) (point-max)))))
  ;;               (t
  ;;                (format " [CHAR %-4d]  "
  ;;                        (1+ (abs (- (region-beginning) (region-end)))))))
  ;;         (force-mode-line-update t))
  ;;     (format " %4s/%d,%-3s"
  ;;                         (format-mode-line "%l")
  ;;                         (line-number-at-pos (point-max))
  ;;                         (format-mode-line "%c"))))

  ;; mod
  (defun doom-modeline-update-buffer-file-state-icon (&rest _)
  "Update the buffer or file state in mode-line."
  (setq doom-modeline--buffer-file-state-icon
        (when doom-modeline-buffer-state-icon
          (ignore-errors
            (concat
             (cond (buffer-read-only
                    (doom-modeline-buffer-file-state-icon
                  ;; "lock" "🔒" "%1*" `(:inherit doom-modeline-warning
                     "lock" "🔒" "%1*" `(:inherit doom-modeline-buffer-modified
                                         :weight ,(if doom-modeline-icon
                                                      'normal
                                                    'bold))))
                   ((and buffer-file-name (buffer-modified-p)
                         doom-modeline-buffer-modification-icon)
                    (doom-modeline-buffer-file-state-icon
                     "save" "💾" "%1*" `(:inherit doom-modeline-buffer-modified
                                         :weight ,(if doom-modeline-icon
                                                      'normal
                                                    'bold))))
                   ((and buffer-file-name
                         (not (file-exists-p buffer-file-name)))
                    (doom-modeline-buffer-file-state-icon
                     "do_not_disturb_alt" "🚫" "!" 'doom-modeline-urgent))
                   (t ""))
             ;; add
             (when (eq major-mode 'org-mode)
               (doom-modeline-icon 'material
                (cond ((eq my-org-global-fold-cycle-state 'hide-all) "more_horiz")
                      ((eq my-org-global-fold-cycle-state 'show-all) "format_align_left")
                      (t "person"))
                   "↕" "><" :face 'doom-modeline-warning :height  1.1 :v-adjust -0.3))
             (when (or (buffer-narrowed-p)
                       (and (bound-and-true-p fancy-narrow-mode)
                            (fancy-narrow-active-p))
                       (bound-and-true-p dired-narrow-mode))
               (doom-modeline-buffer-file-state-icon
                "unfold_less" "↕" "><" 'doom-modeline-warning)))))))

  ;; mod
  (doom-modeline-def-segment buffer-info
  "Combined information about the current buffer, including the current working
directory, the file name, and its state (modified, read-only or non-existent)."
  (concat
   (doom-modeline-spc)
   (doom-modeline--buffer-mode-icon)
   (doom-modeline--buffer-name)
   (doom-modeline--buffer-state-icon)))

  ;; mod
  (doom-modeline-def-segment buffer-encoding
  "Displays the eol and the encoding style of the buffer the same way Atom does."
  (when doom-modeline-buffer-encoding
    (let ((face (if (doom-modeline--active) 'mode-line 'mode-line-inactive))
          (eouse-face 'mode-line-highlight))
      (concat
       (doom-modeline-spc)

       ;; coding system
       (propertize
        (let ((sys (coding-system-plist buffer-file-coding-system)))
          (cond ((memq (plist-get sys :category)
                       '(coding-category-undecided coding-category-utf-8))
                 "UTF-8")
                (t (upcase (symbol-name (plist-get sys :name))))))
        'face face
        ;; 'mouse-face mouse-face
        ;; 'help-echo 'mode-line-mule-info-help-echo
        ;; 'local-map mode-line-coding-system-map
        )

       ;; eol type
       (let ((eol (coding-system-eol-type buffer-file-coding-system)))
         (propertize
          (pcase eol
            (0 "/LF")
            (1 "/CRLF")
            (2 "/CR")
            (_ ""))
          'face face
          ;; 'mouse-face mouse-face
          ;; 'help-echo (format "End-of-line style: %s\nmouse-1: Cycle"
          ;;                    (pcase eol
          ;;                      (0 "Unix-style LF")
          ;;                      (1 "DOS-style CRLF")
          ;;                      (2 "Mac-style CR")
          ;;                      (_ "Undecided")))
          ;; 'local-map (let ((map (make-sparse-keymap)))
		  ;;              (define-key map [mode-line mouse-1] 'mode-line-change-eol)
		  ;;              map)
          ))

       ))))

  (doom-modeline-def-modeline 'main
    ;; '(bar workspace-number window-number evil-state god-state ryo-modal xah-fly-keys matches buffer-info remote-host buffer-position parrot selection-info)
    ;; '(misc-info persp-name lsp github debug minor-modes input-method major-mode process vcs checker))
    '(evil-state matches buffer-info remote-host parrot)
    '(misc-info persp-name lsp github debug buffer-encoding linum-colnum minor-modes major-mode vcs))

  (defun my-minor-modes-toggle ()
    (interactive)
    (setq doom-modeline-minor-modes (if doom-modeline-minor-modes nil t)))
  ;; (remove-text-properties )

  )

;; ----------------------------------------------------------------------
(use-package hide-mode-line
  :hook ((neotree-mode) . hide-mode-line-mode)
  )

;; ----------------------------------------------------------------------
(use-package dashboard
  :disabled
  ;; :defer t
  :config
  (setq inhibit-startup-message t)
  (setq dashboard-banner-logo-title "Life with Evil")
  (setq dashboard-startup-banner "~/.emacs.d/img/e_splash.svg")
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 10)))
  ;; (widget-forward 1)
  )

;; ----------------------------------------------------------------------
(use-package all-the-icons
  :config
  (setq inhibit-compacting-font-caches t)
  (setq all-the-icons-color-icons nil)
  )

;; ----------------------------------------------------------------------
(use-package neotree
  ;; :disabled
  :after evil all-the-icons
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-show-hidden-files t)
  (setq neo-create-file-auto-open t)
  (setq neo-smart-open nil)
  (setq neo-persist-show t)
  (setq neo-keymap-style 'concise)

  (global-set-key (kbd "M-q") 'neotree-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "M-q") 'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "q")   'neotree-hide)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "l")   'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)

  (defun text-scale-twice ()
    (interactive)
    (text-scale-adjust 0)
    (text-scale-decrease 1))
  (add-hook 'neo-after-create-hook (lambda (_)(call-interactively 'text-scale-twice)))

  )

;; ----------------------------------------------------------------------
(use-package ivy
  :diminish counsel-mode
  :init
  (ivy-mode 1)

  :config
  (counsel-mode 1)
  (setq ivy-use-virtual-buffers t                                ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
        ivy-height 20
        ivy-initial-inputs-alist nil                             ;; no regexp by default
        ivy-on-del-error-function 'ignore
        ivy-extra-directories nil                                ;; '("../")
        ivy-re-builders-alist '((t . ivy--regex-ignore-order))   ;; configure regexp engine. allow input not in order
        avy-timeout-seconds 0.4
        counsel-find-file-ignore-regexp "\\.ex4$\\|\\.elc\\'\\|\\.DS_Store\\|^#.+#$"
        ;; ivy-display-style t
  )

  ;; color
  (set-face-foreground 'ivy-action (mycolor 'red))
  (set-face-background 'ivy-confirm-face "'green")
  ;; (set-face-background 'ivy-current-match "#0a5770")
  (set-face-attribute  'ivy-current-match nil
                    :foreground (mycolor 'black) :background (mycolor 'red))
  (set-face-background 'ivy-cursor "'brown")
  (set-face-background 'ivy-highlight-face "'SkyBlue")
  (set-face-background 'ivy-match-required-face "#ce123e")

  ;; (set-face-background 'ivy-minibuffer-match-face-1 "#cc8800")
  ;; (set-face-background 'ivy-minibuffer-match-face-2 "#0a5770")
  ;; (set-face-background 'ivy-minibuffer-match-face-3 "'DarkGray")
  ;; (set-face-background 'ivy-minibuffer-match-face-4 "'DarkCyan")
  (set-face-attribute 'ivy-minibuffer-match-face-1 nil :foreground nil :background nil :bold t :underline t)
  (copy-face 'ivy-minibuffer-match-face-1 'ivy-minibuffer-match-face-2)
  (copy-face 'ivy-minibuffer-match-face-1 'ivy-minibuffer-match-face-3)
  (copy-face 'ivy-minibuffer-match-face-1 'ivy-minibuffer-match-face-4)

  ;; disable mouse hover in minibuffer
  ;; mod
  (defun ivy--format-minibuffer-line (str)
  "Format line STR for use in minibuffer."
  (let* ((str (ivy-cleanup-string str))
         (str (if (eq ivy-display-style 'fancy)
                  (funcall ivy--highlight-function (copy-sequence str))
                (copy-sequence str))))
    ;; (add-text-properties
    ;;  0 (length str)
    ;;  '(mouse-face
    ;;    ivy-minibuffer-match-highlight
    ;;    help-echo
    ;;    (format
    ;;     (if tooltip-mode
    ;;         "mouse-1: %s\nmouse-3: %s"
    ;;       "mouse-1: %s   mouse-3: %s")
    ;;     ivy-mouse-1-tooltip ivy-mouse-3-tooltip))
    ;;  str)
    (let ((annotation-function (plist-get completion-extra-properties :annotation-function)))
      (if annotation-function
          (concat str (funcall annotation-function str))
        str))))

  (copy-face 'ivy-current-match 'ivy-prompt-match)
  (set-face-background 'ivy-modified-buffer "#008800")
  (set-face-foreground 'ivy-remote (mycolor 'pink))
  (set-face-foreground 'ivy-subdir (mycolor 'blue))
  (set-face-foreground 'ivy-virtual (mycolor 'orange))

  (defalias 'list-faces 'counsel-faces)
  (fset 'list-faces-display nil)

  (defun my-ivy-done ()
    (interactive)
    (if (and (boundp 'my-ivy-immediate-flag) (eq my-ivy-immediate-flag t))
        (ivy-immediate-done)
      (ivy-done)))

  (define-key ivy-minibuffer-map [(return)] 'my-ivy-done)

  (defun my-counsel-find-file ()
    (interactive)
    (let ((my-ivy-immediate-flag t))
      (call-interactively
       (cond ((and (fboundp 'counsel-gtags-find-file) (locate-dominating-file default-directory "GTAGS"))
              'counsel-gtags-find-file)
             ((and (fboundp 'magit-find-file) (locate-dominating-file default-directory ".git"))
              'magit-find-file)
             (t 'counsel-find-file)))))


  ;; refrect .ignore to the root of the project
  (setq counsel-git-cmd "rg --files")

  (defun my-counsel-rg (&optional initial-input)
    "counsel-rg at point in the specified directory"
    (interactive)
    (let ((my-ivy-immediate-flag t))
      (ivy-read "rg dir: " 'read-file-name-internal
              :matcher #'counsel--find-file-matcher
              :initial-input initial-input
              :action #'my-counsel-rg-1
              :preselect (counsel--preselect-file)
              :require-match 'confirm-after-completion
              :history 'file-name-history
              :keymap counsel-find-file-map
              :caller 'my-counsel-rg)))

  (defvar my-counsel-rg-exe "")  ;; overridden by _windows.el or _mac.el

  (defun my-counsel-rg-1 (dir)
    (let  ((counsel-ag-base-command (concat my-counsel-rg-exe
                                            " -i --smart-case --no-heading --line-number --color never %s ."))
           (initial-input (if (symbol-at-point) (symbol-name (symbol-at-point)) ""))
           (initial-directory dir)
           (extra-rg-args nil)
           (rg-prompt dir)
           (my-ivy-immediate-flag nil))
      (counsel-ag initial-input initial-directory extra-rg-args rg-prompt)))

  (cl-pushnew 'my-counsel-rg-1 ivy-highlight-grep-commands)

  (defun my-find (&optional initial-input)
    (interactive)
    (let ((my-ivy-immediate-flag t))
      (ivy-read "find dir: " 'read-file-name-internal
                :matcher #'counsel--find-file-matcher
                :initial-input initial-input
                :action #'my-find-1
                :preselect (counsel--preselect-file)
                :require-match 'confirm-after-completion
                :history 'file-name-history
                :keymap counsel-find-file-map
                :caller 'my-find)))

  (defun my-find-1 (dir)
    (let* ((my-ivy-immediate-flag nil)
           (initial-input (or (thing-at-point 'filename) "")))
      (counsel-file-jump initial-input dir)))

  ;; re-defun rom counsel.el
  ;; Usage: C-x C-f M-x m
  (defun counsel-find-file-move (x)
    "Move or rename file X."
    (let* ((name (if (and ivy--directory (string-match "/$" (ivy-state-current ivy-last)))
                     (substring (ivy-state-current ivy-last) 0 -1)
                   (ivy-state-current ivy-last)))
           (new-name (expand-file-name (read-no-blanks-input (format "mv \"%s\" -> " name) name))))
      (require 'dired-aux)
      (dired-rename-file name new-name 1)))

  ;--------------
  (defun my-counsel-ibuffer-kill-buffer (x)
    "Kill buffer X."
    (let ((buf-name (cdr x)))
      (condition-case err
          (kill-buffer buf-name)
        (error (error "Can not kill buffer: %s" buf-name)))))

  (ivy-set-actions
   'counsel-ibuffer
   '(("k" my-counsel-ibuffer-kill-buffer "kill buffer")))

  ;--------------
  (defun my-ivy-find-file-copy-file-name-to-kill-ring (x)
    "Copy file name to kill ring."
    (let ((name (file-name-nondirectory x)))
      (kill-new name)))

  (ivy-set-actions
   'counsel-find-file
   '(("w" my-ivy-find-file-copy-file-name-to-kill-ring "copy file name")))

  ;--------------
  ;; mod from counsel.el
  (defun counsel-find-file-delete (x)
    "Move file X to backup directory instead of deleting it."
    (if (and (stringp my-backup-directory)
             (file-exists-p my-backup-directory))
        (let ((dest (my-backup-get-suffixed-file-name
                     (path-join my-backup-directory (file-name-nondirectory x)))))
          (rename-file x dest)
          (message "Moved to: %s" dest))
      (error "Invalid my-backup-directory: %s" my-backup-directory)))

  ;--------------
  (defun my-counsel-write-file ()
    "Supported creating unexisted parent-directories and 
using a new file name regardless of the candidates"
    (interactive)
    (let ((ivy-minibuffer-map (make-sparse-keymap)))
      (define-key ivy-minibuffer-map (kbd "RET") #'ivy-immediate-done)
      (ivy-read "Write file to: "
                #'read-file-name-internal
                :preselect (or (buffer-file-name) "")
                :history 'write-file-history
                :action #'my-counsel-write-file-action-function
                :caller 'my-counsel-write-file)))

(defun my-counsel-write-file-action-function (fn)
    (let ((dir (file-name-directory fn)))
      (cond ((file-exists-p fn)
             (if (y-or-n-p "Overwrite? ")
                 (write-file fn)
               (message "Canceled")))
            ((not (file-exists-p dir))
             (create-directory-recursive dir)
             (write-file fn))
            (t (write-file fn)))))

(defun create-directory-recursive (path)
  (let* ((slash "/")
         (full-path (expand-file-name path))
         (dirs (split-string full-path slash t))
         (s ""))
    (when (string= (substring (first dirs) -1 nil) ":")
      (setq dirs (push (concat (first dirs) slash (second dirs)) (cl-subseq dirs 2 ))))
    (dolist (d dirs)
      (if (and (> (length d) 1) (string= (substring d 1 2) ":"))
          (setq s (concat s d))
        (setq s (concat s slash d)))
      (unless (file-exists-p s)
        (make-directory (directory-file-name s))
        (unless (file-directory-p s)
          (error "Can not create directory: %s" s))))))

  ;--------------
  (defun my-font-list ()
    "List font using ivy"
    (interactive)
    (ivy-read "Font: "
              (font-family-list)
              :require-match t
              :action (lambda (x) (insert x))
              :caller 'my-font-list))

  ;--------------
  :bind (("M-z"     . ivy-resume)
         ("M-r"     . counsel-recentf)
         ("M-o"     . my-counsel-rg)
         ("C-x C-b" . counsel-ibuffer)
         ("C-x C-w" . my-counsel-write-file)
         ;; ("C-x C-f" . my-counsel-find-file)
         ;; ("C-s"     . swiper)

         :map ivy-minibuffer-map
         ;; ([remap ivy-done] . ivy-immediate-done)
         ([(return)] . my-ivy-done)
         ("C-j" . ivy-next-line)
         ("C-k" . ivy-previous-line)
         ("M-h" . ivy-backward-kill-word)
         ("C-o" . nil)
         ("M-x" . ivy-dispatching-done)                 ; M-o   --> M-x
         ("C-M-x" . ivy-dispatching-call)               ; C-M-o --> C-M-x
         ("M-j" . ivy-next-history-element)
         ("M-k" . ivy-previous-history-element)
         ("M-<down>" . ivy-next-history-element)
         ("M-<up>"   . ivy-previous-history-element)
         ;; ("C-f" . ivy-avy)

         :map counsel-find-file-map
         ("M-RET" . ivy-immediate-done)

         :map counsel-mode-map
         ("M-RET" . ivy-immediate-done)

         :map evil-motion-state-map
         ("f" . avy-goto-char-timer))
  )

;; ----------------------------------------------------------------------
(use-package all-the-icons-ivy
  :init
  (setq all-the-icons-scale-factor 1.0)
  (defun all-the-icons-ivy-icon-for-file (s)
    "Return icon for filename S.
Return the octicon for directory if S is a directory.
Otherwise fallback to calling `all-the-icons-icon-for-file'."
    (cond
     ((string-match-p "\\/$" s)
      (all-the-icons-octicon "file-directory" :face 'all-the-icons-ivy-dir-face))
     (t (all-the-icons-icon-for-file s :v-adjust 0.02))))

  (all-the-icons-ivy-setup)
  )

;; ----------------------------------------------------------------------
(use-package counsel-etags
  :disabled
  ;; :diminish
  :after counsel

  )

;; ----------------------------------------------------------------------
(use-package counsel-gtags
  :disabled
  :after counsel evil
  :diminish '(counsel-gtags-mode . "Gtags")
  :hook ((c-mode . counsel-gtags-mode))
  :init
  ;; (add-hook 'c-mode-hook 'counsel-gtags-mode)

  :config
  (setq counsel-gtags-auto-update t
        counsel-gtags-path-style 'root)

  ;; (defun gtags-update ()
  ;;   (interactive)
  ;;   (let ((s (shell-command-to-string "global -uv")))
  ;;     (if (string-match "not found" s)
  ;;         (call-interactively 'helm-gtags-create-tags)
  ;;       (message "Updated GTAGS files."))))

  (defalias 'my-gtags-update 'counsel-gtags-update-tags)

  (defun my-gtags-create (rootdir)
    "Create tag database in ROOTDIR. Prompt for ROOTDIRif not given.  This command is asynchronous."
    (interactive (list
                  (let ((my-ivy-immediate-flag t))
                    (read-directory-name "GTAGS Dir: " nil nil t))))
    (let* ((default-directory rootdir)
           (proc-buf (get-buffer-create " *counsel-gtags-tag-create*"))
           (proc (start-file-process
                  "counsel-gtags-tag-create" proc-buf
                  "gtags" "-q" (concat "--gtagslabel=default"))))
      (set-process-sentinel
       proc
       (counsel-gtags--make-gtags-sentinel 'create))))
    (fset 'counsel-gtags-create-tags nil)               ; undefine original command

    (setenv "GTAGSLIBPATH" "/usr/local/Cellar/avr-gcc/7.3.0/avr/include") ; for qmk_firmware on Mac

  :bind (("C-x C-g" . counsel-gtags-find-file)
         :map evil-normal-state-map
         ("g t" . counsel-gtags-dwim)
         ;; ("g t" . counsel-gtags-find-definition)
         ("g r" . gtags-find-reference)
         ("g s" . gtags-find-symbol)
         ("g h" . counsel-gtags-go-backward))

)

;; ----------------------------------------------------------------------
(use-package recentf
  :config
  (setq recentf-max-saved-items 5000) ;; 履歴保存の数
  ;; (setq recentf-auto-cleanup 'never)  ;; 存在しないファイルは消さない network経由のときに有効にする
  (setq recentf-exclude '(
     "/recentf" ".recentf" ".my-save-frame" "batch-script.el" "notes.org"))
  ;; (setq recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))
  )

;; ----------------------------------------------------------------------
(use-package smartparens
  :diminish smartparens-mode
  :config
  (smartparens-global-mode)
  ;; (show-smartparens-global-mode t)
  ;; (setq sp-autoinsert-pair nil)
  (set-face-background 'sp-show-pair-match-face "#4C6DA6")

  (ad-disable-advice 'delete-backward-char 'before 'sp-delete-pair-advice) ; disable C-h
  (ad-activate 'delete-backward-char)

  ;; use show-paren to hilight content in parenthesis
  (setq show-paren-style 'expression)
  (set-face-background 'show-paren-match "#263652")
  (setq show-paren-delay 0.2)
  (show-paren-mode 1)

  ;; depends on modes
  (sp-with-modes '(lisp-mode lisp-interaction-mode emacs-lisp-mode)
   (sp-local-pair "'" nil :actions nil)
   (sp-local-pair "`" nil :actions nil))

  )

;; ----------------------------------------------------------------------
(use-package expand-region
  :after evil symbol-overlay
  :config
  (push 'er/mark-outside-pairs er/try-expand-list)
  (setq expand-region-smart-cursor nil)
  ;; (setq expand-region-autocopy-register "e")
  ;; (setq expand-region-autocopy-kill-ring t)
  (define-key evil-normal-state-map (kbd "=") 'er/expand-region)
  (define-key evil-normal-state-map (kbd "-") 'er/contract-region)
  (define-key evil-visual-state-map (kbd "=") 'er/expand-region)
  (define-key evil-visual-state-map (kbd "-") 'er/contract-region)

  )

;; ----------------------------------------------------------------------
(use-package rainbow-delimiters
  ;; :disabled
  :after cl-lib color
  :hook ((prog-mode . rainbow-delimiters-mode))
  :config
  (set-face-foreground 'rainbow-delimiters-depth-9-face "#9a4040")   ; swap 1 <--> 9
  (set-face-foreground 'rainbow-delimiters-depth-2-face "#ff5e5e")
  (set-face-foreground 'rainbow-delimiters-depth-3-face "#ffaa77")
  (set-face-foreground 'rainbow-delimiters-depth-4-face "#dddd77")
  (set-face-foreground 'rainbow-delimiters-depth-5-face "#80ee80")
  (set-face-foreground 'rainbow-delimiters-depth-6-face "#66bbff")
  (set-face-foreground 'rainbow-delimiters-depth-7-face "#da6bda")
  (set-face-foreground 'rainbow-delimiters-depth-8-face "#afafaf")
  (set-face-foreground 'rainbow-delimiters-depth-1-face "#f0f0f0")   ; swap 1 <--> 9

  (setq rainbow-delimiters-outermost-only-face-count 1)
  ;; (set-face-bold 'rainbow-delimiters-depth-1-face t)
  )

;; ----------------------------------------------------------------------
(use-package rainbow-mode
  :diminish rainbow-mode
  ;; :hook ((prog-mode . rainbow-mode))
  :config
  (setq rainbow-html-colors nil)
  )

;; ----------------------------------------------------------------------
(use-package symbol-overlay
  :ensure t
  :hook ((prog-mode . symbol-overlay-mode))
  :config
  (setq symbol-overlay-idle-time 0.2)
  (set-face-attribute 'highlight nil :background "#555555" :foreground "#eeeeee" :bold nil)

  (let ((color (mycolor 'red)))
    (set-face-attribute 'symbol-overlay-face-1 nil :background color :bold nil)
    (set-face-attribute 'symbol-overlay-face-2 nil :background color :bold nil)
    (set-face-attribute 'symbol-overlay-face-3 nil :background color :bold nil)
    (set-face-attribute 'symbol-overlay-face-4 nil :background color :bold nil)
    (set-face-attribute 'symbol-overlay-face-5 nil :background color :bold nil)
    (set-face-attribute 'symbol-overlay-face-6 nil :background color :bold nil)
    (set-face-attribute 'symbol-overlay-face-7 nil :background color :bold nil)
    (set-face-attribute 'symbol-overlay-face-8 nil :background color :bold nil))

  (defvar my-symbol-overlay-marker (make-marker))

  (defun my-symbol-overlay-enter ()
    (interactive)
    (set-marker my-symbol-overlay-marker (point))
    (symbol-overlay-put))

  (defun my-symbol-overlay-exit ()
    (interactive)
    (symbol-overlay-put)    ;; exit
    (symbol-overlay-remove-all)
    (when my-symbol-overlay-marker
      (goto-char my-symbol-overlay-marker)
      (set-marker my-symbol-overlay-marker nil)))

  :bind (:map evil-normal-state-map
         ("M-s"    . symbol-overlay-mode)
         ("s"      . my-symbol-overlay-enter)
         :map symbol-overlay-map
         ("q"      . nop)
         ("j"      . symbol-overlay-jump-next)
         ("k"      . symbol-overlay-jump-prev)
         ("c"      . symbol-overlay-save-symbol)
         ("C-g"    . my-symbol-overlay-exit)
         ([escape] . my-symbol-overlay-exit)
         ([(return)] . my-symbol-overlay-exit)
         ("s"      . my-symbol-overlay-exit))
  )

;; ----------------------------------------------------------------------
(use-package guide-key-tip
  :disabled
  :after guide-key pos-tip
  :init
  (setq guide-key/guide-key-sequence '("C-x"))
  (guide-key-mode 1)

  :config
  (setq guide-key-tip/enabled t)
  (set-face-attribute 'guide-key-tip/pos-tip-face nil
                      :foreground "#333333" :weight 'light :inherit nil)
)

;; ----------------------------------------------------------------------
(use-package scratch-log
  :ensure t
  :config
  (add-to-list 'recentf-exclude "scratch-log-autoloads.el")
  )

;; ----------------------------------------------------------------------
(use-package quick-back
  :load-path "elisp"
  :bind (:map evil-normal-state-map
              ("q SPC" . quick-back-mark)
              ("q q"   . quick-back-jump))
  )

;; ----------------------------------------------------------------------
(use-package gist
  :after evil
  :config
  (evil-add-hjkl-bindings gist-list-menu-mode-map 'emacs
    (kbd "x")       'gist-kill-current
    (kbd "d")       'gist-kill-current
    (kbd "/")       'evil-search-forward
    (kbd "n")       'evil-search-next
    (kbd "N")       'evil-search-previous)

  :bind (:map gist-mode-map
              ("q"       . my-kill-buffer)
              ("C-x C-s" . gist-mode-save-buffer)
              ("C-c C-c" . gist-mode-save-buffer))
  )

;; ----------------------------------------------------------------------
(use-package git-gutter
  :ensure t
  :hook ((focus-in . git-gutter))
  :init
  (use-package git-gutter-fringe
    :ensure t
    :config
    (fringe-helper-define 'git-gutter-fr:modified nil
      "........"
      ".XXXXXX."
      ".XXXXXX."
      ".XXXXXX."
      ".XXXXXX."
      ".XXXXXX."
      ".XXXXXX."
      "........")

    (set-face-attribute 'git-gutter:separator nil :background (face-attribute 'fringe :background))
    (set-face-attribute 'git-gutter:modified  nil :background (face-attribute 'fringe :background))
    (set-face-attribute 'git-gutter:added     nil :background (face-attribute 'fringe :background))
    (set-face-attribute 'git-gutter:deleted   nil :background (face-attribute 'fringe :background))
    (set-face-attribute 'git-gutter:unchanged nil :background (face-attribute 'fringe :background)))

  :config
  (global-git-gutter-mode)

  :bind (([M-down] . git-gutter:next-hunk)
         ([M-up]   . git-gutter:previous-hunk))
  )

;; ----------------------------------------------------------------------
(use-package anzu
  :config
  (defun my-query-replace (&optional arg)
    (interactive "P")
    (call-interactively (if arg
                            'anzu-query-replace-regexp
                          'anzu-query-replace)))
  :bind (("M-%" . my-query-replace))

  )

;; ----------------------------------------------------------------------
(use-package beacon
  ;; :disabled
  :diminish beacon-mode
  :config
  (setq beacon-blink-when-focused t)
  (setq beacon-color "SteelBlue3")
  (setq beacon-blink-delay 0.2)
  (beacon-mode t)
  )

;; ----------------------------------------------------------------------
(use-package org-bullets
  :after org
  :config
  (setq org-bullets-bullet-list '("❖" "☯" "✪" "✿" "✜" "⬟" "⬢" "⬣"))
  (set-face-attribute 'org-level-1 nil :height 1.2)
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; ----------------------------------------------------------------------
(use-package google-translate
  :config
  (defvar google-translate-english-chars "[:ascii:]`‘’“”–'\"`"
    "これらの文字が含まれているときは英語とみなす")

  (defun google-translate-enja-or-jaen (&optional string)
    "regionか、現在のセンテンスを言語自動判別でGoogle翻訳する。"
    (interactive)
    (setq string
          (cond ((stringp string) string)
                (current-prefix-arg
                 (read-string "Google Translate: "))
                ((use-region-p)
                 (buffer-substring (region-beginning) (region-end)))
                (t
                 (save-excursion
                   (let (s)
                     (forward-char 1)
                     (backward-sentence)
                     (setq s (point))
                     (forward-sentence)
                     (buffer-substring s (point)))))))
    (let* ((asciip (string-match
                    (format "\\`[%s]+\\'" google-translate-english-chars)
                    string)))
      (run-at-time 0.1 nil 'deactivate-mark)
      (google-translate-translate
       (if asciip "en" "ja")
       (if asciip "ja" "en")
       string)))

  :bind (("M-t" . google-translate-enja-or-jaen))

  )

;; ----------------------------------------------------------------------
(use-package dired
  ;; :disabled
  :config
  (setq dired-dwim-target t                   ; diredを2つのウィンドウで開いている時に、デフォルトの移動orコピー先をもう一方のdiredで開いているディレクトリにする
        dired-recursive-copies 'always        ; ディレクトリを再帰的にコピーする
        dired-isearch-filenames t)            ; diredバッファでC-sした時にファイル名だけにマッチするように

  ;; ファイルなら別バッファで、ディレクトリなら同じバッファで開く
  ;; http://nishikawasasaki.hatenablog.com/entry/20120222/1329932699
  (defun dired-open-in-accordance-with-situation ()
    (interactive)
    (let ((file (dired-get-filename)))
      (if (file-directory-p file)
          (dired-find-alternate-file)
        (dired-find-file))))

  (put 'dired-find-alternate-file 'disabled nil) ;; dired-find-alternate-file の有効化

  :bind (:map dired-mode-map
             ("a"     . dired-find-file)
             ("RET"   . dired-open-in-accordance-with-situation)
             ([right] . dired-open-in-accordance-with-situation)
             ([left]  . dired-up-directory)
             ("r"     . revert-buffer))                                    ; reload

  )

;; ----------------------------------------------------------------------
(use-package flycheck
  :disabled
  :hook ((c-mode . flycheck-c-mode-hook-func))
  :init
  (defun flycheck-c-mode-hook-func ()
    ;; (flycheck-select-checker 'my-c)
    (flycheck-mode t)
    (setq flycheck-check-syntax-automatically '(mode-enabled save)) ;; new-line also possible
    )

  ;; :config
  ;; (flycheck-define-checker my-c
  ;;   "My C checker using gcc"
  ;;   :command ("gcc" "-Wall" "-Wextra" source)
  ;;   :standard-input t
  ;;   :error-patterns  ((error line-start
  ;;                            (file-name) ":" line ":" column ":" " error: " (message)
  ;;                            line-end)
  ;;                     (warning line-start
  ;;                              (file-name) ":" line ":" column ":" " warning: " (message)
  ;;                              line-end))
  ;;   :modes (c-mode c++-mode))

  :bind (([S-down] . flycheck-next-error)
         ([S-up]   . flycheck-previous-error))
  )

;; ----------------------------------------------------------------------
(use-package cc-mode
  :after flymake
  :mode (("\\.c$" . c-mode)
         ("\\.h$" . c-mode)
         ("\\.cpp$"     . c++-mode)
         ("\\.c\\+\\+$" . c++-mode)
         ("\\.hpp$"     . c++-mode))
  :bind (([S-down] . flymake-goto-next-error)
         ([S-up]   . flymake-goto-prev-error))
  :config
  (advice-add 'c-update-modeline :around #'ignore)      ;; C++//l => C++

  (add-hook 'c-mode-common-hook
            (lambda ()
              (local-set-key "\C-m" 'reindent-then-newline-and-indent)
              (local-set-key "\C-i" 'indent-or-insert-tab)
              ;; (local-set-key "(" 'my-insert-paren)
              ;; (local-set-key "{" 'my-insert-brace)
              ;; (setq case-fold-search nil)                 ; case sensitive
              (c-set-style "stroustrup")
              (c-set-offset 'case-label '+)
              (c-set-offset 'statement-cont 'c-lineup-math)
              (modify-syntax-entry ?_ "w")                ; アンダーバーをワード区切りとしない
              (setq comment-start "//")                   ; コメントを // にする
              (setq comment-end "")
              ;; (setq compilation-read-command nil)         ; make のオプションの確認は不要
              (setq compilation-ask-about-save nil)       ; make するとき save する
              ;; (setq compile-command "make")               ; make時のデフォルトコマンド
              ;; (c-toggle-hungry-state 1)                   ; backspace時にカーソルの左の空白をすべて削除
              (cwarn-mode)
              (which-function-mode 1)
              (display-line-numbers-mode)
              (setq compilation-scroll-output t)
              ;; (setq compile-command "cd ~/git-clone/qmk_firmware; make dichotemy:default")
              (setq compilation-auto-jump-to-first-error t)
              (setq compilation-window-height 10)

              (setq hide-ifdef-shadow t)
              (hide-ifdef-mode 1)
              ))

  ;; never show *compilation* buffer
  (defadvice compilation-start (around inhidbit-display (command &optional mode name-function highlight-regexp))
    (flet ((display-buffer))
      (fset 'display-buffer 'ignore) ad-do-it))
  (ad-activate 'compilation-start)

  ;; enable ANSI color in *compilation* buffer
  ;; (require 'ansi-color)
  (defun colorize-compilation-mode-hook ()
    "Colorize from `compilation-filter-start' to `point'."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region
       compilation-filter-start (point))))
  (add-hook 'compilation-filter-hook #'colorize-compilation-mode-hook)

  (defun truncate-compilation-mode-hook ()
    (setq truncate-lines t) ;; automatically becomes buffer local
    (set (make-local-variable 'truncate-partial-width-windows) nil))
  (add-hook 'compilation-mode-hook 'truncate-compilation-mode-hook)

  (set-face-attribute 'font-lock-comment-face nil :slant 'normal)
  (set-face-attribute 'font-lock-comment-delimiter-face nil :slant 'normal)

  (defun flymake-cc-init ()
    (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                         'flymake-create-temp-inplace))
           (local-file  (file-relative-name
                         temp-file
                         (file-name-directory buffer-file-name)))
           (ext (file-name-extension local-file))
           (compiler (executable-find (cond ((or (string= ext "c")
                                                 (string= ext "h"))
                                             "gcc")
                                            (t
                                             "g++")))))
      (unless (file-executable-p compiler)
        (error "Not found: %s" compiler))
      (list compiler (list "-Wall" "-Wextra" "-fsyntax-only" local-file))))

  (push '("\\.c$" flymake-cc-init) flymake-allowed-file-name-masks)
  (push '("\\.cpp$" flymake-cc-init) flymake-allowed-file-name-masks)
  (push '("\\.c++$" flymake-cc-init) flymake-allowed-file-name-masks)

  (add-hook 'c++-mode-hook '(lambda () (flymake-mode t)))
  (add-hook 'c-mode-hook   '(lambda () (flymake-mode t)))


  ;; :after telephone-line
  ;; :config
  ;; ;; workaround for *compilation* buffer
  ;; (dolist (f '(compilation-info compilation-warning compilation-error))
  ;;   (set-face-background f (face-attribute 'telephone-line-accent-inactive :background)))

  )

;; ----------------------------------------------------------------------
(use-package arduino-mode
  :hook (arduino-mode . flymake-mode)
  :mode (("\\.pde$" . arduino-mode)
         ("\\.ino$" . arduino-mode))
  :config
  (defun flymake-arduino-init ()
    (unless arduino-exe-path
      (error "Not defined arduino-exe-path"))
    (unless (file-exists-p arduino-exe-path)
      (error "Not found %s" arduino-exe-path))
    (unless (file-executable-p arduino-exe-path)
      (error "Not found %s" arduino-exe-path))
    (let* ((temp-file   (flymake-proc-init-create-temp-buffer-copy
                         ;; 'flymake-create-temp-inplace))
                         'flymake-proc-create-temp-with-folder-structure))
           (local-dir   (file-name-directory buffer-file-name)))
      (list arduino-exe-path (list "compile"
                                   (concat "--fqbn=" arduino-fqbn)
                                   (substring local-dir 0 -1)))))

  (push '("\\.ino$" flymake-arduino-init) flymake-proc-allowed-file-name-masks)
  (push '("^\\(.+\.ino\\):\\([0-9]+\\):\\([0-9]+\\): \\(.+\\)$" 1 2 3 4) flymake-err-line-patterns)
  )

;; ----------------------------------------------------------------------
(use-package mql-mode
  :mode (("\\.mq4$" . mql-mode)
         ("\\.mqh$" . mql-mode))
  :bind (([S-down] . flymake-goto-next-error)
         ([S-up]   . flymake-goto-prev-error))
  :config
  (setq mq4-compiler "C:/Users/g/AppData/Roaming/MetaQuotes/WebInstall/mt4clw/metaeditor.exe")
  (add-hook 'mql-mode-hook (lambda ()
                             (flycheck-mode -1)
                             (flymake-mode t)
                             (counsel-gtags-mode -1)
                             (symbol-overlay-mode t)
                             (which-function-mode 1)
                             (setq-local show-trailing-whitespace nil)
                             ))
  )

;; ----------------------------------------------------------------------
(use-package slime
  :disabled
  :init
  (load (expand-file-name "~/.roswell/helper.el"))

  :config
  (setq slime-startup-animation nil)
  (defalias 'slime-reset 'slime-restart-inferior-lisp)
  (setq inferior-lisp-program "ros -Q run")
  (setq slime-net-coding-system 'utf-8-unix)
  (add-hook 'slime-load-hook (lambda () (require 'slime-fancy)))
  (slime-setup '(slime-fancy slime-banner))

  ;; 分割したウィンドウでslime起動
  (defun my-slime (&optional command coding-system)
    "Run slime and split window."
    (interactive)
    (if (< (count-windows) 2)
        (split-window-vertically))
    (slime command coding-system))

  ;; 選択範囲をslime-replへ送って評価
  (defun slime-repl-send-region (start end)
    "Send region to slime-repl."
    (interactive "r")
    (let ((buf-name (buffer-name (current-buffer)))
          (sbcl-buf (get-buffer "*slime-repl sbcl*")))
      (cond (sbcl-buf
             (copy-region-as-kill start end)
             (switch-to-buffer-other-window sbcl-buf)
             (yank)
             (slime-repl-send-input "\n")
             (switch-to-buffer-other-window buf-name))
            (t (message "Not exist *slime-repl sbcl* buffer!")))))

  ;; LISPモードで新しくファイルを開いたらウィンドウが上下に分割して下にREPL
  (add-hook 'lisp-mode-hook
            (lambda ()
              (global-set-key "\C-cC-h" 'hyperspec-lookup)
              (cond ((not (featurep 'slime))
                     (require 'slime)
                     (normal-mode)))
              (my-slime)
              (other-window)))

  :bind (:map lisp-mode-map
             ("M-r" . nil)
             ("C-x C-e" . slime-eval-last-expression-in-repl)
             ("C-c C-c" . slime-compile-and-load-file)
             ("C-c C-r" . slime-repl-send-region)
             ("C-c C-f" . slime-compile-defun))
  )

;; ----------------------------------------------------------------------
(use-package org
  :config
  ;; (setq org-directory "~/Dropbox/org")       ;; defined in _windows.el or _mac.el
  (setq org-default-notes-file (expand-file-name (path-join org-directory "notes.org")))

  (setq org-hide-emphasis-markers t)
  (setq org-todo-keywords '((sequence "[ ]" "[!]" "|" "[X]" )))
  (setq org-capture-templates
        '(("t" "Todo" checkitem (file org-default-notes-file) "" :unnarrowed t)
          ("m" "Memo" entry     (file org-default-notes-file) "* %?" :unnarrowed t)))


  (set-face-attribute 'org-level-2 nil :foreground (face-foreground 'default))

  (set-face-attribute 'org-todo nil :foreground (mycolor 'pink) :background (face-background 'default) :weight 'bold)
  (set-face-attribute 'org-checkbox-statistics-todo nil :foreground (face-foreground 'default) :background (face-background 'default) :weight 'normal)

  (set-face-attribute 'org-done nil :foreground (mycolor 'green) :background (face-background 'default) :weight 'bold)
  (copy-face 'org-done 'org-checkbox-statistics-done)

  (defface my-org-done-date-face
    `((t (:inhelit org-todo :foreground ,(face-background 'org-done) :background ,(face-foreground 'org-done) :weight bold))) "")

  (defun font-lock-user-keywords (mode &optional keywords)
    "Add user highlighting to KEYWORDS to MODE.
See `font-lock-add-keywords' and `font-lock-defaults'."
    (unless mode
      (error "Mode should be non-nil"))
    (font-lock-remove-keywords mode (get mode 'font-lock-user-keywords))
    (font-lock-add-keywords mode keywords)
    (put mode 'font-lock-user-keywords keywords))

  (font-lock-user-keywords 'org-mode '(
    ;; todo
    ("^*+ \\[X\\] \\( [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} \\)\\(.+\\)$" . '(1 'my-org-done-date-face))
    ("^*+ \\[X\\] \\( [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} \\)\\(.+\\)$" . '(2 'org-done))
    ("^*+ \\[!\\] \\(.+\\)$" . '(1 'org-todo))
    ;; "-" --> "•"
    ("^ *\\([-]\\) " (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))
    ;; "* [ ]" --> "[ ]"
    ("^\\(*+ \\)\\[.\\] " (0 (progn () (add-text-properties (match-beginning 1) (match-end 1) '(invisible t)))))
    ;; "https://..." --> ""
    ;; ("\\(http[s]*://.+\\)[ \n]" (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) ""))))
    ))
  ;; (org-set-font-lock-defaults)
  ;; (font-lock-fontify-buffer)

  ;; ----------
  (defun my-org-get-todo-content ()
    "Return string as todo content if current line has todo content. Otherwise return nil"
    (save-excursion
      (beginning-of-line)
      (if (re-search-forward "^*+ \\[ \\] +\\(.+\\)" (line-end-position) t)
          (match-string 1)
        nil)))

  (defun my-org-todo-get-title ()
    "Return string as todo title if it found. Otherwise return nil"
    (save-excursion
      (beginning-of-line)
      (if (re-search-forward "^*+ \\[ \\] \\(.+\\)" (line-end-position) t)
          (if (re-search-backward "^*+ \\([^[]+\\)\\( \\[.+\\]\\)?$" nil t)
              (string-trim (match-string 1))
            nil)
        nil)))

  (defun my-org-kill-whole-line (&optional point)
    (when point
      (goto-char point)
      (beginning-of-line))
    (org-kill-line)
    (org-kill-line))

  (defvar my-org-move-to-never-do-dest-title "やらないことリスト")
  (defun my-org-move-to-never-do (reason title-orig)
    (let ((title my-org-move-to-never-do-dest-title)
          (pt (save-excursion (my-org-beginning-of-content) (point)))
          (content (my-org-get-todo-content)))
      (when content
        (condition-case err
            (save-excursion
              (goto-char (point-min))
              (re-search-forward (concat "^* " title))
              (forward-line 1)
              (insert (format "** [ ] :%s: %s ← %s\n" title-orig content reason)))
          (error (progn (goto-char pt)
                        (format "Not found: %s" title)))))))

  ;; ----------
  (defun my-org-capture-add-1 (type text)
    (interactive)
    (let ((buf (current-buffer))
          (pt (point))
          (tr-re "[ \t\n\r　]+")
          (title (cond ((eq type 'todo) "目安箱")
                       ((eq type 'memo) "memo")
                       (t nil)))
          (fmt (cond ((eq type 'todo) "** [ ] %s\n")
                     ((eq type 'memo) "** %s\n"))))
      (setq text (string-trim text tr-re tr-re))
      (when (and (not (string= text "")) title)
        (find-file org-default-notes-file)
        (goto-char (point-min))
        (if (re-search-forward (concat "^* " title))
            (progn
              (forward-line 1)
              (insert (format fmt text)))
          (message (format "Not found: %s" title)))
        (if (eq (current-buffer) buf)
            (goto-char pt)
          (let ((inhibit-message t))
            (save-buffer))
          (bury-buffer)))))

  (defun my-org-capture-add-todo (text)
    (interactive "sTODO: ")
    (my-org-capture-add-1 'todo text))

  (defun my-org-capture-add-memo (text)
    (interactive "sMEMO: ")
    (my-org-capture-add-1 'memo text))

  (defun my-org-notes-open ()
    (interactive)
    (if (member org-default-notes-file (org-files-list))
        (let* ((buf-org (get-file-buffer org-default-notes-file))
               (win-org (get-buffer-window buf-org)))
          (if (and buf-org win-org)          ;; org-file is already shown in any windows
              (delete-windows-on buf-org)
            (find-file org-default-notes-file)))
      (find-file org-default-notes-file)))

  (defun my-org-notes-close ()
    (interactive)
    (if (string= (buffer-file-name) org-default-notes-file)
        (progn
          (my-org-global-fold-cycle-folding-store)
          (save-buffer)
          (bury-buffer))
      (my-org-notes-open)))

  ;; ----------
  (defun my-org-goto-title-next ()
    (interactive)
    (my-org-goto-title-next-1 nil))

  (defun my-org-goto-title-prev ()
    (interactive)
    (my-org-goto-title-next-1 t))

  (defun my-org-goto-title-next-1 (backward-p)
    (let ((pt (point))
          (re "^* [^[].+$"))
      (if backward-p
          (if (progn (beginning-of-line) (re-search-backward re nil t))
              (my-org-beginning-of-content)
            (goto-char pt))
        (if (progn (end-of-line) (re-search-forward re nil t))
            (my-org-beginning-of-content)
          (goto-char pt)))))

  ;; ----------
  (defun my-org-todo-goto-working-forward ()
    (interactive)
    (let ((pt (point)))
      (unless (progn (end-of-line) (re-search-forward  "^*+ \\[!\\] " nil t))
        (goto-char pt))))

  (defun my-org-todo-goto-working-backward ()
    (interactive)
    (let ((pt (point)))
    (unless (prog2 (goto-char (line-beginning-position))
                (re-search-backward "^*+ \\[!\\] " nil 1)
              (goto-char (+ (point) (- (match-end 0) (match-beginning 0)))))
      (goto-char pt))))

  ;; ----------
  (defun my-org-dup-heading-up ()
    (interactive)
    (unless (my-org-dup-heading-1 t)
      (evil-open-above 1)))

  (defun my-org-dup-heading-down ()
    (interactive)
    (unless (my-org-dup-heading-1 nil)
      (evil-open-below 1)))

  (defun my-org-dup-heading-1 (up)
    (let ((beg (line-beginning-position))
          (end (line-end-position))
          (pt (point)))
      (goto-char beg)
      (if (re-search-forward "^*+ \\[.\\] \\|^*+ " end t)
          (let ((s (buffer-substring beg (point))))
            (if up
                (evil-open-above 1)
              (evil-open-below 1))
            (insert (replace-regexp-in-string "\\[.\\]" "[ ]" s))
            (unless (eq evil-state 'insert)
              (evil-insert-state 1))
            (org-update-parent-todo-statistics)
            t)
        (goto-char pt)
        nil)))

  ;; ----------
  (defun my-org-beginning-of-content ()
    (interactive)
    (let ((beg (line-beginning-position))
          (end (line-end-position))
          (pt (point)))
      (goto-char beg)
      (if (re-search-forward "^*+ \\[.\\] \\|^* " end t)
          (when (= (point) pt)
            (org-beginning-of-line))
        (org-beginning-of-line))))

  ;; ----------
(defun my-org-global-fold-cycle ()
    (interactive)
    (cl-labels ((= (state) (eq my-org-global-fold-cycle-state state))
                (-> (state) (setq my-org-global-fold-cycle-state state))
                (fold-restore () (my-org-global-fold-cycle-folding-restore))
                (fold-backup  () (my-org-global-fold-cycle-folding-store))
                ;; (message-state () (message "Folding: %s" my-org-global-fold-cycle-state)))
                (message-state () nil))
      (cond ((= 'user)     (-> 'hide-all)                           ;; user -> hide-all
             (fold-backup) (outline-hide-sublevels 1) (message-state))
            ((= 'hide-all) (-> 'show-all)                           ;; hide-all -> show-all
             (outline-show-all) (message-state))
            ((= 'show-all) (-> 'user)                               ;; show-all -> user
             (fold-restore) (message-state))
            (t (error (format "Invalid current folding state: %S" my-org-global-fold-cycle-state))))))


  (defun my-org-global-fold-set (target-state)
    (if (memq target-state '(user hide-all show-all))
        (while (not (eq my-org-global-fold-cycle-state target-state))
          (my-org-global-fold-cycle))
      (error "Invalid target-state: %S" target-state)))


  ;; from org-fold.el
  (defun my-org-global-fold-cycle-folding-store ()
    "Store folding states of org-mode to file for current buffer to `my-org-global-fold-cycle-folding-states'"
    (save-excursion
      (goto-char (point-min))
      (let (foldstates)
        (unless (looking-at outline-regexp)
          (outline-next-visible-heading 1))
        (while (not (eobp))
          (push (if (some (lambda (o) (overlay-get o 'invisible))
                          (overlays-at (line-end-position)))
                    t)
                foldstates)
          (outline-next-visible-heading 1))
        (setq my-org-global-fold-cycle-folding-states (nreverse foldstates)))))

  (defun my-org-global-fold-cycle-folding-restore ()
    "Restore folding states of org-mode from file for current buffer"
    (save-excursion
      (goto-char (point-min))
      (let ((foldstates my-org-global-fold-cycle-folding-states))
        (when foldstates
          (show-all)
          (goto-char (point-min))
          (unless (looking-at outline-regexp)
            (outline-next-visible-heading 1))
          (while (and foldstates (not (eobp)))
            (if (pop foldstates)
                (hide-subtree))
            (outline-next-visible-heading 1))))))

  (defun my-org-fold-get-fold-info-file-name ()
    (concat (buffer-file-name) ".fold"))

  (defun my-org-fold-save-to-file ()
    "Save list of folding states about current buffer to fold file."
    (let ((foldstates my-org-global-fold-cycle-folding-states))
      (with-temp-file (my-org-fold-get-fold-info-file-name)
        (prin1 foldstates (current-buffer)))))

  (defun my-org-fold-load-from-file ()
    "Return list of folding states about current buffer from fold file."
    (let ((foldfile (my-org-fold-get-fold-info-file-name)))
      (if (file-readable-p foldfile)
          (with-temp-buffer
            (insert-file-contents foldfile)
            (read (current-buffer)))
        (error (format "Can not load fold file: %s" foldfile)))))

  (add-hook 'org-mode-hook 'org-fold-activate)

  (defun org-fold-activate ()
    (defvar-local my-org-global-fold-cycle-state 'user "cycle state of current org buffer")
    (defvar-local my-org-global-fold-cycle-folding-states nil "A list of user folding states of current org buffer")
    (setq-local my-org-global-fold-cycle-folding-states (my-org-fold-load-from-file))

    (my-org-global-fold-cycle-folding-restore)
    (add-hook 'kill-buffer-hook 'org-fold-kill-buffer nil t)
    (add-hook 'kill-emacs-hook  'org-fold-kill-emacs))

  (defun org-fold-kill-buffer ()
    (my-org-fold-save-to-file))

  (defun org-fold-kill-emacs ()
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (eq major-mode 'org-mode)
          (my-org-fold-save-to-file)))))

  ;; ----------
  (defun my-org-title-line-p (re)
    (save-excursion (goto-char (line-beginning-position))
                    (re-search-forward re (line-end-position) t)))

  (defun my-org-cycle ()
    (interactive)
    (cond ((my-org-title-line-p "^*+ \\[.\\] ")     ;; todo line?
           (my-org-cycle-todo-forward))
          ((my-org-title-line-p "^*+ ")             ;; title line?
           (my-org-cycle-fold-title)
           (when (eq my-org-global-fold-cycle-state 'user)
             (my-org-global-fold-cycle-folding-store)))
          (t nil)))

  (defun my-org-cycle-fold-title ()
    (if (outline-invisible-p (line-end-position))
        (outline-show-subtree)
      (outline-hide-subtree)))

  (defun my-org-cycle-todo-forward ()
    (interactive)
    (my-org-cycle-todo-1 nil))
  (defun my-org-cycle-todo-backward ()
    (interactive)
    (my-org-cycle-todo-1 t))

  (defun my-org-cycle-todo-1 (reverse)
    (save-excursion
      (goto-char (line-beginning-position))
      (when (re-search-forward "\\(^*+ \\[\\)\\(.\\)\\(\\] \\)" (line-end-position) t)
        (let ((kw (match-string 2)))
          (let ((rpl (if reverse
                         (cond ((string= kw "X") "!")
                               ((string= kw "!") " ")
                               (t nil))
                       (cond ((string= kw " ") "!")
                             ((string= kw "!") "X")
                             (t nil)))))
            (when rpl
              (replace-match (concat (match-string 1) rpl (match-string 3)))
              (cond ((string= rpl "X") (my-org-todo-date-insert))
                    (t                 (my-org-todo-date-remove)))
              (org-update-parent-todo-statistics)))))))

  ;; ----------
  (defun my-org-todo-date-insert ()
    (let ((pt (point)))
      (save-excursion
        (goto-char (line-beginning-position))
        (when (re-search-forward "\\(^*+ \\[X\\]\\) " (line-end-position) t)
          (replace-match (concat "\\1" (format-time-string "  %Y-%m-%d  ")))))
      (goto-char pt)))

  (defun my-org-todo-date-remove ()
    (let ((pt (point)))
      (save-excursion
        (goto-char (line-beginning-position))
        (when (re-search-forward "\\(^*+ \\[.\\]\\)  [0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\} "
                                 (line-end-position) t)
          (replace-match "\\1")))
      (goto-char pt)))

  ;; ----------
  (evil-define-command my-org-evil-normal-do-demote () "" (org-demote))
  (evil-define-command my-org-evil-normal-do-promote () "" (org-promote))

  (evil-define-operator my-org-evil-visual-do-demote (beg end) "" :type 'line
    (interactive "<r>")
    (org-map-region #'org-demote beg end)
    (org-fix-position-after-promote))
  (evil-define-operator my-org-evil-visual-do-promote (beg end) "" :type 'line
    (interactive "<r>")
    (org-map-region #'org-promote beg end)
    (org-fix-position-after-promote))

  ;; ----------
  (defun my-org-get-links-in-line (&optional beg)
    (interactive)
    (let ((links '())
          (eol (line-end-position)))
      (save-excursion
        (when beg (goto-char beg))
        (while (< (goto-char (next-single-property-change (point) 'htmlize-link nil eol)) eol)
          (let ((lk (get-text-property (point) 'htmlize-link)))
            (when lk
              (setq links (cons (second lk) links))))))
      links))

  (defvar my-org-todo-publish-cemetery-accept-titles '("目安箱" "Emacs" "次のKeyboard" "ウェブ投票システムをつくる"))
  (defvar my-org-todo-publish-cemetery-reason-default-list '(
    "やる気ないので"　"やる気ねえけん" "やる気なかけん" "やる気ねえかぃ" "やっ気なかで" "やる気ないけん" "やる気ないき"
     "やる気あらへんさかいに" "やる気にゃーで" "やる気ねえすけ" "やる気ねぁがら" "やる気ねはんで" "やる気ねーんくとぅ"
     "ダルいので" "ダルさんくとぅ" "ダルぇはんで" "ダルぇがら" "ダリぃけん" "ダリで" "ダルいけぇ" "ダルいき"
     "しんどいさかいに" "ダやいがで" "ダルいで" "ダリぃすけ" "ダルぇがら" "ダルぇはんで" "ダルさんくとぅ"
     "すみません。急いでおりますので。")
    "thx to https://www.8toch.net/translate/")
  (defvar my-org-todo-publish-cemetery-hugo-dir "~/git-clone/cemetery")
  (defvar my-org-todo-publish-cemetery-front-matter-fmt
"#+TITLE: %s
#+DATE: %s
#+DRAFT: false
#+TAGS[]: %s
")

  (defun my-org-todo-publish-cemetery-or-move-to-never-do-get-reason (prompt)
    "Return string as reason from user input.
If the input is empty, the return value is randomly determined."
    (let ((s (read-string prompt))
          (tr-re "[ \t\n\r　]+"))
      (cond ((string-empty-p s)
             (let ((n (length my-org-todo-publish-cemetery-reason-default-list)))
               (nth (random n) my-org-todo-publish-cemetery-reason-default-list)))
            (t (string-trim s tr-re tr-re)))))

  (defun my-org-todo-publish-cemetery-git-push (path)
    "Execute git commands add, commit then push in order to deploy
new post to netlify/hugo. Commands add and push run synchronously,
but command push takes more time so that runs asynchronously."
    (let* ((process-connection-type nil)
           (default-directory (path-join my-org-todo-publish-cemetery-hugo-dir))
           (path (file-relative-name path default-directory)))
      (condition-case err
          (progn
            (unless (= (call-process "git" nil nil nil "add" path) 0)
              (error "Error at 'git add'"))
            (unless (= (call-process "git" nil nil nil "commit" "-m" "add post") 0)
              (error "Error at 'git commit'"))
            (start-process "" nil "git" "push"))
        (error (error-message-string err)))))

  (defun my-org-todo-publish-cemetery-or-move-to-never-do ()
    "Publish the current todo line to cemetery or move to 'never-do' list,
according to `my-org-todo-publish-cemetery-accept-titles'."
    (interactive)
    (cl-flet ((ask-reason 'my-org-todo-publish-cemetery-or-move-to-never-do-get-reason)
              (kill-current-line () (let ((pt (point))) (my-org-kill-whole-line pt) (goto-char pt))))
      (let ((title (my-org-todo-get-title)))
        (cond ((and (my-org-title-line-p "^*+ \\[ \\] ")
                    (member title my-org-todo-publish-cemetery-accept-titles))
               (my-org-todo-publish-cemetery (ask-reason "墓場 <- ") title)
               (kill-current-line)
               (message "Published to TODO墓場"))
              ((my-org-title-line-p "^*+ \\[ \\] ")
               (my-org-move-to-never-do (ask-reason "やらないことリスト <- ") title)
               (kill-current-line)
               (message "Moved to %s" my-org-move-to-never-do-dest-title))
              ((my-org-title-line-p "^*+ \\[.\\] ") (message "This todo item has any status."))
              (t nil)))))

  (defun my-org-todo-publish-cemetery (reason tag)
    (let ((content (string-trim (save-excursion
                                  (goto-char (line-beginning-position))
                                  (buffer-substring-no-properties
                                   (re-search-forward "^*+ \\[ \\] +" (line-end-position) t)
                                   (next-single-property-change (point) 'htmlize-link nil (line-end-position))))))
          (links (my-org-get-links-in-line (line-beginning-position)))
          (path (path-join my-org-todo-publish-cemetery-hugo-dir "content/post"
                           (format-time-string "%Y%m%d-%H%m%S.org"))))
      (with-temp-buffer
        (insert (format my-org-todo-publish-cemetery-front-matter-fmt
                        content
                        (format-time-string "%Y-%m-%dT%H:%m:%S+09:00")
                        tag)
                "* やらなかった理由\n"
                reason "\n")
        (when links
          (insert "* Link\n")
          (mapc #'(lambda (x) (insert (format "- %s\n" x))) links))
        (write-file path))
      (my-org-todo-publish-cemetery-git-push path)))

  ;; ----------

  ;; ----------
  (set-face-attribute 'org-link nil :foreground (face-foreground 'default) :underline t)

  ;; ----------
  (define-key evil-normal-state-map (kbd "M-c") #'ffap)                       ; M-RET
  (define-key evil-normal-state-map (kbd "t d") #'my-org-capture-add-todo)
  (define-key evil-normal-state-map (kbd "t m") #'my-org-capture-add-memo)
  (define-key evil-normal-state-map (kbd "t t") #'my-org-notes-open)          ; toggle org buffer
  (evil-define-key 'normal org-mode-map (kbd "t t") #'my-org-notes-close)     ; toggle org buffer
  (evil-define-key 'normal org-mode-map (kbd "t d") #'my-org-capture-add-todo)
  (evil-define-key 'normal org-mode-map (kbd "t m") #'my-org-capture-add-memo)
  (evil-define-key 'normal org-mode-map (kbd "<tab>")   #'my-org-evil-normal-do-demote)
  (evil-define-key 'normal org-mode-map (kbd "S-<tab>") #'my-org-evil-normal-do-promote)
  (evil-define-key 'normal org-mode-map (kbd "SPC")   #'my-org-cycle)
  (evil-define-key 'normal org-mode-map (kbd "S-SPC") #'my-org-cycle-todo-backward)
  (evil-define-key 'normal org-mode-map (kbd "M-SPC") #'my-org-global-fold-cycle)
  (evil-define-key 'normal org-mode-map (kbd "C-j") #'org-metadown)
  (evil-define-key 'normal org-mode-map (kbd "C-k") #'org-metaup)
  (evil-define-key 'normal org-mode-map (kbd "O") #'my-org-dup-heading-up)
  (evil-define-key 'normal org-mode-map (kbd "o") #'my-org-dup-heading-down)
  (evil-define-key 'normal org-mode-map (kbd "RET") #'my-org-dup-heading-down)
  (evil-define-key 'normal org-mode-map (kbd "<M-down>") #'my-org-todo-goto-working-forward)
  (evil-define-key 'normal org-mode-map (kbd "<M-up>")   #'my-org-todo-goto-working-backward)
  (evil-define-key 'normal org-mode-map (kbd "<S-left>")  #'nop)
  (evil-define-key 'normal org-mode-map (kbd "<S-right>") #'nop)
  (evil-define-key 'normal org-mode-map (kbd "<S-down>")  #'nop)
  (evil-define-key 'normal org-mode-map (kbd "<S-up>")    #'nop)
  (evil-define-key 'normal org-mode-map (kbd "<C-up>")    #'nop)
  (evil-define-key 'normal org-mode-map (kbd "<C-down>")  #'nop)
  (evil-define-key 'normal org-mode-map (kbd "<C-right>") #'nop)
  (evil-define-key 'normal org-mode-map (kbd "<C-left>")  #'nop)
  (evil-define-key 'normal org-mode-map (kbd "<down>") #'my-org-goto-title-next)
  (evil-define-key 'normal org-mode-map (kbd "<up>")   #'my-org-goto-title-prev)
  (evil-define-key 'normal org-mode-map (kbd "t 0") #'my-org-todo-publish-cemetery-or-move-to-never-do)
  (evil-define-key 'normal org-mode-map (kbd "0")   #'my-org-beginning-of-content)

  (evil-define-key 'insert org-mode-map (kbd "C-a") #'my-org-beginning-of-content)

  (evil-define-key 'visual org-mode-map (kbd "<tab>")   #'my-org-evil-visual-do-demote)
  (evil-define-key 'visual org-mode-map (kbd "S-<tab>") #'my-org-evil-visual-do-promote)
  ;; (evil-define-key 'normal org-mode-map (kbd "M-c") #'my-org-meta-ret)          ; M-RET

  (add-hook 'org-mode-hook #'(lambda ()
          (org-defkey org-mode-map [(meta up)] nil)        ; unmap for tabbar
          (org-defkey org-mode-map [(meta down)] nil)))    ; unmap for tabbar
  )

;; ----------------------------------------------------------------------
(use-package org-fold
  :disabled
  :defer t
  :load-path "~/.emacs.d/elisp"
  :config
  ;; re-defined
  (defun org-fold-activate ()
    ;; (org-fold-restore)
    (unless (file-exists-p (buffer-file-name))
      (org-fold-restore))
    (add-hook 'kill-buffer-hook 'org-fold-kill-buffer nil t)
    (add-hook 'kill-emacs-hook  'org-fold-kill-emacs))
  )

;; ----------------------------------------------------------------------
(use-package shell-script-mode
  :mode (("zshrc" . shell-script-mode))
  )

;; ----------------------------------------------------------------------
(use-package posframe
  :config
  (setq posframe-mouse-banish nil)
  )

;; ----------------------------------------------------------------------
(use-package flymake-posframe
  :load-path "elisp/flymake-posframe"
  :hook (flymake-mode . flymake-posframe-mode)
  :config
  (set-face-attribute 'flymake-error nil :underline `(:color ,(mycolor 'red) :style wave))
  (set-face-attribute 'flymake-warning nil :underline '(:color "orange" :style wave))

  (setq flymake-posframe-error-prefix (propertize "󿞟" 'face `(foreground-color . ,(mycolor 'red))))
  (setq flymake-posframe-warning-prefix (propertize "" 'face '(foreground-color . "orange")))
  (setq flymake-posframe-note-prefix (propertize "" 'face `(foreground-color . "yellow green")))

  (defun my-flymake-posframe-display-adv-delay (orig-fun)
    (when (sit-for 0)
      (funcall orig-fun)))

  (advice-add 'flymake-posframe-display :around #'my-flymake-posframe-display-adv-delay)

  (push 'window-setup-hook flymake-posframe-hide-posframe-hooks)
  (push 'find-file-hook flymake-posframe-hide-posframe-hooks)
  (push 'buffer-list-update-hook flymake-posframe-hide-posframe-hooks)
  (push 'window-configuration-change-hook flymake-posframe-hide-posframe-hooks)
  )

;; ----------------------------------------------------------------------
(use-package super-save
  :ensure t
  :config
  (add-to-list 'super-save-triggers 'tabbar-forward-tab)
  (add-to-list 'super-save-triggers 'tabbar-backward-tab)

  (defun my-adv-super-save-command--disable-message (orig-fun)
    (let ((inhibit-message t))
      (funcall orig-fun)))
  (advice-add 'super-save-command :around #'my-adv-super-save-command--disable-message)

  ;; (setq super-save-auto-save-when-idle t
  ;;       super-save-idle-duration 10)
  (super-save-mode +1)
  )

;; ----------------------------------------------------------------------
(use-package dot-editor
  :disabled
  :after evil
  :config
  (add-hook 'dot-editor-mode-hook #'(lambda ()
    (evil-define-key 'motion dot-editor-mode-map (kbd "C-c C-e") 'dot-editor-encode-region)
    (evil-define-key 'motion dot-editor-mode-map (kbd "C-c C-d") 'dot-editor-decode-region)
    (evil-define-key 'motion dot-editor-mode-map (kbd "C-c C-c") 'dot-editor-insert-canvas)
    (evil-define-key 'motion dot-editor-mode-map (kbd "C-c C-p") 'create-pbm-from-hex)
    (evil-define-key 'motion dot-editor-mode-map (kbd "C-c C-r") 'dot-editor-reverse-region)
    (define-key evil-normal-state-map (kbd "SPC") #'evil-force-normal-state)
    (define-key evil-motion-state-map (kbd "SPC")    'dot-editor-reverse-square)))
    ;; (evil-define-key 'normal dot-editor-mode-map (kbd "SPC")    'dot-editor-reverse-square)))
  )

;; ----------------------------------------------------------------------
(use-package dimmer
  :disabled
  :defer 1
  :config
  (setq dimmer-exclusion-predicates '(window-minibuffer-p)
        dimmer-exclusion-regexp-list '("^\\*Minibuf-[0-9]+\\*" "^*Messages*")
        dimmer-fraction 0.35)

  (dimmer-configure-which-key)
  (dimmer-configure-org)
  (dimmer-configure-posframe)
  ;; (dimmer-configure-hydra)

  (defun dimmer-off ()
    (dimmer-process-all)
    (dimmer-mode -1))

  (defun dimmer-on ()
    (dimmer-mode 1)
    (dimmer-process-all))

  (add-hook 'focus-out-hook #'dimmer-off)
  (add-hook 'focus-in-hook  #'dimmer-on)
  (add-hook 'minibuffer-setup-hook #'dimmer-off)
  (add-hook 'minibuffer-exit-hook  #'dimmer-on)

  (dimmer-mode t)
  )

;; ----------------------------------------------------------------------
(use-package migemo
  :disabled
  :config
  ;; fixme not work in _mac.el
  (setq migemo-command "/usr/local/bin/cmigemo")
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")

  (setq migemo-options '("-q" "--emacs"))
  (setq migemo-user-dictionary nil)
  (setq migemo-regex-dictionary nil)
  (setq migemo-coding-system 'utf-8-unix)
  (migemo-init)

  (setq evil-regexp-search nil)
  (defun my-adv-evil-search-function--migemo (&rest _)
    "Enable migemo when evil-search by / or ?.
Thx to https://qiita.com/duloxetine/items/0adf103804b29090738a"
    (cl-flet ((search-forward 'migemo-forward)
              (search-backward 'migemo-backward))))

  (advice-add 'evil-search-function :before #'my-adv-evil-search-function--migemo)
  )

;; ----------------------------------------------------------------------
(use-package default-text-scale
  :config
  (setq default-text-scale-amount 30)
  (default-text-scale-mode 1)

  (defun my-adv-default-text-scale--reset-frame (&rest _)
    (modify-frame-parameters nil initial-frame-alist))

  (advice-add 'default-text-scale-reset :after #'my-adv-default-text-scale--reset-frame)
  )

;; ----------------------------------------------------------------------
(use-package org-tree-slide
  ;; :defer t
  :bind (:map org-mode-map
         ([f5] . org-tree-slide-on)
         :map org-tree-slide-mode-map
         ([next]  . org-tree-slide-move-next-tree)        ;; page down
         ([prior] . org-tree-slide-move-previous-tree)    ;; page up
         ([f5] . org-tree-slide-off))

  :config
  (defun presen-override-key-bindings ()
    (evil-make-overriding-map org-tree-slide-mode-map 'normal)
    (evil-add-hjkl-bindings org-tree-slide-mode-map 'normal
      [right] 'org-tree-slide-move-next-tree
      [down]  'org-tree-slide-move-next-tree
      [left]  'org-tree-slide-move-previous-tree
      [up]    'org-tree-slide-move-previous-tree)
    (evil-normal-state))    ;; dummy

  (add-hook 'org-tree-slide-play-hook #'presen-override-key-bindings)

  (setq org-tree-slide-indicator '(:next "" :previous "" :content ""))
  (defun org-tree-slide-on  () (interactive) (org-tree-slide-mode 1))
  (defun org-tree-slide-off () (interactive) (org-tree-slide-mode 0))

  (lexical-let ((face-default nil)
                (face-fringe nil)
                (face-minibuf nil)
                (face-link nil)
                (face-level-1 nil)
                (frame-height 36)
                (bg-color "#fefae0")
                (margin (window-margins)))
    (defun presen-enter ()
      (set-frame-height nil frame-height)
      (beacon-mode 0)
      (centaur-tabs-mode -1)
      (scroll-bar-mode 0)
      (set-fringe-mode 0)
      (set-window-margins (selected-window) 4)
      (setq-local evil-normal-state-cursor '(bar . 1))
      (hide-mode-line-mode 1)
      (face-remap-add-relative 'org-tree-slide-header-overlay-face
                                     :foreground "#283618" :background bg-color :height 0.5)
      (setq face-default (face-remap-add-relative 'default :background bg-color
                              :foreground "grey13" :height 2.0 :family "Hiragino Maru Gothic Pro"))
      (setq face-fringe  (face-remap-add-relative 'fringe  :background bg-color))
      (setq face-minibuf (face-remap-add-relative 'minibuffer-prompt :background bg-color))
      (setq face-link  (face-remap-add-relative 'org-link  :foreground "#606c38"))
      (setq face-level-1 (face-remap-add-relative 'outline-1 :foreground "#99581E" :height 1.5 :weight 'bold))
      (setq org-tree-slide-header nil)
      (setq org-tree-slide-slide-in-effect nil)
      (setq org-tree-slide-exit-at-next-last-slide t)
      (setq-local show-trailing-whitespace nil)
      (org-display-inline-images))

    (defun presen-exit ()
      (face-remap-remove-relative face-default)
      (face-remap-remove-relative face-minibuf)
      (face-remap-remove-relative face-fringe)
      (face-remap-remove-relative face-link)
      (face-remap-remove-relative face-level-1)
      (set-frame-height nil 100)
      (beacon-mode 1)
      (centaur-tabs-mode +1)
      (scroll-bar-mode 1)
      (set-fringe-mode nil)
      (set-window-margins (selected-window) (car margin) (cdr margin))
      (setq-local evil-normal-state-cursor 'box)
      (minibuffer-timer-stop)
      (hide-mode-line-mode 0)
      (my-org-global-fold-set 'hide-all)))

  (lexical-let ((first-paging t))
    (defun presen-timer-reset ()
      (setq first-paging t))

    (defun presen-timer-start ()
      (when first-paging
          (minibuffer-timer-start-force 5)
          (setq first-paging nil)))

    (defun presen-timer-stop ()
      (minibuffer-timer-stop)
      (setq first-paging t)))

    (defun sayonara ()
      (setq buffer-read-only nil)
      (lexical-let ((animate-n-steps 60)
                    (v 8)
                    (h 43))
        (animate-string "おしまい！" v h))
      (sit-for 2)
      (undo))

  (add-hook 'org-tree-slide-before-exit-hook #'sayonara)
  (add-hook 'org-tree-slide-play-hook #'presen-enter)
  (add-hook 'org-tree-slide-stop-hook #'presen-exit)

  (add-hook 'org-tree-slide-play-hook #'presen-timer-reset)
  (add-hook 'org-tree-slide-before-move-next-hook #'presen-timer-start)
  (add-hook 'org-tree-slide-stop-hook #'presen-timer-stop)
  )
;; ----------------------------------------------------------------------
(use-package csharp-mode
  :config
  (add-hook 'csharp-mode-hook
            '(lambda ()
              (setq indent-tabs-mode nil)
              (setq c-basic-offset 4)
              (c-set-offset 'substatement-open 0)
              ;; (flycheck-mode 1)
              (omnisharp-mode)))
  )

;;; ----------------------------------------------------------------------
(use-package slime
  :config
  (setq slime-startup-animation nil)
  (defalias 'slime-reset 'slime-restart-inferior-lisp)

  (defvar ros-exe "path/to/ros.exe")
  (defvar my-slime-helper "path/to/helper.el")
  (setq my-slime-helper (expand-file-name my-slime-helper))

  (unless (file-executable-p ros-exe)
      (error "Not found roswell: %s" ros-exe))
  (unless (file-exists-p my-slime-helper)
      (error "Not found slime helper: %s" helper))
  (load my-slime-helper)
  (setq inferior-lisp-program (format "%s -Q run" ros-exe))
  (add-hook 'slime-load-hook (lambda () (require 'slime-fancy)))
  (slime-setup '(slime-repl slime-fancy slime-banner))

  (defun slime-smart-quit ()
    (interactive)
    (without-yes-or-no
      (when (slime-connected-p)
        (if (equal (slime-machine-instance) "my.workstation")
            (slime-quit-lisp)
          (slime-disconnect)))
      (slime-kill-all-buffers)))

  (add-hook 'kill-emacs-hook 'slime-smart-quit)

    ;; 分割したウィンドウでslime起動
  (defun my-slime (&optional command coding-system)
    "Run slime and split window."
    (interactive)
    (when (< (count-windows) 2)
      (split-window-vertically))
    (slime command coding-system)
    (other-window))

  ;; 選択範囲をslime-replへ送って評価
  (defun slime-repl-send-region (start end)
    "Send region to slime-repl."
    (interactive "r")
    (let ((buf-name (buffer-name (current-buffer)))
          (sbcl-buf (get-buffer "*slime-repl sbcl*")))
      (cond (sbcl-buf
             (copy-region-as-kill start end)
             (switch-to-buffer-other-window sbcl-buf)
             (yank)
             (slime-repl-send-input "\n")
             (switch-to-buffer-other-window buf-name))
            (t (message "Not exist *slime-repl sbcl* buffer!")))))

  ;; LISPモードで新しくファイルを開いたらウィンドウが上下に分割して下にREPL
  (add-hook 'lisp-mode-hook
            (lambda ()
              (global-set-key "\C-cC-h" 'hyperspec-lookup)
              (cond ((not (featurep 'slime))
                     (require 'slime)
                     (normal-mode)))
              (my-slime)))

  (evil-define-key 'normal sldb-mode-map (kbd "M-j") 'centaur-tabs-backward)
  (evil-define-key 'normal sldb-mode-map (kbd "M-k") 'centaur-tabs-forward)

  :bind (:map lisp-mode-map
             ("M-r" . nil)
             ("C-x C-e" . slime-eval-last-expression-in-repl)
             ("C-c C-c" . slime-compile-and-load-file)
             ("C-c C-r" . slime-repl-send-region)
             ("C-c C-f" . slime-compile-defun))
  )
;; ----------------------------------------------------------------------
(use-package company-quickhelp
  :disabled
  :ensure t
  :config
  (setq company-quickhelp-color-background (face-background 'default))
  (setq company-quickhelp-color-foreground (face-foreground 'default))
  )
;; ----------------------------------------------------------------------
(use-package company
  :ensure t
  :config
  (global-company-mode)
  (setq company-idle-delay 0)
  (setq company-minimum-prefix-length 2)
  (setq company-selection-wrap-around t)
  (setq completion-ignore-case nil)
  (setq company-dabbrev-downcase nil)
  ;; (setq company-dabbrev-ignore-case nil)
  ;; (add-to-list 'company-backends 'company-yasnippet)
  ;; (company-quickhelp-mode +1)

  ;; 候補から数字を外す
  (push (apply-partially #'cl-remove-if
                         (lambda (c)
                           (or (string-match-p "[^\x00-\x7F]+" c)
                               (string-match-p "[0-9]+" c)
                               (if (equal major-mode "org")
                                   (>= (length c) 15)))))
        company-transformers)

  ; YASnippet のスニペットを company の候補に表示するための設定
  (defvar company-mode/enable-yas t
    "Enable yasnippet for all backends.")

  (defun company-mode/backend-with-yas (backend)
    (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (defun set-yas-as-company-backend ()
    (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends)))

  (add-hook 'company-mode-hook 'set-yas-as-company-backend)

  (defvar company--insert-candidate2-old-cand nil)
  (defun company--insert-candidate2 (candidate)
    (when (> (length candidate) 0)
      (setq candidate (substring-no-properties candidate))
      (if (eq (company-call-backend 'ignore-case) 'keep-prefix)
          (insert (company-strip-prefix candidate))
        (message "%s    %s" company-prefix candidate)
        ;; (if (equal company-prefix candidate)
        ;;     (company-select-next)
        ;;   (delete-region (- (point) (length company-prefix)) (point))
        ;;   (insert candidate))
        (cond ((equal company-prefix candidate)
               (delete-region (- (point) (length company-prefix)) (point))
               (insert candidate))
              (t
               ()))
        )))

  (defun company-complete-common2 ()
    (interactive)
    (when (company-manual-begin)
      (if (and (not (cdr company-candidates))
               (equal company-common (car company-candidates)))
          (company-complete-selection)
        (company--insert-candidate2 company-common))))

  (define-key company-active-map [tab] 'company-complete-common2)
  ;; (define-key company-active-map [backtab] 'company-select-previous) ; おまけ

  (define-key company-active-map (kbd "M-n") nil)
  (define-key company-active-map (kbd "M-p") nil)
  (define-key company-active-map [tab] 'company-complete-common)
  (define-key company-active-map (kbd "C-e") 'company-complete)
  (define-key company-active-map (kbd "C-n") 'company-select-next)
  (define-key company-active-map (kbd "C-p") 'company-select-previous)
  (define-key company-active-map (kbd "C-j") 'company-select-next)
  (define-key company-active-map (kbd "C-k") 'company-select-previous)
  )
;; ----------------------------------------------------------------------
(use-package yasnippet
  ;; :disabled
  :ensure t
  :config
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))

  (yas-global-mode 1)
  )

;; ----------------------------------------------------------------------
(use-package lsp-mode
  :disabled
  :ensure t
  :commands lsp
  :custom
  ((lsp-print-io t)                     ;; => *lsp-log*
   (lsp-enable-snippet t)
   (lsp-enable-indentation nil)     ;; disable when using ccls
   (lsp-prefer-flymake 'flymake)
   (lsp-prefer-capf nil)              ;; use with company
   (lsp-headerline-breadcrumb-mode t)
   (lsp-document-sync-method 2)
   (lsp-inhibit-message t)
   (lsp-message-project-root-warning nil)
   (create-lockfiles nil))
  :init
  (unbind-key "C-l")
  :bind
  (("C-l C-l"  . lsp)
   ("C-l h"    . lsp-describe-session)
   ("C-l t"    . lsp-goto-type-definition)
   ("C-l r"    . lsp-rename)
   ("C-l <f5>" . lsp-restart-workspace)
   ("C-l l"    . lsp-lens-mode))
  :hook
  (prog-major-mode . lsp-prog-major-mode-enable)
  )
;; ----------------------------------------------------------------------
;; (use-package lsp-ui
;;   ;; :disabled
;;   :commands lsp-ui-mode
;;   :after lsp-mode
;;   :custom
;;   ;; lsp-ui-doc
;;   (lsp-ui-doc-enable t)
;;   (lsp-ui-doc-header t)
;;   (lsp-ui-doc-include-signature t)
;;   (lsp-ui-doc-position 'top)
;;   (lsp-ui-doc-max-width  60)
;;   (lsp-ui-doc-max-height 20)
;;   (lsp-ui-doc-use-childframe t)
;;   (lsp-ui-doc-use-webkit nil)

;;   ;; lsp-ui-flycheck
;;   (lsp-ui-flycheck-enable nil)

;;   ;; lsp-ui-sideline
;;   (lsp-ui-sideline-enable t)
;;   (lsp-ui-sideline-ignore-duplicate t)
;;   (lsp-ui-sideline-show-symbol t)
;;   (lsp-ui-sideline-show-hover t)
;;   (lsp-ui-sideline-show-diagnostics t)
;;   (lsp-ui-sideline-show-code-actions t)

;;   ;; lsp-ui-imenu
;;   (lsp-ui-imenu-enable nil)
;;   (lsp-ui-imenu-kind-position 'top)

;;   ;; lsp-ui-peek
;;   (lsp-ui-peek-enable t)
;;   (lsp-ui-peek-always-show t)
;;   (lsp-ui-peek-peek-height 30)
;;   (lsp-ui-peek-list-width 30)
;;   (lsp-ui-peek-fontify 'always)
;;   :hook
;;   (lsp-mode . lsp-ui-mode)
;;   :bind
;;   (("C-l s"   . lsp-ui-sideline-mode)
;;    ("C-l C-d" . lsp-ui-peek-find-definitions)
;;    ("C-l C-r" . lsp-ui-peek-find-references))
;;   )
;; ----------------------------------------------------------------------
(use-package ccls
  :ensure t
  :disabled
  :custom
  ;; (ccls-executable "/usr/local/bin/ccls")        ;; defined in _mac.el, _windows.el
  (ccls-sem-highlight-method 'font-lock)
  (ccls-use-default-rainbow-sem-highlight)
  ;; :hook ((c-mode c++-mode mql-mode) .
  :hook ((c-mode c++-mode) .
         (lambda () (require 'ccls) (lsp))))
;; ----------------------------------------------------------------------
(use-package dumb-jump
  :ensure t
  :config
  ;; (setq dumb-jump-default-project "")
  ;; (setq dumb-jump-quiet t)
  (setq dumb-jump-force-searcher 'rg)
  ;; (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
  (dumb-jump-mode)
  )
;; ----------------------------------------------------------------------
(use-package jumplist
  :ensure t
  :config
  (define-key evil-motion-state-map (kbd "g p") 'jumplist-previous)
  (define-key evil-motion-state-map (kbd "g o") 'jumplist-next)
  (global-set-key (kbd "C->") 'jumplist-next)

  (setq jumplist-hook-commands '(
          avy-goto-char-timer
          evilmi-jump-items
          isearch-forward isearch-backward
          isearch-forward-regexp isearch-backward-regexp
          evil-search-word-backward evil-search-word-forward
          evil-search-next evil-search-previous
          my-beginning-of-defun
          my-beginning-of-line my-end-of-line
          ))
  )
;; ----------------------------------------------------------------------
(use-package arduino-cli-mode
  :ensure t
  ;; :hook arduino-mode
  ;; :mode "\\.ino\\'"
  :config
  (push "D:/pgm/" exec-path)
  
  :custom
  (arduino-cli-warnings 'all)
  (arduino-cli-verify t)
  )
;; ----------------------------------------------------------------------
(use-package minibuffer-timer)

;; ----------------------------------------------------------------------
(use-package re-builder
  :ensure t
  :config
  (define-key reb-mode-map (kbd "C-x k") 'reb-quit)
  )
;; ----------------------------------------------------------------------
;; customize setting
(setq custom-file "~/.emacs.d/custom.el") ; write custom settings into external file instead of init.el
(load custom-file nil t)

;;; init.el ends here
