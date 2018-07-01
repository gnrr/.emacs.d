;; ----------------------------------------------------------------------
;; packages for melpa
;;   1. type 'M-x package-list-packages'
;;   2. select the package you want and install it
;;   3. type 'package-initialize'
;; ----------------------------------------------------------------------
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)


;; ----------------------------------------------------------------------
;; theme
;; ----------------------------------------------------------------------
(load-theme 'atom-one-dark t)

;; ----------------------------------------------------------------------
;; evil
;; ----------------------------------------------------------------------
(require 'evil)
(evil-mode 1)

;; evil-escape
(evil-escape-mode 1)
(setq-default evil-escape-delay 0.2)
(setq-default evil-escape-key-sequence "jj")

;; インサートモードではEmacs キーバインド
(setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map [escape] 'evil-normal-state)

;; ----------------------------------------------------------------------
;; discrete setting
;; ----------------------------------------------------------------------
(tool-bar-mode -1)

(setq linum-format "%5d")
(global-linum-mode t)
(column-number-mode t)

;(show-paren-mode 1)
(setq vc-follow-symlinks t)

(setq inhibit-startup-message t)
(blink-cursor-mode 0)
(setq cursor-type 'box)

(setq ring-bell-function 'ignore)
(setq parens-require-spaces nil)
(setq transient-mark-mode nil)

(setq indent-line-function 'indent-relative-maybe)
(global-set-key "\C-m" 'newline-and-indent)  ; Returnキーで改行＋オートイン
;; mode-line
;; (set-face-attribute 'mode-line          nil :box nil) ; モードラインを非3D化
;; (set-face-attribute 'mode-line-inactive nil :box nil)
;; (setq scroll-preserve-screen-position t)
;; (setq initial-scratch-message "")

;; tab
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq tab-stop-list  '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60))

(setq default-truncate-lines t)

;; kill-ringに同じ内容を重複して入れない
(defadvice kill-new (before ys:no-kill-new-duplication activate)
  (setq kill-ring (delete (ad-get-arg 0) kill-ring)))

;; prevent annoying message "Text is read only" at mimibuffer
(plist-put minibuffer-prompt-properties
           'point-entered 'minibuffer-avoid-prompt)

;(setq scroll-step 1)
(setq scroll-conservatively most-positive-fixnum)
; カーソルが画面外にはみ出した時10000行以内のはみ出しならスクロールする
;(setq next-screen-context-lines 1)

(defun indent-or-insert-tab ()
  (interactive)
  (let ((pos (point)))
    (funcall indent-line-function)
    (when (= pos (point))
      (insert "\t"))))

(global-set-key "\C-i" 'indent-or-insert-tab)

;;;
;;; unbinding and key binding
;;;
(keyboard-translate ?\C-h ?\C-?)        ; c-h


(global-unset-key "\C-z")                          ; suspend-frame
(global-unset-key "\C-x\C-z")                      ; suspend-frame
(global-unset-key "\C-xo")                         ; other-window
(global-unset-key "\M-t")                          ; transpose-word

(global-set-key "\C-o" 'other-window)

(global-set-key "\M-g" 'goto-line)
(global-set-key "\M-v" 'new-empty-buffer-other-frame)
(global-set-key ";" 'comment-set-column)         ; c-x ;
(global-set-key [24 67108923] 'comment-indent)     ; c-x c-;
(global-set-key "\C-xt" 'revert-buffer)            ; c-x t
(global-set-key "\C-xnf" 'narrow-to-defun)
(global-set-key "\C-x\C-t" 'toggle-truncate-lines) ; c-x c-t

(global-set-key "\M-9" 'insert-parentheses)
(global-set-key "\M-P" 'beginning-of-buffer)
(global-set-key "\M-N" 'end-of-buffer)
;; (global-set-key "\M-p" 'View-scroll-page-backward)
;; (global-set-key "\M-n" 'View-scroll-page-forward)

(define-key isearch-mode-map (kbd "C-b") 'isearch-delete-char)

;;
;; c-mode
;;
(add-to-list 'auto-mode-alist '("\\.h$" . c-mode))
(electric-indent-mode)

(add-hook 'c-mode-hook
          (lambda ()
            (c-set-style "stroustrup")
            (define-key c-mode-map "\C-i" 'indent-or-insert-tab)
            (setq comment-column 52)
            (modify-syntax-entry ?_ "w")                ; アンダーバーをワード区切りとしない
            (setq comment-start "// ")                  ; コメントを // にする
            (setq comment-end "")
            (cwarn-mode)
            (setq compilation-read-command nil)         ; make のオプションの確認は不要
            (setq compilation-ask-about-save nil)       ; make するとき save する
            (setq compile-command "make")               ; make時のデフォルトコマンド
            (setq case-fold-search nil)))               ; case sensitive

(eval-after-load "align"
  '(add-to-list 'align-rules-list
                '(tab-stop-assignment
                  (regexp   . "\\(\\s-+\\)")
                  (tab-stop . t)              ; タブ位置でそろえる
                  (modes     . '(c-mode c++-mode)))))

(setq auto-mode-alist
      (cons '("\\.emacs.*$" . lisp-interaction-mode)
        auto-mode-alist))

;;
;; bs-mode / bs-cycle-buffer (pre-installed)
;;
;; (global-set-key [(f11)]       'bs-cycle-previous)
;; (global-set-key [(f12)]       'bs-cycle-next)
;(setq bs-default-configuration "files-and-scratch")
;(setq bs-cycle-configuration-name "files-and-scratch")

;; (defun my-bs-toggle-configuration ()
;;   (interactive)
;;   (bs-set-configuration (if (string= bs-current-configuration bs-default-configuration)
;;                                         bs-cycle-configuration-name
;;                                       bs-default-configuration))
;;   (bs--redisplay t))


;; (add-hook 'bs-mode-hook 
;;           '(lambda ()
;;              (define-key bs-mode-map "j" 'bs-down)
;;              (define-key bs-mode-map "k" 'bs-up)
;;              (define-key bs-mode-map "/" 'isearch-forward)
;;              (define-key bs-mode-map "s" 'isearch-forward)
;;              (define-key bs-mode-map "\C-x\C-b" 'my-bs-toggle-configuration)))



;;;
;;; command aliases
;;;

;; iro mihon
(defalias 'color-list 'list-colors-display)

;; edebug-defun
(require 'edebug)
(eval-after-load "edebug"
  '(defalias 'ede 'edebug-defun))

;; apropos
(defalias 'a 'apropos)
(defalias 'l 'linum-mode)

(defalias 'yes-or-no-p 'y-or-n-p)

(defalias 'com 'comment-region)
(defalias 'ind 'indent-region)

;;
;; saveplace
;;
;(require 'saveplace)
;(setq save-place-limit 50)
;(setq-default save-place t)

;;
;; mic-paren
;;
;(setq paren-match-face 'region)  ;; coloring inside between each parentheses
;(setq paren-sexp-mode t)
;(setq show-paren-delay 1)
;(setq paren-delay 0.5)
;(paren-activate)

;; jump to paren
(defvar my-paren-open "\\s(")
(make-variable-buffer-local 'my-paren-open)
(defvar my-paren-close "\\s)")
(make-variable-buffer-local 'my-paren-close)

(defun my-paren (ARG)
  (interactive "P")
  (let ((FOL-CHAR (char-to-string (following-char)))
        (PRE-CHAR (char-to-string (preceding-char))))
    (save-match-data
      (cond
       ((and (string-match my-paren-open FOL-CHAR) (string-match my-paren-close PRE-CHAR))
  (if ARG (paren-forward-sexp) (paren-backward-sexp)))
       ((string-match my-paren-open FOL-CHAR)  (paren-forward-sexp))
       ((string-match my-paren-close PRE-CHAR) (paren-backward-sexp))
       (t (re-search-backward my-paren-open))))))

(global-set-key "\M-]" 'my-paren)


;; ----------------------------------------------------------------------
;; my discrete functions
;; ----------------------------------------------------------------------
(add-to-list 'load-path "~/.emacs.d/elisp")
(load "~/.emacs.d/elisp/discrete.el")

(require 'my-backup)
(setq my-backup-directory "~/bak")

;; ----------------------------------------------------------------------
; computer independent
;; ----------------------------------------------------------------------
(load
 (cond ((eq system-type 'windows-nt) "~/.emacs.d/elisp/_windows.el")
       ((eq system-type 'gnu/linux)  "~/.emacs.d/elisp/_linux.el")
       (t                            "~/.emacs.d/elisp/_mac.el")))


;; ----------------------------------------------------------------------
;; automatically added
;; ----------------------------------------------------------------------
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("78496062ff095da640c6bb59711973c7c66f392e3ac0127e611221d541850de2" default)))
 '(package-selected-packages (quote (atom-one-dark-theme mic-paren evil-escape evil))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
