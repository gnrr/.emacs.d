;; ----------------------------------------------------------------------
;; packages for melpa
;;   1. type 'M-x package-list-packages'
;;   2. select the package you want and install it
;; ----------------------------------------------------------------------
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)

;; ----------------------------------------------------------------------
;;; unbinding and key binding
;; ----------------------------------------------------------------------
(keyboard-translate ?\C-h ?\C-?)        ; c-h

(global-unset-key (kbd "C-z"))                          ; suspend-frame
(global-unset-key (kbd "C-x C-z"))                      ; suspend-frame
(global-unset-key (kbd "C-x o"))                         ; other-window
(global-unset-key (kbd "M-t"))                          ; transpose-word

(global-set-key (kbd "C-0") 'delete-window)
(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-2") 'split-window-below)
(global-set-key (kbd "C-3") 'split-window-right)
(global-set-key (kbd "C-o") 'other-window)

(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-v") 'new-empty-buffer-other-frame)
(global-set-key ";" 'comment-set-column)         ; c-x ;
(global-set-key [24 67108923] 'comment-indent)     ; c-x c-;
(global-set-key (kbd "C-x t") 'revert-buffer)
(global-set-key (kbd "C-x C-t") 'toggle-truncate-lines)
(global-set-key (kbd "C-x n f") 'narrow-to-defun)

(global-set-key (kbd "M-9") 'insert-parentheses)
(global-set-key (kbd "M-P") 'beginning-of-buffer)
(global-set-key (kbd "M-N") 'end-of-buffer)
(global-set-key (kbd "M-;") 'comment-line)

(define-key isearch-mode-map (kbd "C-b") 'isearch-delete-char)


;; ----------------------------------------------------------------------
;; theme
;; ----------------------------------------------------------------------
(load-theme 'atom-one-dark t)

;; ----------------------------------------------------------------------
;; evil
;; ----------------------------------------------------------------------
(require 'evil)
(evil-mode 1)

(evil-escape-mode 1)
(setq-default evil-escape-delay 0.2)
(setq-default evil-escape-key-sequence "jj")
(setq-default evil-escape-excluded-states '(normal visual multiedit emacs motion))

;; インサートモードではEmacsキーバインド
(setcdr evil-insert-state-map nil)
(define-key evil-insert-state-map [escape] 'evil-normal-state)

(define-key evil-motion-state-map (kbd "C-f") nil)
(define-key evil-motion-state-map (kbd "C-b") nil)
(define-key evil-motion-state-map (kbd "C-o") nil)		; evil-jump-backward

(evil-ex-define-cmd "q[uit]" 'kill-this-buffer)
(define-key evil-normal-state-map (kbd "SPC SPC") 'evil-scroll-down)
(define-key evil-normal-state-map (kbd "S-SPC S-SPC") 'evil-scroll-up) 
(define-key evil-normal-state-map (kbd "E") 'er/expand-region)


;; ----------------------------------------------------------------------
;; ido
;; ----------------------------------------------------------------------
;; (ido-mode 1)
;; (ido-everywhere 1)

;; (setq ido-enable-flex-matching t) ;; 中間/あいまい一致
;; (global-set-key (kbd "M-x") 'smex) ;; for M-x
;; (ido-ubiquitous-mode 1)

;; (smex-initialize) ; Can be omitted. This might cause a (minimal) delay
;;                   ; when Smex is auto-initialized on its first run.
;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; (ido-vertical-mode 1)
;; (ido-yes-or-no-mode 1)

;; (defun my/ido-recentf ()
;;   (interactive)
;;   (find-file (ido-completing-read "Find recent file: " recentf-list)))


;; ----------------------------------------------------------------------
;; helm
;; ----------------------------------------------------------------------
(setq helm-buffers-fuzzy-matching t
      helm-recentf-fuzzy-match    t)

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)

;; ----------------------------------------------------------------------
;; recentf
;; ----------------------------------------------------------------------
(setq recentf-max-saved-items 2000) ;; 2000ファイルまで履歴保存する
;(setq recentf-auto-cleanup 'never)  ;; 存在しないファイルは消さない
(setq recentf-exclude '("/recentf" ".recentf"))
(setq recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))

(recentf-mode 1)
(global-set-key (kbd "M-r") 'helm-recentf)
;; (global-set-key "\M-r" 'my/ido-recentf)

;; ----------------------------------------------------------------------
;; helm-ag
;; ----------------------------------------------------------------------
(setq helm-ag-base-command "ag --nocolor --nogroup")

;; ----------------------------------------------------------------------
;;
;; ----------------------------------------------------------------------
(dumb-jump-mode) 
(setq dumb-jump-selector 'helm)

;; ----------------------------------------------------------------------
;; tabbar
;; ----------------------------------------------------------------------
(tabbar-mode)

(set-face-attribute 'tabbar-default nil
 :family (face-attribute 'fixed-pitch-serif :family)
 :background (face-attribute 'tabbar-default :background)
 :foreground (face-attribute 'tool-bar :foreground)
 :height 0.9)

(set-face-attribute 'tabbar-unselected nil
 :background (face-attribute 'menu :background)
 :foreground (face-attribute 'mode-line-inactive :foreground)
 :box nil)

(set-face-attribute 'tabbar-selected nil
 :background (face-attribute 'default :background)
 :foreground (face-attribute 'mode-line :foreground)
 :box nil)

(set-face-attribute 'tabbar-selected-modified nil
 :background (face-attribute 'default :background)
 :foreground (face-attribute 'mode-line :foreground)
 ;; :underline t
 :box nil)

(set-face-attribute 'tabbar-modified nil
 :background (face-attribute 'menu :background)
 :foreground (face-attribute 'mode-line-inactive :foreground)
 ;; :underline t
 :box nil)

(set-face-attribute 'tabbar-separator nil
 :background (face-attribute 'default :background))

(setq tabbar-separator '(0.2))

(global-set-key (kbd "<f12>") 'tabbar-forward-tab)
(global-set-key (kbd "<f11>") 'tabbar-backward-tab)

(tabbar-mwheel-mode nil)                  ;; マウスホイール無効
(setq tabbar-buffer-groups-function nil)  ;; グループ無効
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
                     ((buffer-file-name b) b)
                     ((char-equal ?\  (aref (buffer-name b) 0)) nil)
                     ((equal "*scratch*" (buffer-name b)) b) ; *scratch*バッファは表示する
                     ((char-equal ?* (aref (buffer-name b) 0)) nil) ; それ以外の * で始まるバッファは表示しない
                     ((buffer-live-p b) b)))
                (buffer-list))))

(setq tabbar-buffer-list-function 'my-tabbar-buffer-list)

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

;; ----------------------------------------------------------------------
;; expand-region
;; ----------------------------------------------------------------------
;(push 'er/mark-outside-pairs er/try-expand-list)

;; ----------------------------------------------------------------------
;; discrete setting
;; ----------------------------------------------------------------------
(tool-bar-mode -1)

(setq linum-format "%5d ")
(global-linum-mode t)
(column-number-mode t)
(set-face-attribute 'linum nil
            :foreground "#898989"
            :background "Gray20"
            :height 0.9)
;; (set-face-background 'fringe "dark red") 

(show-paren-mode 1)
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
(set-face-attribute 'mode-line          nil :box nil) ; モードラインを非3D化
(set-face-attribute 'mode-line-inactive nil :box nil)
(setq scroll-preserve-screen-position t)
(setq initial-scratch-message "")

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

(setq scroll-conservatively most-positive-fixnum)
; カーソルが画面外にはみ出した時10000行以内のはみ出しならスクロールする
(setq next-screen-context-lines 1)

(defun indent-or-insert-tab ()
  (interactive)
  (let ((pos (point)))
    (funcall indent-line-function)
    (when (= pos (point))
      (insert "\t"))))

(global-set-key "\C-i" 'indent-or-insert-tab)

;; カーソル行をハイライト
(global-hl-line-mode t)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(hl-line ((t (:background "#000000")))))

;; c-mode
;;
(add-to-list 'auto-mode-alist '("\\.h$" . c-mode))
(electric-indent-mode)

(add-hook 'c-mode-hook
          (lambda ()
            (c-set-style "stroustrup")
            (define-key c-mode-map "\C-i" 'indent-or-insert-tab)
            (setq comment-column 58)
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
 '(package-selected-packages
   (quote
    (expand-region tabbar ag ido-vertical-mode ido-yes-or-no dumb-jump helm atom-one-dark-theme mic-paren evil-escape evil))))

