;; ----------------------------------------------------------------------
;; packages for melpa
;;   1. type 'M-x package-list-packages'
;;   2. select the package you want and install it
;; ----------------------------------------------------------------------
(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))
(package-initialize)
(unless (require 'use-package nil t)
  (defmacro use-package (&rest args)))

; ----------------------------------------------------------------------
;;; defaults
;; ----------------------------------------------------------------------
(setq-default
 auto-window-vscroll nil                          ; Lighten vertical scroll
 confirm-kill-emacs 'yes-or-no-p                  ; Confirm before exiting Emacs
 delete-by-moving-to-trash t                      ; Delete files to trash
 display-time-default-load-average nil            ; Don't display load average
 display-time-format "%H:%M"                      ; Format the time string
 fill-column 80                                   ; Set width for automatic line breaks
 indent-tabs-mode nil                             ; Stop using tabs to indent
 inhibit-startup-screen t                         ; Disable start-up screen
 initial-scratch-message ""                       ; Empty the initial *scratch* buffer
 left-margin-width 1 right-margin-width 1         ; Add left and right margins
 scroll-margin 10                                 ; Add a margin when scrolling vertically
 select-enable-clipboard t                        ; Merge system's and Emacs' clipboard
 sentence-end-double-space nil                    ; End a sentence after a dot and a space
 show-trailing-whitespace nil                     ; Display trailing whitespaces
 tab-width 4                                      ; Set width for tabs
 uniquify-buffer-name-style 'forward              ; Uniquify buffer names
 window-combination-resize t                      ; Resize windows proportionally

 vc-follow-symlinks t
 ring-bell-function 'ignore
 parens-require-spaces nil
 transient-mark-mode nil
 initial-scratch-message ""
 indent-tabs-mode nil
 tab-width 4
 tab-stop-list  '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60)

 ;; 1行スクロール
 ;; (setq scroll-conservatively most-positive-fixnum)
 scroll-margin 5
 next-screen-context-lines 5
 scroll-preserve-screen-position t
 )

(fset 'yes-or-no-p 'y-or-n-p)                     ; Replace yes/no prompts with y/n
(global-hl-line-mode 1)                           ; Hightlight current line
(tool-bar-mode -1)
(menu-bar-mode 0)                                 ; Disable the menu bar
(save-place-mode 1)                               ; Enable saveplace
(add-hook 'focus-out-hook #'garbage-collect)

(setq cursor-type 'box)
(blink-cursor-mode 0)

;; show-paren
(show-paren-mode 1)
(setq show-paren-delay 0.3)
(setq show-paren-style 'expression)
(set-face-background 'show-paren-match "#263652")

(setq indent-line-function 'indent-relative-maybe)
(global-set-key "\C-m" 'newline-and-indent)  ; Returnキーで改行＋オートイン

;; mode-line
(set-face-attribute 'mode-line          nil :box nil) ; モードラインを非3D化
(set-face-attribute 'mode-line-inactive nil :box nil)

;; linum
(global-linum-mode t)
(column-number-mode t)
(setq linum-format "%5d")
(set-face-attribute 'linum nil
            :foreground "#898989"
            :background "Gray20"
            :height 0.9)

;; tab
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
(global-set-key (kbd "C-x ;") 'comment-set-column)         ; c-x ;
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
(use-package atom-one-dark-theme
;; ----------------------------------------------------------------------
  :config
  (load-theme 'atom-one-dark t)
  )

;; ----------------------------------------------------------------------
(use-package dashboard
;; ----------------------------------------------------------------------
;; :defer t
  :config
  (setq inhibit-startup-message t)
  (setq dashboard-banner-logo-title "Life with Evil")
  (setq dashboard-startup-banner "~/.emacs.d/img/e_splash.svg")
  (dashboard-setup-startup-hook)
  (setq dashboard-items '((recents  . 20)))
  ;; (widget-forward 1)
  )

;; ----------------------------------------------------------------------
(use-package evil
;; ----------------------------------------------------------------------
  :config
  (evil-mode 1)
  (evil-set-initial-state 'edebug-mode 'emacs)
  (defalias #'forward-evil-word #'forward-evil-symbol)

  ;; インサートモードではEmacsキーバインド
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map [escape] 'evil-normal-state)

  (define-key evil-motion-state-map (kbd "C-f") nil)
  (define-key evil-motion-state-map (kbd "C-b") nil)
  (define-key evil-motion-state-map (kbd "C-o") nil)		; evil-jump-backward
  (define-key evil-normal-state-map (kbd "C-n") nil)
  (define-key evil-normal-state-map (kbd "C-p") nil)
  (define-key evil-normal-state-map (kbd "C")   nil)

  (define-key evil-normal-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-normal-state-map (kbd "k") 'evil-previous-visual-line)

  (evil-ex-define-cmd "q[uit]" 'kill-this-buffer)
  (define-key undo-tree-map (kbd "U") 'undo-tree-redo)
  (define-key evil-normal-state-map (kbd "SPC SPC") 'evil-scroll-down)
  (define-key evil-normal-state-map (kbd "S-SPC S-SPC") 'evil-scroll-up)

  (defun evil-return-insert-mode-after-save ()
    (when evil-insert-state-minor-mode
      (funcall (evil-escape--escape-normal-state))))
  (add-hook 'after-save-hook 'evil-return-insert-mode-after-save)
  )

(use-package evil-escape
  :config
  (evil-escape-mode 1)
  (setq-default evil-escape-delay 0.2)
  (setq-default evil-escape-key-sequence "jj")
  (setq-default evil-escape-excluded-states '(normal visual multiedit emacs motion))
)

;; ----------------------------------------------------------------------
(use-package all-the-icons)
;; ----------------------------------------------------------------------

;; ----------------------------------------------------------------------
(use-package neotree
;; ----------------------------------------------------------------------
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-show-hidden-files t)
  (setq neo-create-file-auto-open t)
  (setq neo-smart-open nil)
  (setq neo-persist-show t)
  (setq neo-keymap-style 'concise)
  (global-set-key (kbd "C-q") 'neotree-toggle)
  (evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
  (evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
  (evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
  (defun text-scale-twice ()
    (interactive)
    (text-scale-adjust 0)
    (text-scale-decrease 1))
  (add-hook 'neo-after-create-hook (lambda (_)(call-interactively 'text-scale-twice)))
  )

;; ----------------------------------------------------------------------
(use-package helm
;; ----------------------------------------------------------------------
  :config
  (helm-mode 1)
  (setq helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t)

  (setq-default helm-ff-skip-boring-files t)
  (add-to-list 'helm-boring-file-regexp-list "scratch-log-.*$")
  
  ;; (setq helm-ag-base-command "ag --nocolor --nogroup")
  (setq helm-ag-base-command "rg --vimgrep --no-heading")		; ripgrep
  (setq helm-ag-insert-at-point 'symbol)

  :bind (("M-x" . helm-M-x)
	     ("M-y" . helm-show-kill-ring)
	     ("C-x b" . helm-mini)
	     ("C-x C-f" . helm-find-files)
	     ("C-x C-b" . helm-buffers-list))

  ;; (:map helm-find-files-map ("TAB" . helm-execute-persistent-action))
  ;; (define-key helm-read-file-map (kbd "TAB") 'helm-execute-persistent-action)
)

;; ----------------------------------------------------------------------
(use-package helm-gtags
;; ----------------------------------------------------------------------
  :config
  (helm-gtags-mode t)
  (setq helm-gtags-auto-update t)

  (defun gtags-update ()
    (interactive)
    (let ((s (shell-command-to-string "global -uv")))
      (if (string-match "not found" s)
          (call-interactively 'helm-gtags-create-tags)
        (message "Updated GTAGS files."))))

  :bind (:map evil-normal-state-map
              ("g t" . helm-gtags-dwim)
              ("g d" . helm-gtags-find-tag)
              ("g r" . helm-gtags-find-rtag)
              ("g s" . helm-gtags-find-symbol)
              ("g h" . helm-gtags-previous-history)
              ("g l" . helm-gtags-next-history))
)

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
(use-package recentf
;; ----------------------------------------------------------------------
  :config
  (setq recentf-max-saved-items 5000) ;; 履歴保存の数
  ;; (setq recentf-auto-cleanup 'never)  ;; 存在しないファイルは消さない
  (setq recentf-exclude '("/recentf" ".recentf"))
  ;; (setq recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))

  (recentf-mode 1)
  (global-set-key (kbd "M-r") 'helm-recentf)
  ;; (global-set-key "\M-r" 'my/ido-recentf)
  )

;; ----------------------------------------------------------------------
(use-package dumb-jump
;; ----------------------------------------------------------------------
  :config
  (setq dumb-jump-default-project "") ;; これをしないとホームディレクトリ以下が検索対象になる
  ;; (setq dumb-jump-force-searcher 'rg) ;; 日本語を含むパスだとgit grepがちゃんと動かない…
  (dumb-jump-mode) ;; 標準キーバインドを有効にする
  (setq dumb-jump-selector 'helm)
  )

;; ----------------------------------------------------------------------
(use-package tabbar
;; ----------------------------------------------------------------------
  :config
  (tabbar-mode)

  (set-face-attribute 'tabbar-default nil
                      :family (face-attribute 'fixed-pitch-serif :family)
                      ;; :background (face-attribute 'tabbar-default :background)
                      ;; :background (face-attribute 'linum :background)
                      :foreground (face-attribute 'tool-bar :foreground)
                      ;; :weight 'light
                      ;; :slant 'normal
                      :height 1.0)

  (set-face-attribute 'tabbar-unselected nil
                      :background (face-attribute 'tabbar-default :background)
                      :background (face-attribute 'tool-bar :background)
                      ;; :foreground (face-attribute 'mode-line-inactive :foreground)
                      :foreground "#080808"
                      :box nil)

  (set-face-attribute 'tabbar-selected nil
                      :background (face-attribute 'default :background)
                      ;; :foreground (face-attribute 'mode-line :foreground)
                      :foreground "#E8E8E8"
                      ;; :slant 'normal
                      :box nil)

  (set-face-attribute 'tabbar-selected-modified nil
                      :background (face-attribute 'default :background)
                      :foreground (face-attribute 'mode-line :foreground)
                      ;; :slant 'italic
                      :box nil)

  (set-face-attribute 'tabbar-modified nil
                      :background (face-attribute 'menu :background)
                      :foreground (face-attribute 'mode-line-inactive :foreground)
                      ;; :slant 'italic
                      ;; :underline t
                      :box nil)

  (set-face-attribute 'tabbar-separator nil
                      :background (face-attribute 'default :background))

  (setq tabbar-separator '(0.2))

  (define-key evil-normal-state-map (kbd "M-l") 'tabbar-forward-tab)
  (define-key evil-normal-state-map (kbd "M-h") 'tabbar-backward-tab)

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
                         (length (tabbar-riew
                                  (tabbar-current-tabset)))))))))
  )

;; ----------------------------------------------------------------------
(use-package expand-region
;; ----------------------------------------------------------------------
  :config
  (push 'er/mark-outside-pairs er/try-expand-list)
  (define-key evil-normal-state-map (kbd "+") 'er/expand-region)
  ;; (define-key evil-visual-state-map (kbd "x") 'er/expand-region)
  ;; (define-key evil-visual-state-map (kbd "X") 'er/contract-region)
  (define-key evil-visual-state-map (kbd "+") 'er/expand-region)
  (define-key evil-visual-state-map (kbd "_") 'er/contract-region)
  (setq expand-region-smart-cursor t)
  )

;; ----------------------------------------------------------------------
(use-package scratch-log
;; ----------------------------------------------------------------------
  :config
  ;; (setq sl-scratch-log-file "~/.emacs.d/.scratch-log")  ;; default
  ;; (setq sl-prev-scratch-string-file "~/.emacs.d/.scratch-log-prev")
  (setq sl-restore-scratch-p t)           ;復元
  (setq sl-prohibit-kill-scratch-buffer-p t) ;削除不能
  ;; *scratch*とscratch-logのメジャーモードをorg-modeにする
  (setq initial-major-mode 'org-mode)
  (add-to-list 'auto-mode-alist '("scratch-log" . org-mode))
  ;; 30秒ごとに自動保存
  (setq sl-use-timer t)
  ;; (setq sl-timer-interval 3)

  (add-to-list 'recentf-exclude "scratch-log-autoloads.el")
  ) 

;; ----------------------------------------------------------------------
(use-package quick-back
;; ----------------------------------------------------------------------
  :load-path "elisp"
  :bind (:map evil-normal-state-map
              ("g m" . quick-back-mark)
              ("g q" . quick-back-jump))
  )

;; ----------------------------------------------------------------------
(use-package discrete
;; ----------------------------------------------------------------------
  :load-path "elisp"
  )

;; ----------------------------------------------------------------------
(use-package my-backup
;; ----------------------------------------------------------------------
  :load-path "elisp"
  :config
  (setq my-backup-directory "~/bak")
  )

;; ----------------------------------------------------------------------
;; discrete setting
;; ----------------------------------------------------------------------
(setq truncate-partial-width-windows nil)
(setq-default truncate-lines t)

;; kill-ringに同じ内容を重複して入れない
(defadvice kill-new (before ys:no-kill-new-duplication activate)
  (setq kill-ring (delete (ad-get-arg 0) kill-ring)))

;; prevent annoying message "Text is read only" at mimibuffer
(plist-put minibuffer-prompt-properties
           'point-entered 'minibuffer-avoid-prompt)

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
 '(hl-line ((t (:background "#080808")))))

;; c-mode
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

(defalias 'com 'comment-region)
(defalias 'ind 'indent-region)

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
; computer independent
;; ----------------------------------------------------------------------
(load
 (cond ((eq system-type 'windows-nt) "~/.emacs.d/elisp/_windows.el")
       ((eq system-type 'gnu/linux)  "~/.emacs.d/elisp/_linux.el")
       (t                            "~/.emacs.d/elisp/_mac.el")))

;;ins:tkkk
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
    (helm-gtags scratch-log neotree all-the-icons markdown-mode expand-region helm-ag dashboard use-package tabbar ag ido-vertical-mode ido-yes-or-no dumb-jump helm atom-one-dark-theme mic-paren evil-escape evil))))

