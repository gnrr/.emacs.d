;;; -*- coding:utf-8; mode:emacs-lisp -*-
;;;
;;; _windows.el
;;;
(message "--> loading \"_windows.el\"...")

;;;
;;; window size and position
;;;
(setq initial-frame-alist '(
				    (top    . 0)
				    (left   . 2358)     ;; for 4K display
				    ;; (left   . 71)
				    (height . 85)
				    (width  . 119)))
(setq default-frame-alist initial-frame-alist)

;;;
;;; default font
;;;

(let ((font (myfont 'default3)))
  (when font
    (set-default-font font)))

(set-face-attribute 'default nil :height 122)
(set-face-attribute 'mode-line nil :height 105)
(set-face-attribute 'minibuffer-prompt nil :height 105)

;;;
;;; external program
;;;
;; (add-to-list 'exec-path (expand-file-name "~/bin"))
;; (setq my-counsel-rg-exe (expand-file-name "~/bin/rg"))

;; migemo
  ;; fixme not work
  ;; (setq migemo-command "/usr/local/bin/cmigemo")
  ;; (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")

;;;
;;; appearance adjust
;;;
(setq my-face-adj-line-number-height 1.0)

;;;
;;; IME settings
;;;
(require 'wdired)

;; IMEの初期化
(w32-ime-initialize)

;; 標準IMEの設定
(setq default-input-method "W32-IME")

;; IME状態のモードライン表示の設定
(setq-default w32-ime-mode-line-state-indicator "")
(setq w32-ime-mode-line-state-indicator-list '("" "" ""))

;; IME OFF時の初期カーソルカラーの設定
(defvar my-windows-cursor-color-bak (face-background 'cursor))
(set-cursor-color my-windows-cursor-color-bak)

;; IME ON/OFF時のカーソルカラーの設定
;; （wrap-function-to-control-ime コマンド内等で、ime-force-on や ime-force-off が単独で
;; 　呼ばれた際もカーソルカラーの変更が機能するように hook ではなく advice に変更した）
(advice-add 'ime-force-on :before (lambda (&rest args)
                      (set-cursor-color (mycolor 'red))))
(advice-add 'ime-force-off :before (lambda (&rest args)
                      (set-cursor-color my-windows-cursor-color-bak)))

;; カーソルの点滅を OFF にする
(blink-cursor-mode 0)

;; バッファ切り替え時に IME の状態を引き継がない
(setq w32-ime-buffer-switch-p t)

;; minibuffer に入った時、IME を OFF にする
(add-hook 'minibuffer-setup-hook (lambda () (deactivate-input-method)))
(add-hook 'helm-minibuffer-set-up-hook (lambda () (deactivate-input-method)))

;; IMEの制御付きにラップする
;;（上記の minibuffer の設定が機能しない関数について設定する）
;; http://sanrinsha.lolipop.jp/blog/2010/07/emacs-1.html#i-4
(wrap-function-to-control-ime 'y-or-n-p nil nil)
(wrap-function-to-control-ime 'map-y-or-n-p nil nil)
(wrap-function-to-control-ime 'read-char nil nil)

;; for im-on/off
(defun im-ctl (on)
  (let ((code (if on 104 102)))
    ;; (start-process "im-ctl" nil "osascript" "-e"
    ;;     (format "tell application \"System Events\" to key code %d" code))))
    ))  ;; todo

;; isearch の設定
;; http://d.hatena.ne.jp/ksugita0510/20110103/p1
;; http://highmt.wordpress.com/2010/10/25/isearch%E3%81%A7%E6%97%A5%E6%9C%AC%E8%AA%9E%E5%85%A5%E5%8A%9B%E3%82%92%E3%82%84%E3%82%8A%E3%82%84%E3%81%99%E3%81%8F%E3%81%99%E3%82%8B%E3%83%91%E3%83%83%E3%83%81/
(defun w32-isearch-update ()
  (interactive)
  (isearch-update))

(add-hook 'isearch-mode-hook
          (lambda ()
            (setq ime-state (ime-get-mode))
            (when ime-state
              (w32-ime-state-switch nil))
            (setq w32-ime-composition-window (minibuffer-window))))

(add-hook 'isearch-mode-end-hook
          (lambda ()
            (unless (eq ime-state (ime-get-mode))
              (if ime-state
                  (w32-ime-state-switch t)
                (w32-ime-state-switch nil)))
            (setq w32-ime-composition-window nil)))

(defun enable-input-method (&optional arg interactive)
  (interactive "P\np")
  (if (not current-input-method)
      (toggle-input-method arg interactive)))

(defun disable-input-method (&optional arg interactive)
  (interactive "P\np")
  (if current-input-method
      (toggle-input-method arg interactive)))

(defun isearch-enable-input-method ()
  (interactive)
  (if (not current-input-method)
      (isearch-toggle-input-method)
    (cl-letf (((symbol-function 'toggle-input-method)
               (symbol-function 'ignore)))
      (isearch-toggle-input-method))))

(defun isearch-disable-input-method ()
  (interactive)
  (if current-input-method
      (isearch-toggle-input-method)
    (cl-letf (((symbol-function 'toggle-input-method)
               (symbol-function 'ignore)))
      (isearch-toggle-input-method))))

;; (define-key isearch-mode-map (kbd "<compend>") 'w32-isearch-update)
(define-key isearch-mode-map (kbd "<kanji>") 'isearch-toggle-input-method)

;; IME をトグルするキー設定
(global-set-key (kbd "<kanji>") 'toggle-input-method)
(define-key isearch-mode-map (kbd "<kanji>") 'isearch-toggle-input-method)
(define-key wdired-mode-map (kbd "<kanji>") 'toggle-input-method)

;; IME を無効にするキー設定
;; (global-set-key (kbd "<non-convert>") 'disable-input-method) ; 無変換キー
;; (define-key isearch-mode-map (kbd "<non-convert>") 'isearch-disable-input-method)
;; (define-key wdired-mode-map (kbd "<non-convert>") 'disable-input-method)

;; (global-set-key (kbd "C-j") 'disable-input-method)
;; (define-key isearch-mode-map (kbd "C-j") 'isearch-disable-input-method)
;; (define-key wdired-mode-map (kbd "C-j") 'disable-input-method)

;; IME を有効にするキー設定
;; (global-set-key (kbd "<convert>") 'enable-input-method) ; 変換キー
;; (define-key isearch-mode-map (kbd "<convert>") 'isearch-enable-input-method)
;; (define-key wdired-mode-map (kbd "<convert>") 'enable-input-method)

;; (global-set-key (kbd "C-o") 'enable-input-method)
;; (define-key isearch-mode-map (kbd "C-o") 'isearch-enable-input-method)
;; (define-key wdired-mode-map (kbd "C-o") 'enable-input-method)

;; wdired 終了時に IME が ON になっていたら OFF にする
(advice-add 'wdired-finish-edit
            :after (lambda (&rest args)
                     (deactivate-input-method)))


;;
;; fullscreen
;;
(global-set-key (kbd "C-M-f") 'toggle-frame-fullscreen)

;; path
;; (add-to-list 'load-path "c:/Users/g/AppData/Roaming/.emacs.d/elisp/zerodark-theme")
(add-to-list 'exec-path "c:/Users/g/.emacs.d/bin-windows")

;;
;; my-counsel-rg
;;
(setq my-counsel-rg-exe "c:/Users/g/.emacs.d/bin-windows/rg.exe")
(setq my-counsel-rg-dot-ignore-path "~/.ignore")
(setq my-counsel-rg-dot-ignore-list '("*bak*" "#*#"))

;; org-mode
(setq org-directory "D:/Dropbox/org")


;;
;; check-emacs-setting
;;
(setq check-emacs-setting-diff-pgm "C:/Program\ Files/WinMerge/WinMergeU.exe")
(setq check-emacs-setting-cmp-pgm "c:/GnuWin32/bin/cmp.exe")

(message "<-- done    \"_windows.el\"")

(provide '_windows)
;; _windows.el ends here
