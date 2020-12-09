;;; -*- coding:utf-8; mode:emacs-lisp -*-
;;;
;;; _mac.el
;;;
(message "--> loading \"_mac.el\"...")

;;;
;;; window size and position
;;;
(setq initial-frame-alist (append '(
				    (top    . 0)
				    (left   . 529)      ;; Macbook Pro 13"
				    ;; (left   . 71)
				    (height . 64)
				    (width  . 110))
				  initial-frame-alist))
(setq default-frame-alist initial-frame-alist)

;;;
;;; font
;;;
(set-default-font (myfont 'default))

;;;
;;; external program
;;;
(add-to-list 'exec-path (expand-file-name "~/bin"))
(setq my-counsel-rg-exe (expand-file-name "~/bin/rg"))

;; migemo
  ;; fixme not work
  (setq migemo-command "/usr/local/bin/cmigemo")
  (setq migemo-dictionary "/usr/local/share/migemo/utf-8/migemo-dict")

;; (add-hook 'after-init-hook 'mac-change-language-to-us)          ;; emacs 起動時は英数モードから始める
;; (add-hook 'minibuffer-setup-hook 'mac-change-language-to-us)    ;; minibuffer 内は英数モードにする
;; (add-hook 'isearch-mode-hook 'mac-change-language-to-us)        ;; [migemo]isearch のとき IME を英数モードにする

;; ;; アイコンやdockから起動したemacsのpathやexec-pathが正しく設定されてないのをなんとかする
;; ;; http://yukihr.github.com/blog/2012/03/02/emacs-path-settings/
;; ;; when opened from desktep entry, path won't be set to shell's value.
;; (let ((path-str
;;        (replace-regexp-in-string "\n+$" "" (shell-command-to-string "echo $path"))))
;;     (setenv "path" path-str)
;;     (setq exec-path (nconc (split-string path-str ":") exec-path)))


;; 日本語環境設定 for mac
;; http://maro.air-nifty.com/maro/2009/02/carbon-emacs-sh.html
;; (set-language-environment "japanese")

(set-language-environment 'utf-8)
(prefer-coding-system 'utf-8-unix)
(setq default-buffer-file-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-clipboard-coding-system 'utf-8)

;; (setq coding-system-for-read 'utf-8-unix)
;; (setq coding-system-for-write 'utf-8-unix)

;; http://sakito.jp/emacs/emacsshell.html#id7
(cond ((or (eq window-system 'mac) (eq window-system 'ns))
       ;; mac os x の hfs+ ファイルフォーマットではファイル名は nfd (の様な物)で扱うため以下の設定をする必要がある
       (require 'ucs-normalize)
       (setq file-name-coding-system 'utf-8-hfs)
       (setq locale-coding-system 'utf-8-hfs))
      ((or (eq system-type 'cygwin) (eq system-type 'windows-nt)
          (setq file-name-coding-system 'utf-8)
          (setq locale-coding-system 'utf-8)
          ;; もしコマンドプロンプトを利用するなら sjis にする
          ;; (setq file-name-coding-system 'sjis)
          ;; (setq locale-coding-system 'sjis)
          ;; 古い cygwin だと euc-jp にする
          ;; (setq file-name-coding-system 'euc-jp)
          ;; (setq locale-coding-system 'euc-jp)
          ))
      (t
       (setq file-name-coding-system 'utf-8)
       (setq locale-coding-system 'utf-8)))

;;
;; for emacs mac port
;;

;; ;; ミニバッファで入力する際に自動的にASCIIにする
;; (when (fboundp 'mac-auto-ascii-mode)
;;   (mac-auto-ascii-mode 1))

;; for im-on/off
(defun im-ctl (on)
  (let ((code (if on 104 102)))
    (start-process "im-ctl" nil "osascript" "-e"
        (format "tell application \"System Events\" to key code %d" code))))
    ;; (call-process
    ;;  "osascript" nil t nil "-e"
    ;;  (format "tell application \"System Events\" to key code %d" code))))

;;
;; 入力モードが日本語の時はカーソルの色を変える
;;
(defvar my-mac-selected-keyboard-input-source-change-bak (face-background 'cursor))
(defun my-mac-selected-keyboard-input-source-change ()
  (set-cursor-color (if (string-match "\\.US$" (mac-input-source))
                        my-mac-selected-keyboard-input-source-change-bak
                      (mycolor 'red))))

(add-hook 'mac-selected-keyboard-input-source-change-hook 'my-mac-selected-keyboard-input-source-change)

;;
;; fullscreen
;;
(global-set-key (kbd "C-M-f") 'toggle-frame-fullscreen)

;;
;; check-emacs-setting
;;
(setq check-emacs-setting-diff-pgm "/Applications/Meld.app/Contents/MacOS/Meld")
(setq check-emacs-setting-cmp-pgm "cmp")

(message "<-- done    \"_mac.el\"")

;;
;; _mac.el ends here
;;
