;;; -*- coding:utf-8; mode:emacs-lisp -*-
;;;
;;; _mac.el
;;;
(message "loading _mac.el")

;;;
;;; window size and position
;;;
;; (tool-bar-mode 0)
(setq default-frame-alist (append '(
				    (top    . 0)
				    ;(left   . 1007)
				    (left   . 71)
				    ;; (left   . 0)
				    (height . 64)
				    (width  . 90))
				  default-frame-alist))

;;;
;;; font
;;;
;; (set-face-attribute 'default nil :family "menlo" :height 135)
;; (set-fontset-font (frame-parameter nil 'font) 'japanese-jisx0208 (font-spec :family "hiragino maru gothic pron") nil 'append) 
;; (set-fontset-font (frame-parameter nil 'font) 'japanese-jisx0212 (font-spec :family "hiragino maru gothic pron") nil 'append) 
;; (add-to-list 'face-font-rescale-alist '("^-apple-hiragino_.*" . 1.1))

(set-default-font "Source Han Code JP N")


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

;; http://sakito.jp/emacs/emacsshell.html#id7
(cond ((or (eq window-system 'mac) (eq window-system 'ns))
       ;; mac os x の hfs+ ファイルフォーマットではファイル名は nfd (の様な物)で扱うため以下の設定をする必要がある
       (require 'ucs-normalize)
       (setq file-name-coding-system 'utf-8-hfs)
       (setq locale-coding-system 'utf-8-hfs))
      (or (eq system-type 'cygwin) (eq system-type 'windows-nt)
          (setq file-name-coding-system 'utf-8)
          (setq locale-coding-system 'utf-8)
          ;; もしコマンドプロンプトを利用するなら sjis にする
          ;; (setq file-name-coding-system 'sjis)
          ;; (setq locale-coding-system 'sjis)
          ;; 古い cygwin だと euc-jp にする
          ;; (setq file-name-coding-system 'euc-jp)
          ;; (setq locale-coding-system 'euc-jp)
          )
      (t
       (setq file-name-coding-system 'utf-8)
       (setq locale-coding-system 'utf-8)))

;;
;; for emacs mac port
;;
;; Emacs Mac Port 用設定
;; ミニバッファで入力する際に自動的にASCIIにする
(when (fboundp 'mac-auto-ascii-mode)
  (mac-auto-ascii-mode 1))

