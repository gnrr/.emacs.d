;; https://y-mattu.hatenablog.com/entry/2019/05/19/171734

(add-to-list 'auto-mode-alist '("\\.lsp$" . lisp-mode))
(add-to-list 'auto-mode-alist '("\\.lisp$" . lisp-mode))
(load (expand-file-name "~/.roswell/helper.el"))

;; SLIMEのロード
;; 以下3行は(load (expand-file-name "~/.roswell/helper.el"))の時点で設定される
;; (require 'slime)
;; (require 'slime-autoloads)
;; (slime-setup '(slime-repl slime-fancy slime-banner))
;; SLIMEからの入力をUTF-8に設定
(setq slime-net-coding-system 'utf-8-unix)

(eval-after-load "slime"
   '(slime-setup '(slime-fancy slime-banner)))
(global-set-key "\C-cs" 'slime-selector)

;; M-x my-slime: 分割したウィンドウでslime起動
;; C-c C-r: 選択範囲をslime-replへ送って評価
(defun my-slime (&optional command coding-system)
  "Run slime and split window."
  (interactive)
  (if (< (count-windows) 2)
      (split-window-vertically)
  )
  (slime command coding-system)
  (other-window 1)
  )
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
          (t (message "Not exist *slime-repl sbcl* buffer!")))
    ))
(global-set-key "\C-c\C-r" 'slime-repl-send-region)

;; LISPモードで新しくファイルを開いたらウィンドウが上下に分割して下にREPL
(add-hook 'lisp-mode-hook
           (lambda ()
             (global-set-key "\C-cH" 'hyperspec-lookup)
             (cond ((not (featurep 'slime))
                    (require 'slime)
                    (normal-mode)))
             (my-slime)))
