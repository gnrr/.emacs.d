;;; -*- coding:utf-8; mode:emacs-lisp -*-
;;;
;;; init.el
;;;
(message "--> loading \"init.el\"...")

;; to hide message "ad-handle-definition: ‘vc-revert’ got redefined"
(setq ad-redefinition-action 'accept)

;; ----------------------------------------------------------------------
(defun mycolor (name)
  (let ((colors '((white       . "#f9f9f9")
                  (light-gray  . "#a4a2a2")
                  (gray        . "#7c7a7a")
                  (dark-gray   . "#555555")
                  (black       . "#000000")
                  (red         . "#ff6b7f")
                  (blue        . "#61afef")
                  (dark-blue2  . "#1684DF")
                  (dark-blue   . "#126EBA")
                  (green       . "#98be65")
                  (pink        . "#eb7bc0")
                  (purple      . "#c678dd")
                  (orange      . "#e3b23c")
                  ;; (charcoal . "#3d363e"))))
                  ;; (charcoal    . "#362f37"))))
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
                  (ui3      . "Squarea")))
         (name (cdr (assoc type fonts))))
    (if window-system
        (if (x-list-fonts name)
            name
          (progn
            (message (format "ERROR: Font not found: %s" name))
            nil))
      (progn
        (message "ERROR: Specifying font can only work under any window-system.")
        nil))))

;; e.g. (myfont 'default) => "Source Han Code JP N"

;; ----------------------------------------------------------------------
;; defaults
(setq-default inhibit-startup-screen t)           ; Disable start-up screen

(add-hook 'emacs-startup-hook (lambda ()
 (message "--> startup-hook")

 (setq-default
  auto-window-vscroll nil                          ; Lighten vertical scroll
  confirm-kill-emacs 'yes-or-no-p                  ; Confirm before exiting Emacs
  delete-by-moving-to-trash t                      ; Delete files to trash
  display-time-default-load-average nil            ; Don't display load average
  display-time-format "%H:%M"                      ; Format the time string
  fill-column 80                                   ; Set width for automatic line breaks
  indent-tabs-mode nil                             ; Stop using tabs to indent
  initial-scratch-message ""                       ; Empty the initial *scratch* buffer
  left-margin-width 1 right-margin-width 1         ; Add left and right margins
  select-enable-clipboard t                        ; Merge system's and Emacs' clipboard
  sentence-end-double-space nil                    ; End a sentence after a dot and a space
  show-trailing-whitespace nil                     ; Display trailing whitespaces
  uniquify-buffer-name-style 'forward              ; Uniquify buffer names
  window-combination-resize t                      ; Resize windows proportionally

  bidi-display-reordering nil                      ; 右から左に読む言語に対応させないことで描画を高速化
  vc-follow-symlinks t
  ring-bell-function 'ignore
  parens-require-spaces nil
  transient-mark-mode nil
  initial-scratch-message ""
  indent-tabs-mode nil
  tab-width 4
  tab-stop-list  '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60)
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
  auto-save-file-name-transforms
  '(("~/\\([^/]*/\\)*\\([^/]*\\)$" "~/.Trash/\\2" t))
                                        ;             '((".*" "~/.Trash" t))

  ;; auto-save-default nil
  auto-save-timeout 10     ;; 保存の間隔 秒   (デフォルト : 30)
  auto-save-interval 100   ;;         打鍵  (デフォルト : 300)

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
 (defvar my-mode-line-position-format "%%3c %%4l/%d")
 (setq mode-line-position '(:eval (format my-mode-line-position-format
                                          (count-lines (point-max) (point-min)))))

 ;; タイトルバーにファイルのフルパス表示
 (defmacro replace-home-directory-string (file-name)
   `(if ,file-name
        (let ((regexp (cond ((eq system-type 'windows-nt) "^C:\\Users\\[^\\]+\\")
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

 (set-face-background 'region (mycolor 'dark-blue))

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
 (global-unset-key [f11])                                ; toggle-frame-fullscreen
 (global-unset-key [f12])                                ; "M-c"

 ;; (global-set-key "(" 'my-insert-paren)                   ; ()
 (global-set-key "{" 'my-insert-brace)                   ; {}
 (global-set-key "[" 'my-insert-bracket)                 ; []
 ;; (global-set-key "<" 'my-insert-angle)                   ; <>
 (global-set-key "'" 'my-insert-squote)                  ; ''
 (global-set-key "\"" 'my-insert-dquote)                 ; ""

 (global-set-key (kbd "C-m") 'newline-and-indent)             ; Returnキーで改行＋オートインデント
 (global-set-key (kbd "C-0") 'delete-window)
 (global-set-key (kbd "C-1") 'delete-other-windows)
 (global-set-key (kbd "C-2") 'split-window-below)
 (global-set-key (kbd "C-3") 'split-window-right)
 (global-set-key (kbd "C-o") 'other-window)

 (global-set-key (kbd "M-9") 'insert-parentheses)
 (global-set-key (kbd "M-g") 'goto-line)
 (global-set-key (kbd "M-v") 'new-empty-buffer-other-frame)
 (global-set-key (kbd "M-P") 'beginning-of-buffer)
 (global-set-key (kbd "M-N") 'end-of-buffer)

 (global-set-key (kbd "C-x t") 'revert-buffer)
 (global-set-key (kbd "C-x C-t") 'toggle-truncate-lines)
 (global-set-key (kbd "C-x n f") 'narrow-to-defun)

 (define-key undo-tree-map (kbd "C-?") 'nil)
 (define-key undo-tree-map (kbd "C-r") 'nil)    ;; undo-tree-redo      FIXME: not work
 (define-key isearch-mode-map (kbd "C-b") 'isearch-delete-char)

;; ----------------------------------------------------------------------
 (defun my-func ()
   "called \'my-func\'")

 (global-set-key [f2] '(lambda () (interactive) (message "%S" (funcall 'my-func))))

;; ----------------------------------------------------------------------
;; which-func-mode
 (setq which-func-unknown "-"
       ;; which-func-modes '(emacs-lisp-mode lisp-interaction-mode c-mode python-mode ruby-mode)
       which-func-format '(:propertize which-func-current face which-func))

 (which-function-mode 1)        ;; global

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

 ;; for elisp
 (defalias 'ev 'eval-defun)
 (eval-after-load "edebug"
   '(defalias 'ede 'edebug-defun))

 ;; apropos
 (when (featurep 'counsel)
   (defalias 'a 'counsel-apropos))

 (defalias 'l 'display-line-numbers-mode)
 (defalias 'hl 'hl-line-mode)
 (defalias 'com 'comment-or-uncomment-region)
 (defalias 'ind 'indent-region)
 (defalias 'calc 'quick-calc)


 ;; ----------------------------------------------------------------------
 ;; my-elisp
 (require 'discrete)
 (require 'my-backup)
 (setq my-backup-directory "~/bak")

 ;; ----------------------------------------------------------------------
 ;; computer independent
 (load
  (cond ((eq system-type 'windows-nt) "~/.emacs.d/elisp/_windows.el")
        ((eq system-type 'gnu/linux)  "~/.emacs.d/elisp/_linux.el")
        (t                            "~/.emacs.d/elisp/_mac.el"))
  nil t)

 ;; ----------------------------------------------------------------------
 (defvar exclude-face-list '(mode-line-buffer-id
                             mode-line-emphasis
                             mode-line-highlight
                             mode-line-inactive
                             mode-line))

 (my-font-lighter (remove-if (lambda (x) (member x exclude-face-list)) (face-list)))

 ;; (zerodark-setup-modeline-format)
 (my-load-frame)

 (message "<-- startup-hook")

 ;; startup message in mini-buffer
 (message "%s / %s" (replace-regexp-in-string "(.+)\\|of\\|[\n]" "" (emacs-version)) (emacs-init-time))

 (lisp-interaction-mode)                            ;; workaround for scratch-log
)) ;; emacs-startup-hook function ends here

;; ======================================================================
;; auto-insert
(add-hook 'find-file-hook 'auto-insert)
(setq auto-insert-directory "~/.emacs.d/templates")
(defvar auto-insert-alist nil)
(setq auto-insert-alist (append '(("\\.mq4" . "mq4"))
                                auto-insert-alist))

;; ----------------------------------------------------------------------
;; diminish
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

;; major mode
(diminish-major-mode 'emacs-lisp-mode-hook "Elisp")
(diminish-major-mode 'lisp-interaction-mode-hook "LispInt")

;; ----------------------------------------------------------------------
;; utility for use-package
(defun my-font-exists-p ($font-name)
  (if (null (x-list-fonts $font-name))
      nil t))

;; ----------------------------------------------------------------------
(use-package cl)

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

  ;; motion-state-map
  (define-key evil-motion-state-map (kbd "!") #'nop)                            ; unmap
  (define-key evil-motion-state-map (kbd "@") #'nop)                            ; unmap
  (define-key evil-motion-state-map (kbd "#") #'nop)                            ; unmap
  (define-key evil-motion-state-map (kbd "$") #'nop)                            ; unmap
  (define-key evil-motion-state-map (kbd "%") #'nop)                            ; unmap
  (define-key evil-motion-state-map (kbd "^") #'nop)                            ; unmap
  (define-key evil-motion-state-map (kbd "&") #'nop)                            ; unmap
  (define-key evil-motion-state-map (kbd "*") #'nop)                            ; unmap
  (define-key evil-motion-state-map (kbd "(") #'nop)                            ; unmap
  (define-key evil-motion-state-map (kbd ")") #'nop)                            ; unmap
  (define-key evil-motion-state-map (kbd "3") #'evil-search-word-backward)      ; works as #
  (define-key evil-motion-state-map (kbd "8") #'evil-search-word-forward)       ; works as *

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
  (define-key evil-motion-state-map (kbd "6") #'evil-first-non-blank)
  (define-key evil-motion-state-map (kbd "4") #'evil-end-of-line)
  (define-key evil-motion-state-map (kbd "]") #'evil-jump-item)
  (define-key evil-motion-state-map (kbd "M-w") #'my-forward-word)
  (define-key evil-motion-state-map (kbd ":") #'nop)        ; unmap :
  (define-key evil-motion-state-map (kbd ";") #'evil-ex)    ; works as :

  ;; normal-state-map
  (define-key evil-normal-state-map (kbd "q") nil)
  (define-key evil-normal-state-map (kbd "m") nil)
  (define-key evil-normal-state-map (kbd "M-.") nil)        ; evil-repeat-pop-next
  (define-key evil-normal-state-map (kbd "C-p") nil)        ; evil-paste-pop
  (define-key evil-normal-state-map (kbd "M-j") nil)        ; outline-move-sutree-*
  (define-key evil-normal-state-map (kbd "M-k") nil)        ; outline-move-sutree-*
  (define-key evil-normal-state-map (kbd "TAB") #'evil-indent-line)
  (define-key evil-normal-state-map (kbd "U") #'undo-tree-redo)
  (define-key evil-normal-state-map (kbd "M-p") #'evil-paste-pop-next)
  (define-key evil-normal-state-map (kbd "SPC") #'evil-force-normal-state)

  ;; insert-state-map
  (define-key evil-insert-state-map (kbd "C-h") #'delete-backward-char)
  (define-key evil-insert-state-map (kbd "M-h") #'my-backward-kill-word)
  (define-key evil-insert-state-map (kbd "TAB") #'(lambda () (interactive) (insert-tab)))

  (defun evil-return-insert-mode-after-save ()
    (when evil-insert-state-minor-mode
      (funcall (evil-escape--escape-normal-state))))

  (add-hook 'after-save-hook #'evil-return-insert-mode-after-save)

  (defun my-evil-paste (&optional arg)
    (interactive  "P")
    (if (memq last-command '(evil-paste-before evil-paste-after))
        (call-interactively #'evil-paste-pop)
      (call-interactively (if arg #'evil-paste-before #'evil-paste-after))))
;;  (define-key evil-normal-state-map (kbd "p") #'my-evil-paste)

  (defun my-evil-search-dummy-func ()
    (remove-hook 'isearch-mode-hook #'my-evil-search-dummy-func)
    (setq unread-command-events (listify-key-sequence (kbd "RET"))))

  (defun my-evil-search-dummy ()
    "workaround for `my-evil-search-from-region-next'. swapping search direction is prevent by calling this function prior 'my-evil-search-from-region-next'."
    (add-hook 'isearch-mode-hook #'my-evil-search-dummy-func)
    (call-interactively 'evil-search-forward))

  (defvar my-evil-search-first-time t)

  (defun my-evil-search-from-region-next ()
    "under the evil-visual-state, jump next immediately after selecting region and pressing specified key."
    (interactive)
    (when my-evil-search-first-time
      (my-evil-search-dummy)
      (setq my-evil-search-first-time nil))
    (my-evil-search-from-region-1 t))

  (defun my-evil-search-from-region-prev ()
    "under the evil-visual-state, jump previous immediately after selecting region and pressing specified key."
    (interactive)
    (when my-evil-search-first-time
      (my-evil-search-dummy)
      (setq my-evil-search-first-time nil))
    (my-evil-search-from-region-1 nil))

  (defun my-evil-search-from-region-1 (forward)
    "pull string from region as search string then jump"
    (if (use-region-p)
        (let ((beg (region-beginning))
              (end (region-end)))
          (when (< beg end)
            (let ((s (buffer-substring-no-properties beg end)))
              (delete-duplicates (push s (if evil-regexp-search regexp-search-ring search-ring))
                                 :test 'string= :from-end t)
              (evil-normal-state nil)
              (evil-search s forward))))))

  (define-key evil-visual-state-map (kbd "n") #'my-evil-search-from-region-next)
  (define-key evil-visual-state-map (kbd "N") #'my-evil-search-from-region-prev)

  (defvar my-evil-visual-surround-paired '((?\" . ?\") (?\' . ?\') (?\( . ?\)) (?\{ . ?\}) (?\[ . ?\]) (?\< . ?>)))
  (defun my-evil-visual-surround-add (beg end)
    (let* ((c (read-char "?"))
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

  (defun my-evil-visual-surround-remove (beg end)
    (save-excursion
      (goto-char (1- end))
      (delete-char 1)
      (goto-char beg)
      (delete-char 1)))

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
             (my-evil-visual-surround-remove beg end))
            (t (my-evil-visual-surround-add beg end)))))

  (define-key evil-visual-state-map "s" 'my-evil-visual-surround)

  (defun my-gg ()
    (interactive)
    (if (eq last-command this-command)
        (exchange-point-and-mark t)
      (push-mark (point) t)
      (goto-char (point-min))))

  (define-key evil-motion-state-map (kbd "gg") #'my-gg)

  ;; (defvar my-evil-visual-state-symbol-overlay-p nil)
  ;; (defun my-evil-visual-state-entry-hook-func ()
  ;;   (setq my-evil-visual-state-symbol-overlay-p symbol-overlay-mode)
  ;;   (symbol-overlay-remove-all)
  ;;   (symbol-overlay-mode nil))
  ;; (defun my-evil-visual-state-exit-hook-func ()
  ;;   (symbol-overlay-mode my-evil-visual-state-symbol-overlay-p))

  ;; (defvar my-evil-visual-state-symbol-overlay-p nil)
  ;; (defun my-expand-region ()
  ;;   (interactive)
  ;;   (setq my-evil-visual-state-symbol-overlay-p symbol-overlay-mode)
  ;;   (symbol-overlay-remove-all)
  ;;   (symbol-overlay-mode nil)
  ;;   (call-interactively 'er/expand-region))

  ;; (defun my-contract-region ()
  ;;   (interactive)
  ;;   (symbol-overlay-mode my-evil-visual-state-symbol-overlay-p)
  ;;   (call-interactively 'er/contract-region))

  ;; (add-hook 'evil-visual-state-entry-hook #'my-evil-visual-state-entry-hook-func)
  ;; (add-hook 'evil-visual-state-exit-hook #'my-evil-visual-state-exit-hook-func)
;  :hook ((evil-visual-state-entry . my-evil-visual-state-entry-hook-func)
;         (evil-visual-state-exit . my-evil-visual-state-exit-hook-func))

 ;; :bind (:map evil-normal-state-map
 ;;        ("=" . my-expand-region)
 ;;        ("-" . my-contract-region)
 ;;        :map evil-visual-state-map
 ;;        ("=" . my-expand-region)
 ;;        ("-" . my-contract-region))
  :config
  ;; package-mode
 (evil-add-hjkl-bindings package-menu-mode-map 'emacs
   (kbd "/")       'evil-search-forward
   (kbd "n")       'evil-search-next
   (kbd "N")       'evil-search-previous)

)

;; ----------------------------------------------------------------------
(use-package evil-collection
  ;; :disabled
  :after evil
  :config
  (evil-collection-init '(edebug dired neotree slime help))
  )

;; ----------------------------------------------------------------------
(use-package evil-escape
  :after evil
  :diminish evil-escape-mode
  :config
  (evil-escape-mode 1)
  (setq-default evil-escape-delay 0.2)
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
(use-package evil-org
  :disabled
  :after evil
  ;; :diminish evil-org-mode
  :config
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
(use-package atom-one-dark-theme
  :disabled
  :if window-system
  :config
  (load-theme 'atom-one-dark t)
  )

;; ----------------------------------------------------------------------
(use-package zerodark-theme
  ;; :disabled
  :load-path "~/.emacs.d/elisp/zerodark-theme"
  :if window-system
  :config
  (setq zerodark-use-paddings-in-mode-line nil)
  (load-theme 'zerodark t)

  ;; mod
  (setq zerodark-modeline-vc '(vc-mode (""
     (:eval (all-the-icons-faicon "code-fork"
                                  ;; :height 0.9
                                  :v-adjust 0
                                  :face (when (zerodark--active-window-p)
                                          (zerodark-git-face))))
     (:eval (when (eq zerodark-theme-display-vc-status 'full)
              ;; (propertize (truncate-string-to-width vc-mode 25 nil nil "...")
              (propertize (truncate-string-to-width (replace-regexp-in-string "^.+:" " " vc-mode) 25 nil nil "...")
                          'face (when (zerodark--active-window-p)
                                  (zerodark-git-face))))))))

  (set-face-attribute 'cursor nil
                      :background (mycolor 'blue)
                      :foreground "#000000"
                      :weight 'bold)

  (set-face-attribute 'font-lock-builtin-face nil :weight 'light)
  (set-face-attribute 'font-lock-comment-face nil :weight 'light)
  (set-face-attribute 'font-lock-constant-face nil :weight 'light)
  (set-face-attribute 'font-lock-function-name-face nil :weight 'light)
  (set-face-attribute 'font-lock-keyword-face nil :weight 'light)
  (set-face-attribute 'font-lock-string-face nil :weight 'light)
  (set-face-attribute 'font-lock-doc-face nil :weight 'light)
  (set-face-attribute 'font-lock-type-face nil :weight 'light)
  ;; (set-face-attribute 'fant-lock-variable-name-face nil :weight 'light)
  (set-face-attribute 'font-lock-warning-face nil :weight 'light)

  (set-face-attribute 'minibuffer-prompt  nil :slant 'italic :height 1.1 :foreground (mycolor 'blue))

  (set-face-attribute 'line-number              nil :height 1.1 :slant 'italic :background "#2B2F38" :foreground "#5B6475")
  (set-face-attribute 'line-number-current-line nil :height 1.1 :slant 'italic :background "#2B2F38")

  (set-face-background 'default "#21252B")
  (set-face-attribute 'fringe nil :foreground (face-attribute 'line-number :foreground)
                      	          :background (face-attribute 'line-number :background))

  (let ((font (myfont 'ui)))
    (when font
      (set-face-attribute 'mode-line          nil :family font)
      (set-face-attribute 'mode-line-inactive nil :family font)
      (set-face-attribute 'minibuffer-prompt  nil :family font)

      (set-face-attribute 'line-number              nil :family font)
      (set-face-attribute 'line-number-current-line nil :family font)))

  )

;; ----------------------------------------------------------------------
(use-package doom-themes
  :disabled
  :config
  ;; Global settings (defaults)
  (setq doom-themes-enable-bold nil    ; if nil, bold is universally disabled
        doom-themes-enable-italic nil) ; if nil, italics is universally disabled
  (load-theme 'doom-one t)

  ;; Enable flashing mode-line on errors
  (doom-themes-visual-bell-config)
  
  ;; Enable custom neotree theme (all-the-icons must be installed!)
  (doom-themes-neotree-config)
  ;; or for treemacs users
  (setq doom-themes-treemacs-theme "doom-colors") ; use the colorful treemacs theme
  (doom-themes-treemacs-config)
  
  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config)
  )

;; ----------------------------------------------------------------------
(use-package doom-modeline
  ;; :disabled
  :ensure t
  :after evil
  :hook
  (after-init . doom-modeline-mode)

  :custom
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-height 18)

  :config
  (set-face-attribute 'doom-modeline-project-dir nil :foreground (mycolor 'blue) :weight 'normal)
  (set-face-attribute 'doom-modeline-buffer-file nil :foreground (mycolor 'blue) :weight 'bold)

  (let ((bg (face-background 'mode-line)))
    (setq evil-normal-state-tag   (propertize " NORMAL " 'face `((:background ,(mycolor 'blue)    :foreground ,(face-background 'mode-line) :weight bold)))
          evil-emacs-state-tag    (propertize " EMACS  " 'face `((:background ,(mycolor 'orange)  :foreground ,(face-background 'mode-line) :weight bold)))
          evil-insert-state-tag   (propertize " INSERT " 'face `((:background ,(mycolor 'red)     :foreground ,(face-background 'mode-line) :weight bold)))
          evil-motion-state-tag   (propertize " MOTION " 'face `((:background ,(mycolor 'purple)  :foreground ,(face-background 'mode-line) :weight bold)))
          evil-visual-state-tag   (propertize " VISUAL " 'face `((:background ,(mycolor 'green)   :foreground ,(face-background 'mode-line) :weight bold)))
          evil-operator-state-tag (propertize " OPERATOR " 'face `((:background ,(mycolor 'pink)    :foreground ,(face-background 'mode-line) :weight bold)))))

  (doom-modeline-def-segment evil-state
    "The current evil state.  Requires `evil-mode' to be enabled."
    (when (bound-and-true-p evil-local-mode)
      ;; (s-trim-right (evil-state-property evil-state :tag t))))
      (evil-state-property evil-state :tag t)))

  (doom-modeline-def-segment linum-colnum
    "Display current linum/colnum"
    (propertize (format " %4s/%s,%-3s"
                        (format-mode-line "%l")
                        (line-number-at-pos (point-max))
                        (format-mode-line "%c"))
                'face '(:foreground (face-foreground mode-line) :weight bold)))

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
             (when (or (buffer-narrowed-p)
                       (and (bound-and-true-p fancy-narrow-mode)
                            (fancy-narrow-active-p))
                       (bound-and-true-p dired-narrow-mode))
               (doom-modeline-buffer-file-state-icon
                "vertical_align_center" "↕" "><" 'doom-modeline-warning)))))))

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
(use-package telephone-line
  :disabled
  :after evil zerodark-theme all-the-icons
  :if window-system
  :config

  ;; (set-face-attribute 'telephone-line-accent-active nil
                      ;; :background "#7e7e7e" :foreground "#f9f9f9")
  ;; (set-face-attribute 'telephone-line-accent-inactive nil
                      ;; :background "#4e4e4e" :foreground (face-attribute 'mode-line-inactive :foreground))

  ;; (defface telephone-line-accent2-active
  ;;   ;; '((t (:background "#5e5e5e" :inherit telephone-line-accent-active))) "")
  ;;   '((t (:inherit telephone-line-accent-active))) "")

  ;; (defface telephone-line-accent2-inactive
  ;;   ;; '((t (:background "#3a3a3a" :inherit telephone-line-accent-inactive))) "")
  ;;   '((t (:inherit telephone-line-accent-inactive))) "")

  ;; (add-to-list 'telephone-line-faces
  ;;              '(accent2 . (telephone-line-accent2-active . telephone-line-accent2-inactive)))

;   (setq telephone-line-lhs
;         '((evil   . (telephone-line-evil-tag-segment))
;           (accent . (telephone-line-vc-segment
;                      telephone-line-process-segment))
;           (accent2 . (telephone-line-buffer-info-segment))
;           (nil    . (telephone-line-buffer-segment))))
  (setq telephone-line-lhs
        '((evil . (telephone-line-evil-tag-segment))
          (nil  . (telephone-line-vc-segment))
          ;; (nil  . (telephone-line-buffer-info-segment))
          (nil  . (telephone-line-buffer-segment
                     telephone-line-process-segment))))

  (setq telephone-line-rhs
        '((nil . (telephone-line-position-segment))
          (nil . (telephone-line-mode-segment))))
          ;; (nil . (telephone-line-misc-info-segment
          ;;         ;; telephone-line-minor-mode-segment
          ;;         telephone-line-mode-segment))))

  (telephone-line-defsegment* telephone-line-vc-segment ()
    `(""
      ,zerodark-modeline-vc
      " "
      ))

  (telephone-line-defsegment* telephone-line-mode-segment ()
    `("|"
      minor-mode-alist
      " | "
      mode-name
      " |"))
  (telephone-line-defsegment* telephone-line-buffer-segment ()
    `(,mode-line-mule-info
      ;; mode-line-modified
      ;; mode-line-client
      ;; mode-line-remote
      ,zerodark-modeline-ro
      ,zerodark-modeline-modified
      " "
      ,(telephone-line-raw mode-line-frame-identification t)
      ""
      ,(telephone-line-raw mode-line-buffer-identification t)))

  (telephone-line-defsegment* telephone-line-position-segment ()
    ;; (telephone-line-raw-mod
    (telephone-line-raw
     (if (eq major-mode 'paradox-menu-mode)
         ;;Paradox fills this with position info.
         mode-line-front-space
       mode-line-position) t))

  ;; (setq telephone-line-primary-left-separator 'telephone-line-identity-left
  ;;       telephone-line-secondary-left-separator 'telephone-line-identity-hollow-left
  ;;       telephone-line-primary-right-separator 'telephone-line-identity-left
  ;;       telephone-line-secondary-right-separator 'telephone-line-identity-hollow-left)

  (setq telephone-line-primary-left-separator 'telephone-line-flat)
        ;; telephone-line-secondary-left-separator 'telephone-line-flat
        ;; telephone-line-primary-right-separator 'telephone-line-flat
        ;; telephone-line-secondary-right-separator 'telephone-line-flat)

  (setq telephone-line-primary-right-separator 'telephone-line-flat)
        ;; telephone-line-secondary-left-separator 'telephone-line-flat
        ;; telephone-line-primary-right-separator 'telephone-line-flat
        ;; telephone-line-secondary-right-separator 'telephone-line-flat)

  (setq telephone-line-height 16
        telephone-line-evil-use-short-tag nil)

  (telephone-line-mode 1)

  (set-face-background 'telephone-line-evil-insert   (mycolor 'red))
  (set-face-background 'telephone-line-evil-normal   (mycolor 'blue))
  (set-face-background 'telephone-line-evil-visual   (mycolor 'green))
  (set-face-background 'telephone-line-evil-operator (mycolor 'pink))
  (set-face-background 'telephone-line-evil-motion   (mycolor 'purple))
  (set-face-background 'telephone-line-evil-replace  (mycolor 'gray))
  (set-face-background 'telephone-line-evil-emacs    (mycolor 'orange))

  (dolist (f '(telephone-line-evil-insert
               telephone-line-evil-normal
               telephone-line-evil-visual
               telephone-line-evil-operator
               telephone-line-evil-motion
               telephone-line-evil-replace
               telephone-line-evil-emacs))
    (set-face-foreground f "#061826"))

  (defun telephone-line-raw-mod (str &optional preformatted)
    "Conditionally render STR as mode-line data, or just verify output if not PREFORMATTED.
Return nil for blank/empty strings."
    (let ((fmt (format-mode-line str)))
      (unless (seq-empty-p fmt)
        (if preformatted
                                        ; format-mode-line will condense all escaped %s, so we need
                                        ; to re-escape them.
            (replace-regexp-in-string "%" "%%" fmt)
          str))))

  ;; mod
  (defun telephone-line-raw (str &optional preformatted)
    "Conditionally render STR as mode-line data, or just verify output if not PREFORMATTED.
Return nil for blank/empty strings."
    (let ((trimmed-str (string-trim (format-mode-line str))))
      (unless (seq-empty-p trimmed-str)
        (if preformatted
                                        ; format-mode-line will condense all escaped %s, so we need
                                        ; to re-escape them.
            (replace-regexp-in-string "%" "%%" trimmed-str)
          str))))

  )

;; ----------------------------------------------------------------------
(use-package hide-mode-line
  :hook
  ((neotree-mode) . hide-mode-line-mode)
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
        counsel-find-file-ignore-regexp "\\.elc\\'"
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

  ;; (copy-face 'ivy-highlight-face 'ivy-minibuffer-match-highlight)
  (set-face-attribute 'ivy-minibuffer-match-highlight nil :foreground nil :background nil)  ; disable

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
    "counsel-at-point in specified directory"
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
                                            " -i --no-heading --line-number --color never %s ."))
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

  ;; re-defun from counsel.el
  ;; Usage: C-x C-f M-x m
  (defun counsel-find-file-move (x)
    "Move or rename file X."
    (let* ((name (if (and ivy--directory (string-match "/$" (ivy-state-current ivy-last)))
                     (substring (ivy-state-current ivy-last) 0 -1)
                   (ivy-state-current ivy-last)))
           (new-name (read-no-blanks-input "Rename to:" name)))
      (require 'dired-aux)
      (dired-rename-file name new-name 1)))

  :bind (("M-z"     . ivy-resume)
         ("M-r"     . counsel-recentf)
         ("M-o"     . my-counsel-rg)
         ("C-x C-b" . counsel-ibuffer)
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
  ;; :disabled
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
  :after ivy
  :config
  (setq recentf-max-saved-items 5000) ;; 履歴保存の数
  ;; (setq recentf-auto-cleanup 'never)  ;; 存在しないファイルは消さない
  (setq recentf-exclude '("/recentf" ".recentf" ".my-save-frame"))
  ;; (setq recentf-auto-save-timer (run-with-idle-timer 30 t 'recentf-save-list))

  ;; (recentf-mode 1)
  ;; (global-set-key (kbd "M-r") 'counsel-recentf)
  ;; (global-set-key (kbd "M-r") 'helm-recentf)
  ;; (global-set-key "\M-r" 'my/ido-recentf)
  )

;; ----------------------------------------------------------------------
(use-package tabbar
  ;; :disabled
  :hook ((after-save   . tabbar-on-saving-buffer)
         (first-change . tabbar-on-modifying-buffer))
  :config
  (tabbar-mode)

  (set-face-attribute 'tabbar-default nil
                      :height 0.9
                      :background (face-background 'mode-line)
                      :slant 'normal
                      :box nil
                      :overline (face-background 'mode-line)
                      )

  (set-face-attribute 'tabbar-selected nil
                      :foreground (face-background 'mode-line)
                      :background (face-foreground 'line-number-current-line)
                      ;; :inherit 'tabbar-default
                      :slant 'normal
                      :box nil
                      :overline (face-foreground 'line-number-current-line)
                      )

  (set-face-attribute 'tabbar-unselected nil
                      ;; :background (face-attribute 'telephone-line-accent-active :background)
                      ;; :background "#aaaaaa"
                      :background (face-foreground 'tabbar-selected)
                      :foreground (face-background 'tabbar-selected)
                      ;; :inherit 'tabbar-default
                      :slant 'normal
                      :box nil
                      :overline (face-foreground 'tabbar-selected)
                      )

  (set-face-attribute 'tabbar-selected-modified nil
                      :background (face-background 'tabbar-selected)
                      :foreground (face-foreground 'tabbar-selected)
                      ;; :inherit 'tabbar-default
                      :slant 'normal
                      :box nil
                      :overline "orange"
                      )

  (set-face-attribute 'tabbar-modified nil
                      :background (face-attribute 'tabbar-unselected :background)
                      :foreground (face-attribute 'tabbar-unselected :foreground)
                      ;; :inherit 'tabbar-default
                      :slant 'normal
                      :box nil
                      :overline "orange"
                      )

  (set-face-attribute 'tabbar-separator nil
                      :background (face-attribute 'tabbar-selected :background))

  ;; (setq tabbar-separator '(0.2))

  (global-set-key (kbd "M-j") 'tabbar-backward-tab)
  (global-set-key (kbd "M-k") 'tabbar-forward-tab)

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

  (let ((font (myfont 'ui)))
    (when font
      (set-face-attribute 'tabbar-default nil :family font)))

  )

;; ----------------------------------------------------------------------
(use-package smartparens
  :diminish smartparens-mode
  :config
  (smartparens-global-mode)
  (show-smartparens-global-mode t)
  (setq sp-autoinsert-pair nil)
  (set-face-background 'sp-show-pair-match-face "#4C6DA6")

  (ad-disable-advice 'delete-backward-char 'before 'sp-delete-pair-advice) ; disable C-h
  (ad-activate 'delete-backward-char)

  ;; use show-paren to hilight content in parenthesis
  (setq show-paren-style 'expression)
  (set-face-background 'show-paren-match "#263652")
  (setq show-paren-delay 0.2)
  (show-paren-mode 1)

  ;; depends on modes
  (sp-with-modes '(lisp-mode lisp-interaction-mode)
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
  (set-face-bold 'rainbow-delimiters-depth-1-face t)
  )

;; ----------------------------------------------------------------------
(use-package rainbow-mode
  :diminish rainbow-mode
  :hook ((prog-mode . rainbow-mode))
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
  :after evil
  :load-path "elisp"
  :bind (:map evil-normal-state-map
              ("q SPC" . quick-back-mark)
              ("q q"   . quick-back-jump))
  )

;; ----------------------------------------------------------------------
(use-package hiwin
  ;; :disabled
  :ensure t
  :diminish hiwin-mode
  :config
  (set-face-background 'hiwin-face (mycolor 'charcoal))
  ;; (set-face-foreground 'hiwin-face (mycolor 'light-gray))
  (hiwin-mode t)
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
  (setq beacon-blink-delay 0.1)
  (beacon-mode t)
  )

;; ----------------------------------------------------------------------
(use-package org-bullets
  :config
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
  :mode (("\\.c$" . c-mode)
         ("\\.h$" . c-mode))
  :init
  (add-hook 'c-mode-hook
            (lambda ()
              (local-set-key "\C-m" 'reindent-then-newline-and-indent)
              (local-set-key "\C-i" 'indent-or-insert-tab)
              (local-set-key "(" 'my-insert-paren)
              (local-set-key "{" 'my-insert-brace)
              ;; (setq case-fold-search nil)                 ; case sensitive
              (c-set-style "stroustrup")
              (c-set-offset 'case-label 4)
              (modify-syntax-entry ?_ "w")                ; アンダーバーをワード区切りとしない
              (setq comment-start "//")                   ; コメントを // にする
              (setq comment-end "")
              ;; (setq compilation-read-command nil)         ; make のオプションの確認は不要
              (setq compilation-ask-about-save nil)       ; make するとき save する
              ;; (setq compile-command "make")               ; make時のデフォルトコマンド
              ;; (c-toggle-hungry-state 1)                   ; backspace時にカーソルの左の空白をすべて削除
              (cwarn-mode)
              ;; (which-function-mode 1)
              (display-line-numbers-mode)
              (setq compilation-scroll-output t)
              ;; (setq compile-command "cd ~/git-clone/qmk_firmware; make dichotemy:default")
              (setq compilation-auto-jump-to-first-error t)
              (setq compilation-window-height 10)
              ))
  :config
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


  ;; :after telephone-line
  ;; :config
  ;; ;; workaround for *compilation* buffer
  ;; (dolist (f '(compilation-info compilation-warning compilation-error))
  ;;   (set-face-background f (face-attribute 'telephone-line-accent-inactive :background)))

  )
 
;; ----------------------------------------------------------------------
(use-package arduino-mode
  :mode (("\\.pde$" . arduino-mode)
         ("\\.ino$" . arduino-mode))
  )

;; ----------------------------------------------------------------------
(use-package mql-mode
  :config
  (add-hook 'mql-mode-hook (lambda ()
                             (flycheck-mode -1)
                             (counsel-gtags-mode -1)
                             (symbol-overlay-mode t)
                             ))
  )

;; ----------------------------------------------------------------------
(use-package slime
  :disabled
  :init
  (load (expand-file-name "~/.roswell/helper.el"))

  :config
  (setq slime-net-coding-system 'utf-8-unix)
  (add-hook 'slime-load-hook (lambda () (require 'slime-fancy)))
  (slime-setup '(slime-fancy slime-banner))

  ;; LISPモードで新しくファイルを開いたらウィンドウが上下に分割して下にREPL
  (add-hook 'lisp-mode-hook
            (lambda ()
              (global-set-key "\C-cH" 'hyperspec-lookup)
              (cond ((not (featurep 'slime))
                     (require 'slime)
                     (normal-mode)))
              (my-slime)))

  ;; 分割したウィンドウでslime起動
  (defun my-slime (&optional command coding-system)
    "Run slime and split window."
    (interactive)
    (if (< (count-windows) 2)
        (split-window-vertically))
    (slime command coding-system)
    (other-window 1))

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

  :bind (:map lisp-mode-map
             ("M-r" . nil)
             ("C-x C-e" . slime-eval-last-expression-in-repl)
             ("C-c C-c" . slime-compile-and-load-file)
             ("C-c C-r" . slime-repl-send-region)
             ("C-c C-f" . slime-compile-defun))
  )

;; ----------------------------------------------------------------------
(message "<-- done    \"init.el\"")

;;
;; init.el ends here
;;
