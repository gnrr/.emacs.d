;;; -*- coding:utf-8; mode:emacs-lisp -*-
;;;
;;; init.el
;;;
(message "--> loading \"init.el\"...")

;; ----------------------------------------------------------------------
;; defaults
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

 )

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
(global-set-key "\C-m" 'newline-and-indent)             ; Returnキーで改行＋オートインデント

;; mode-line
(column-number-mode t)
(set-face-attribute 'mode-line          nil :box nil)   ; モードラインを非3D化
(set-face-attribute 'mode-line-inactive nil :box nil)

;; モードラインの割合表示を総行数表示に
(defvar my-mode-line-position-format "%%3c:%%4l/%d")
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

;; ----------------------------------------------------------------------
;; utility
(defun my-font-exists-p ($font-name)
  (if (null (x-list-fonts $font-name))
      nil t))

(defun my-get-cursor-color ()
  (car (cl-loop for ($k . $v) in (frame-parameters)
                if (eq $k 'cursor-color)
                collect $v)))

;; ----------------------------------------------------------------------
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

(global-set-key "(" 'my-insert-paren)                   ; ()
(global-set-key "{" 'my-insert-brace)                   ; {} 
(global-set-key "[" 'my-insert-bracket)                 ; []
(global-set-key "<" 'my-insert-angle)                   ; <>
(global-set-key "\"" 'my-insert-dquote)                 ; ""
(global-set-key "'" 'my-insert-squote)                  ; ''

(global-set-key (kbd "C-0") 'delete-window)
(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-2") 'split-window-below)
(global-set-key (kbd "C-3") 'split-window-right)
(global-set-key (kbd "C-o") 'other-window)

(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-v") 'new-empty-buffer-other-frame)
(global-set-key (kbd "C-x t") 'revert-buffer)
(global-set-key (kbd "C-x C-t") 'toggle-truncate-lines)
(global-set-key (kbd "C-x n f") 'narrow-to-defun)

(global-set-key (kbd "M-9") 'insert-parentheses)
(global-set-key (kbd "M-P") 'beginning-of-buffer)
(global-set-key (kbd "M-N") 'end-of-buffer)

(define-key isearch-mode-map (kbd "C-b") 'isearch-delete-char)

(defun my-func ()
  "called \'my-func\'")

(global-set-key [f2] '(lambda () (interactive) (message "%S" (funcall 'my-func))))

;; ----------------------------------------------------------------------
(use-package cl)

;; ----------------------------------------------------------------------
(use-package evil
  :init
  (setq evil-want-integration nil)

  :config
  (evil-mode 1)
  (evil-set-initial-state 'help-mode 'emacs)

  (defalias #'forward-evil-word #'forward-evil-symbol)

  (evil-ex-define-cmd "q[uit]" 'kill-this-buffer)

  ;; インサートモードではEmacsキーバインド
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map [escape] 'evil-normal-state)

  ;; motion-state-map
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

  (define-key evil-motion-state-map (kbd "m") 'evil-scroll-page-down)
  (define-key evil-motion-state-map (kbd "M") 'evil-scroll-page-up)
  (define-key evil-motion-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "4") 'evil-end-of-line)
  (define-key evil-motion-state-map (kbd "]") 'evil-jump-item)

  ;; normal-state-map
  (define-key evil-normal-state-map (kbd "m") nil)
  (define-key evil-normal-state-map (kbd "M-.") nil)        ; evil-repeat-pop-next
  (define-key evil-normal-state-map (kbd "C-p") nil)        ; evil-paste-pop
  (define-key evil-normal-state-map (kbd "M-j") nil)        ; outline-move-sutree-*
  (define-key evil-normal-state-map (kbd "M-k") nil)        ; outline-move-sutree-*
  (define-key evil-normal-state-map (kbd "TAB") 'evil-indent-line)
  (define-key evil-normal-state-map (kbd "U") 'undo-tree-redo)
  (define-key evil-normal-state-map (kbd "M-p") 'evil-paste-pop-next)

  ;; insert-state-map
  (define-key evil-insert-state-map (kbd "C-h") 'delete-backward-char)
  (define-key evil-insert-state-map (kbd "M-h") 'my-backward-kill-word)
  (define-key evil-insert-state-map (kbd "TAB") '(lambda () (interactive) (insert-tab)))

  (defun evil-return-insert-mode-after-save ()
    (when evil-insert-state-minor-mode
      (funcall (evil-escape--escape-normal-state))))

  (add-hook 'after-save-hook 'evil-return-insert-mode-after-save)

  (defun my-evil-paste (&optional arg)
    (interactive  "P")
    (if (memq last-command '(evil-paste-before evil-paste-after))
        (call-interactively 'evil-paste-pop)
      (call-interactively (if arg 'evil-paste-before 'evil-paste-after))))
;;  (define-key evil-normal-state-map (kbd "p") 'my-evil-paste)


  (defun my-gg ()
    (interactive)
    (if (eq last-command this-command)
        (exchange-point-and-mark t)
      (push-mark (point) t)
      (goto-char (point-min))))

  (define-key evil-motion-state-map (kbd "gg") 'my-gg)

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
  (evil-collection-init '(edebug dired neotree))
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
(use-package evil-surround
  :after evil
  ;; :diminish evil-surround-mode
  :config
  (global-evil-surround-mode 1)
)

;; ----------------------------------------------------------------------
(use-package evil-lion
  :after evil
  ;; :diminish evil-surround-mode
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
(use-package atom-one-dark-theme
  :disabled
  :if window-system
  :config
  (load-theme 'atom-one-dark t)
  )

;; ----------------------------------------------------------------------
(use-package zerodark-theme
  ;; :disabled
  :load-path "~/git-clone/zerodark-theme"
  :if window-system
  :config
  (setq zerodark-use-paddings-in-mode-line nil)
  (load-theme 'zerodark t)
  ;; (zerodark-setup-modeline-format)

  (set-face-attribute 'cursor nil
                      :background (face-attribute 'mode-line :foreground)
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

  (set-face-attribute 'mode-line          nil :slant 'italic :height 1.1)
  (set-face-attribute 'mode-line-inactive nil :slant 'italic :height 1.1)
  (set-face-attribute 'minibuffer-prompt  nil :slant 'italic :height 1.1 :foreground "#cc8800")

  (set-face-attribute 'line-number              nil :height 1.1 :slant 'italic :background "#2B2F38" :foreground "#5B6475")
  (set-face-attribute 'line-number-current-line nil :height 1.1 :slant 'italic :background "#2B2F38")

  (set-face-background 'default "#21252B")
  (set-face-attribute 'fringe nil :foreground (face-attribute 'line-number :foreground)
                      	          :background (face-attribute 'line-number :background))

  :if (and window-system (my-font-exists-p "x14y24pxHeadUpDaisy"))
  :config
  (set-face-attribute 'mode-line          nil :family "x14y24pxHeadUpDaisy")
  (set-face-attribute 'mode-line-inactive nil :family "x14y24pxHeadUpDaisy")
  (set-face-attribute 'minibuffer-prompt  nil :family "x14y24pxHeadUpDaisy")

  (set-face-attribute 'line-number              nil :family "x14y24pxHeadUpDaisy")
  (set-face-attribute 'line-number-current-line nil :family "x14y24pxHeadUpDaisy")

  )

;; ----------------------------------------------------------------------
(use-package telephone-line
  :after evil zerodark-theme
  :if window-system
  :config
  (set-face-background 'telephone-line-evil-visual "#009161")
  (set-face-background 'telephone-line-evil-insert "#cc4444")
  (set-face-background 'telephone-line-evil-emacs  "#cc8800")
  (set-face-background 'telephone-line-evil-normal "#0088cc")

  (set-face-attribute 'telephone-line-accent-active nil
                      :background "#7e7e7e" :foreground "#f9f9f9")
  (set-face-attribute 'telephone-line-accent-inactive nil
                      :background "#4e4e4e" :foreground (face-attribute 'mode-line-inactive :foreground))

  (defface telephone-line-accent2-active
    '((t (:background "#5e5e5e" :inherit telephone-line-accent-active))) "")

  (defface telephone-line-accent2-inactive
    '((t (:background "#3a3a3a" :inherit telephone-line-accent-inactive))) "")

  (add-to-list 'telephone-line-faces
               '(accent2 . (telephone-line-accent2-active . telephone-line-accent2-inactive)))

;   (setq telephone-line-lhs
;         '((evil   . (telephone-line-evil-tag-segment))
;           (accent . (telephone-line-vc-segment
;                      telephone-line-process-segment))
;           (accent2 . (telephone-line-buffer-info-segment))
;           (nil    . (telephone-line-buffer-segment))))
  (setq telephone-line-lhs
        '((evil   . (telephone-line-evil-tag-segment))
          (accent . (telephone-line-vc-segment))
          (accent2 . (telephone-line-buffer-info-segment))
          (nil    . (telephone-line-buffer-segment
                     telephone-line-process-segment))))

  (setq telephone-line-rhs
        '((nil    . (telephone-line-misc-info-segment))
          (accent2 . (telephone-line-minor-mode-segment))
          (accent . (telephone-line-major-mode-segment))
          (evil   . (telephone-line-position-segment))))

  (telephone-line-defsegment* telephone-line-buffer-info-segment ()
    `(""
      mode-line-mule-info
      mode-line-modified
      ;; mode-line-client
      ;; mode-line-remote
      ;; mode-line-frame-identification
      ;; ,(telephone-line-raw mode-line-buffer-identification t)
      ))

  (telephone-line-defsegment* telephone-line-buffer-segment ()
    `(; mode-line-mule-info
      ;; mode-line-modified
      ;; mode-line-client
      ;; mode-line-remote
      mode-line-frame-identification
      ,(telephone-line-raw mode-line-buffer-identification t)))

  (setq telephone-line-primary-left-separator 'telephone-line-identity-left
        telephone-line-secondary-left-separator 'telephone-line-identity-hollow-left
        telephone-line-primary-right-separator 'telephone-line-identity-left
        telephone-line-secondary-right-separator 'telephone-line-identity-hollow-left)

  (setq telephone-line-height 16
        telephone-line-evil-use-short-tag nil)

  (telephone-line-defsegment* telephone-line-position-segment ()
    (telephone-line-raw-mod
     (if (eq major-mode 'paradox-menu-mode)
         ;;Paradox fills this with position info.
         mode-line-front-space
       mode-line-position) t))

  (telephone-line-mode 1)

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
(use-package all-the-icons)

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
  ;; :disabled
  :diminish counsel-mode
  :init
  (ivy-mode 1)

  :config
  (counsel-mode 1)
  (setq ivy-use-virtual-buffers t          ;; add ‘recentf-mode’ and bookmarks to ‘ivy-switch-buffer’.
        ivy-height 20
        ;; ivy-count-format ""             ;; does not count candidates
        ivy-initial-inputs-alist nil       ;; no regexp by default
        ivy-on-del-error-function 'ignore
        ivy-extra-directories nil          ;; '("../")
        ivy-re-builders-alist              ;; configure regexp engine.
        '((t   . ivy--regex-ignore-order)) ;; allow input not in order
        counsel-find-file-ignore-regexp "\\.elc\\'"
  )

  ;; ;; my-ivy-find-file
  ;; ;; (defvar my-ivy-find-file-exclude "-not \\( -path '*\\/.svn\\/*' -o -path '*\\/.git\\/*' -o -path '*\\~' -o -path '*\\.DS_Store' -o -path '\\.emacs-places' \\)")

  ;; ;; (defun my-ivy-find-file-exclude-p (str)
  ;; ;;   (catch 'loop
  ;; ;;     (dolist (regex my-ivy-find-file-exclude-regex-list)
  ;; ;;       (when (string-match regex str)
  ;; ;;         (throw 'loop t)))))

  ;; (defun my-ivy-find-file (&optional dir)
  ;;   "list files recursively under specified directory"
  ;;   (interactive "")
  ;;   ;; (let* ((cmd (format "find %s -type f" (if dir dir ".")))
  ;;   ;;        (cands (remove-if 'my-ivy-find-file-exclude-p
  ;;   ;;                          (split-string (shell-command-to-string cmd) "\n" t))))
  ;;   ;;   (ivy-read "%d File: " cands
  ;;   ;;             :action #'find-file
  ;;   ;;             :caller 'my-ivy-find-file)))

  ;;   (let* ((dir (cond ((and dir (file-exists-p dir)) dir)
  ;;                     ((buffer-file-name) (directory-file-name
  ;;                                          (file-name-directory (buffer-file-name))))
  ;;                      (t (file-name-directory (expand-file-name "~")))))
  ;;          (cmd (format "find %s -type f %s" dir my-ivy-find-file-exclude))
  ;;          (cands (split-string (shell-command-to-string cmd) "\n" t)))
  ;;     ;; (ivy-read (format "%s %%d: " dir) cands
  ;;     (ivy-read (format "%s %%d: " dir) cands
  ;;               :action #'find-file
  ;;               :caller 'my-ivy-find-file)))

  (defun my-counsel-find-file ()
    (interactive)
    (call-interactively
     (cond ((and (fboundp 'counsel-gtags-find-file) (locate-dominating-file default-directory "GTAGS"))
            'counsel-gtags-find-file)
           ((and (fboundp 'magit-find-file) (locate-dominating-file default-directory ".git"))
            'magit-find-file)
           (t 'counsel-find-file))))

  ;; refrect .ignore to the root of the project
  (setq counsel-git-cmd "rg --files")
  (setq counsel-rg-base-command
        "rg -i -M 120 --no-heading --line-number --color never %s .")
  
  (defun my-ivy-done ()
    (interactive)
    (cond ((eq last-command 'counsel-find-file)
           (ivy-immediate-done))
          (t (ivy-done))))

  (define-key ivy-minibuffer-map [(return)] 'my-ivy-done)
  ;; (define-key ivy-minibuffer-map [(return)] 'ivy-done)

  
  (defun counsel-rg-at-point ()
    (interactive)
    (counsel-rg (or (symbol-name (symbol-at-point)) "")))

  :bind (("M-r"     . counsel-recentf)
         ("M-o"     . counsel-rg-at-point)
         ("C-x C-b" . counsel-ibuffer)
         ;; ("C-x C-f" . my-counsel-find-file)
         ;; ("C-s"     . swiper)

         :map ivy-minibuffer-map
         ;; ([remap ivy-done] . ivy-immediate-done)
         ([(return)] . my-ivy-done)
         ("M-h" . ivy-backward-kill-word)
         ("C-f" . ivy-avy)

         :map counsel-find-file-map
         ("M-RET" . ivy-immediate-done)
         ;; ("M-c" . ivy-immediate-done)                      ; M-RET

         :map counsel-mode-map
         ("M-RET" . ivy-immediate-done)
         ;; ("M-c" . ivy-immediate-done)                      ; M-RET

         :map evil-motion-state-map
         ("f" . avy-goto-word-1)

         :map evil-normal-state-map
         ("R" . ivy-resume))
  )

;; ----------------------------------------------------------------------
(use-package counsel-etags
  :disabled
  ;; :diminish 
  :after counsel

  )

;; ----------------------------------------------------------------------
(use-package counsel-gtags
  :after counsel evil
  :diminish '(counsel-gtags-mode . "Gtags")
  :init
  (add-hook 'c-mode-hook 'counsel-gtags-mode)

  (setq counsel-gtags-auto-update t
        counsel-gtags-path-style 'root
        )
  
  ;; (defun gtags-update ()
  ;;   (interactive)
  ;;   (let ((s (shell-command-to-string "global -uv")))
  ;;     (if (string-match "not found" s)
  ;;         (call-interactively 'helm-gtags-create-tags)
  ;;       (message "Updated GTAGS files."))))

  (defalias 'gtags-update 'counsel-gtags-update-tags)
  (defalias 'gtags-create 'counsel-gtags-create-tags)

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
  :config
  (tabbar-mode)

  (set-face-attribute 'tabbar-default nil
                      :height 0.85
                      :background (face-attribute 'mode-line :background)
                      :foreground (face-attribute 'telephone-line-accent-active :foreground)
                      :slant 'italic
                      :box nil
                      )

  (set-face-attribute 'tabbar-selected nil
                      :background (face-attribute 'default :background)
                      :foreground (face-attribute 'mode-line :foreground)
                      :slant 'italic
                      :box nil
                      )

  (set-face-attribute 'tabbar-unselected nil
                      :background (face-attribute 'telephone-line-accent-active :background)
                      :foreground (face-attribute 'tabbar-selected :background)
                      :slant 'italic
                      :box nil
                      )

  (set-face-attribute 'tabbar-selected-modified nil
                      :background (face-attribute 'tabbar-selected :background)
                      :foreground (face-attribute 'tabbar-selected :foreground)
                      :slant 'italic
                      :overline "orange" 
                      :box nil
                      )

  (set-face-attribute 'tabbar-modified nil
                      :background (face-attribute 'tabbar-unselected :background)
                      :foreground (face-attribute 'tabbar-unselected :foreground)
                      :slant 'italic
                      :overline "orange"
                      :box nil
                      )

  (set-face-attribute 'tabbar-separator nil
                      :background (face-attribute 'default :background))

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
                         (length (tabbar-riew
                                  (tabbar-current-tabset)))))))))

  :if (and window-system (my-font-exists-p "x14y24pxHeadUpDaisy"))
  :config
  (set-face-attribute 'tabbar-default nil :family "x14y24pxHeadUpDaisy")

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
  (show-paren-mode 1)
  (setq show-paren-style 'expression)
  (set-face-background 'show-paren-match "#263652")
  )

;; ----------------------------------------------------------------------
(use-package expand-region
  :after evil
  :config
  (push 'er/mark-outside-pairs er/try-expand-list)
  (setq expand-region-smart-cursor nil)
  ;; (setq expand-region-autocopy-register "e")
  ;; (setq expand-region-autocopy-kill-ring t)

  (define-key evil-normal-state-map (kbd "+") 'er/expand-region)
  ;; (define-key evil-visual-state-map (kbd "x") 'er/expand-region)
  ;; (define-key evil-visual-state-map (kbd "X") 'er/contract-region)
  (define-key evil-visual-state-map (kbd "+") 'er/expand-region)
  (define-key evil-visual-state-map (kbd "_") 'er/contract-region)
  )

;; ----------------------------------------------------------------------
(use-package rainbow-delimiters
  ;; :disabled
  :config
  (require 'cl-lib)
  (require 'color)

  (set-face-foreground 'rainbow-delimiters-depth-9-face "#9a4040")   ; swap 1 <--> 9
  (set-face-foreground 'rainbow-delimiters-depth-2-face "#ff5e5e")
  (set-face-foreground 'rainbow-delimiters-depth-3-face "#ffaa77")
  (set-face-foreground 'rainbow-delimiters-depth-4-face "#dddd77")
  (set-face-foreground 'rainbow-delimiters-depth-5-face "#80ee80")
  (set-face-foreground 'rainbow-delimiters-depth-6-face "#66bbff")
  (set-face-foreground 'rainbow-delimiters-depth-7-face "#da6bda")
  (set-face-foreground 'rainbow-delimiters-depth-8-face "#afafaf")
  (set-face-foreground 'rainbow-delimiters-depth-1-face "#f0f0f0")   ; swap 1 <--> 9

  (rainbow-delimiters-mode 1)
  (setq rainbow-delimiters-outermost-only-face-count 1)
  (set-face-bold 'rainbow-delimiters-depth-1-face t)

  (add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'c-mode-hook 'rainbow-delimiters-mode)

  )

;; ----------------------------------------------------------------------
(use-package rainbow-mode
  :diminish rainbow-mode
  :config
  (setq rainbow-html-colors nil)
  (add-hook 'lisp-interaction-mode-hook 'rainbow-mode)
  (add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
  ;; (add-hook 'css-mode-hook 'rainbow-mode)
  ;; (add-hook 'less-mode-hook 'rainbow-mode)
  ;; (add-hook 'web-mode-hook 'rainbow-mode)
  ;; (add-hook 'html-mode-hook 'rainbow-mode)
  )

;; ----------------------------------------------------------------------
(use-package sublimity
  :disabled
  :if window-system
  :config
  (sublimity-mode 1)

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
  :config
  ;; (setq sl-scratch-log-file "~/.emacs.d/.scratch-log")  ;; default
  ;; (setq sl-prev-scratch-string-file "~/.emacs.d/.scratch-log-prev")
  (setq sl-restore-scratch-p t)                   ; 復元
  (setq sl-prohibit-kill-scratch-buffer-p t)      ; 削除不能
  ;; *scratch*とscratch-logのメジャーモードをorg-modeにする
  ;; (setq initial-major-mode 'org-mode)
  ;; (add-to-list 'auto-mode-alist '("scratch-log" . org-mode))
  ;; 30秒ごとに自動保存
  (setq sl-use-timer t)
  ;; (setq sl-timer-interval 3)

  (add-to-list 'recentf-exclude "scratch-log-autoloads.el")
  )

;; ----------------------------------------------------------------------
(use-package quick-back
  :after evil
  :load-path "elisp"
  :bind (:map evil-normal-state-map
              ("g m" . quick-back-mark)
              ("g q" . quick-back-jump))
  )

;; ----------------------------------------------------------------------
(use-package hiwin
  :diminish hiwin-mode
  :config
  (set-face-background 'hiwin-face "#313640")
  (hiwin-mode)
  )

;; ----------------------------------------------------------------------
(use-package gist
  :after evil
  :config
  (evil-add-hjkl-bindings gist-list-menu-mode-map 'emacs
    (kbd "/")       'evil-search-forward
    (kbd "n")       'evil-search-next
    (kbd "N")       'evil-search-previous)

  )

;; ----------------------------------------------------------------------
(use-package git-gutter
  :disabled
  :load-path "~/git-clone/emacs-git-gutter"
  )

;; ----------------------------------------------------------------------
(use-package git-gutter-fringe
  ;; :disabled
  :diminish git-gutter-mode
  :after git-gutter fringe-helper
  :init
  (global-git-gutter-mode t)

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
  (set-face-attribute 'git-gutter:unchanged nil :background (face-attribute 'fringe :background))

  (add-hook 'focus-in-hook 'git-gutter)      ; refresh automatically when modifyed current buffer by external program

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
  :diminish beacon-mode
  :config
  ;; (beacon-mode 1)
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
(use-package align
  ;; :disabled
  :config
  (add-to-list 'align-rules-list
               '(tab-stop-assignment
                 (regexp   . "\\(\\s-+\\)")
                 (tab-stop . t)              ; タブ位置でそろえる
                 (modes     . '(c-mode c++-mode))))
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
(use-package discrete
  :load-path "elisp"
  )

;; ----------------------------------------------------------------------
(use-package my-backup
  :load-path "elisp"
  :config
  (setq my-backup-directory "~/bak")
  )

;; ----------------------------------------------------------------------
(use-package flycheck
  :init
  (defun flycheck-c-mode-hook-func ()
    ;; (flycheck-select-checker 'my-c) 
    (flycheck-mode t)
    (setq flycheck-check-syntax-automatically '(mode-enabled save)) ;; new-line also possible
    )

  (add-hook 'c-mode-hook 'flycheck-c-mode-hook-func)

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
              (which-func-mode 1)
              (display-line-numbers-mode)
              (setq compilation-scroll-output t)
              (setq compile-command "cd ~/git-clone/qmk_firmware; make dichotemy:default")
              (setq compilation-auto-jump-to-first-error t)
              (setq compilation-window-height 10)
              ))
  :config
  ;; enable ANSI color in *compilation* buffer
  ;; (require 'ansi-color)
  (defun endless/colorize-compilation ()
    "Colorize from `compilation-filter-start' to `point'."
    (let ((inhibit-read-only t))
      (ansi-color-apply-on-region
       compilation-filter-start (point))))

  (add-hook 'compilation-filter-hook #'endless/colorize-compilation)

  :after telephone-line
  :config
  ;; workaround for *compilation* buffer
  (dolist (f '(compilation-info compilation-warning compilation-error))
    (set-face-background f (face-attribute 'telephone-line-accent-inactive :background)))

  )

;; ----------------------------------------------------------------------
;; arduino mode
(use-package arduino-mode
  :mode (("\\.pde$" . arduino-mode)
         ("\\.ino$" . arduino-mode))
  )

;; ----------------------------------------------------------------------
;; which-func-mode
(setq which-func-unknown "-")
(setq which-func-modes '(c-mode python-mode ruby-mode))
(setq which-func-format '(:propertize which-func-current face which-func))

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
;; discrete setting
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

(defalias 'com 'comment-or-uncomment-region)
(defalias 'ind 'indent-region)

(defalias 'calc 'quick-calc)

;; カーソル行をハイライト
(defalias 'hl 'hl-line-mode)


;; ----------------------------------------------------------------------
; computer independent
(load
 (cond ((eq system-type 'windows-nt) "~/.emacs.d/elisp/_windows.el")
       ((eq system-type 'gnu/linux)  "~/.emacs.d/elisp/_linux.el")
       (t                            "~/.emacs.d/elisp/_mac.el")))

(my-font-lighter)

(message "<-- loaded \"init.el\"")

;;
;; init.el ends here
;;
