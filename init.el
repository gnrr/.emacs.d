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

 vc-follow-symlinks t
 ring-bell-function 'ignore
 parens-require-spaces nil
 transient-mark-mode nil
 initial-scratch-message ""
 indent-tabs-mode nil
 tab-width 4
 tab-stop-list  '(4 8 12 16 20 24 28 32 36 40 44 48 52 56 60)
 comment-column 80

 ;; display-line-numbers-grow-only t
 ;; display-line-numbers-width-start 10
 ;; line-number-display-width 10
 display-line-numbers-width 4
 
 ;; 1行スクロール
 ;; (setq scroll-conservatively most-positive-fixnum)
 scroll-margin 5
 next-screen-context-lines 5
 scroll-preserve-screen-position t

 next-line-add-newlines nil                  ; バッファ末尾に余計な改行コードを防ぐための設定
 idle-update-delay 0.3

 ;;
 ;; backup files
 ;; http://yohshiy.blog.fc2.com/blog-entry-319.html
 ;;
 ;; backup to `hoge.txt~'
 backup-directory-alist '((".*" . "~/bak"))
 version-control     t  ;; 番号付けによる複数保存 存実行の有無
 kept-new-versions   5  ;;                   最新の保持数
 kept-old-versions   1  ;;                   最古の保持数
 delete-old-versions t  ;;                   範囲外を削除

 ;; backup to `#hoge.txt#'
 make-backup-files nil                       ; #のバックアップファイルを作成しない
 auto-save-file-name-transforms   '((".*" "~/bak" t))
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
(set-window-buffer nil (current-buffer)) ; Use them now.

;; save-place
(setq save-place-file "~/.emacs.d/.emacs-places")
(save-place-mode 1)                               ; Enable save-place

;; ミニバッファの履歴を保存する
(savehist-mode 1)
(setq history-length 1000)

(setq indent-line-function 'indent-relative-maybe)
(global-set-key "\C-m" 'newline-and-indent)  ; Returnキーで改行＋オートインデント

;; mode-line
(column-number-mode t)
(set-face-attribute 'mode-line          nil :box nil) ; モードラインを非3D化
(set-face-attribute 'mode-line-inactive nil :box nil)

;; モードラインの割合表示を総行数表示に
(defvar my-lines-page-mode t)

(when my-lines-page-mode
  (setq my-mode-line-format "%3d:%%4l/%d")
  (setq mode-line-position '(:eval (format my-mode-line-format
                                           (1+ (current-column))
                                           (count-lines (point-max) (point-min))))))

;; タイトルバーにファイルのフルパス表示
(defmacro replace-home-directory-string (file-name)
  `(if ,file-name
      (let ((regexp "^/Users/[^/]+/"))
        (replace-regexp-in-string regexp "~/" ,file-name))
    ""))

(defun emacs-version-briefly ()
  (let ((lst (split-string (emacs-version))))
    (concat (nth 1 lst) (nth 2 lst))))

(setq frame-title-format '(format "%s"
      (:eval (if (buffer-file-name) (replace-home-directory-string (buffer-file-name)) (buffer-name)))))

;; ----------------------------------------------------------------------
;; key unbinding / binding
(keyboard-translate ?\C-h ?\C-?)        ; c-h

(global-unset-key (kbd "M-,"))                          ; xref
(global-unset-key (kbd "M-."))                          ; xref
(global-unset-key (kbd "C-f"))                          ; scroll
(global-unset-key (kbd "C-b"))                          ; scroll
(global-unset-key (kbd "C-z"))                          ; suspend-frame
(global-unset-key (kbd "C-x C-z"))                      ; suspend-frame
(global-unset-key (kbd "C-x o"))                        ; other-window
(global-unset-key (kbd "M-t"))                          ; transpose-word
(global-unset-key (kbd "M-'"))                          ; abbrev-prefix-mark
(global-unset-key [f11])                                ; toggle-frame-fullscreen

(global-set-key (kbd "C-0") 'delete-window)
(global-set-key (kbd "C-1") 'delete-other-windows)
(global-set-key (kbd "C-2") 'split-window-below)
(global-set-key (kbd "C-3") 'split-window-right)
(global-set-key (kbd "C-o") 'other-window)

(global-set-key (kbd "M-g") 'goto-line)
(global-set-key (kbd "M-v") 'new-empty-buffer-other-frame)
(global-set-key (kbd "C-x ;") 'comment-set-column)         ; c-x ;
(global-set-key [24 67108923] 'comment-indent)             ; c-x c-;
(global-set-key (kbd "C-x t") 'revert-buffer)
(global-set-key (kbd "C-x C-t") 'toggle-truncate-lines)
(global-set-key (kbd "C-x n f") 'narrow-to-defun)

(global-set-key (kbd "M-9") 'insert-parentheses)
(global-set-key (kbd "M-P") 'beginning-of-buffer)
(global-set-key (kbd "M-N") 'end-of-buffer)
(global-set-key (kbd "M-;") 'comment-line)

(define-key isearch-mode-map (kbd "C-b") 'isearch-delete-char)

(defun my-func ()
  (interactive)
  (message "called \'my-func\'"))

(global-set-key [f2] 'my-func)

;; ----------------------------------------------------------------------
(use-package cl)

;; ----------------------------------------------------------------------
(use-package telephone-line
  :after zerodark-theme
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

  (setq telephone-line-lhs
        '((evil   . (telephone-line-evil-tag-segment))
          (accent . (telephone-line-vc-segment
                     telephone-line-process-segment))
          (accent2 . (telephone-line-buffer-info-segment))
          (nil    . (telephone-line-buffer-segment))))
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
(use-package atom-one-dark-theme
  :disabled
  :config
  (load-theme 'atom-one-dark t)
  )

;; ----------------------------------------------------------------------
(use-package zerodark-theme
  ;; :disabled
  :load-path "~/git-clone/zerodark-theme"
  :config
  (setq zerodark-use-paddings-in-mode-line nil)

  (load-theme 'zerodark t)
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

  (set-face-attribute 'mode-line          nil :family "x14y24pxHeadUpDaisy" :slant 'italic :height 1.1)
  (set-face-attribute 'mode-line-inactive nil :family "x14y24pxHeadUpDaisy" :slant 'italic :height 1.1)
  (set-face-attribute 'minibuffer-prompt  nil :family "x14y24pxHeadUpDaisy" :slant 'italic :height 1.1 :foreground "#cc8800")

  (set-face-attribute 'line-number nil              :family "x14y24pxHeadUpDaisy" :height 1.1 :slant 'italic :background "#2F343D" :foreground "#7A8496")
  (set-face-attribute 'line-number-current-line nil :family "x14y24pxHeadUpDaisy" :height 1.1 :slant 'italic :background "#2F343D")

  (set-face-background 'default "#21252B")
  (set-face-attribute 'fringe nil :foreground (face-attribute 'line-number :foreground)
                      	          :background (face-attribute 'line-number :background))
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
(use-package evil
  :config
  (evil-mode 1)
  (evil-set-initial-state 'help-mode 'emacs)
  (evil-set-initial-state 'edebug-mode 'emacs)
  ;; (evil-make-intercept-map edebug-mode-map nil)
  (defalias #'forward-evil-word #'forward-evil-symbol)

  ;; インサートモードではEmacsキーバインド
  (setcdr evil-insert-state-map nil)
  (define-key evil-insert-state-map [escape] 'evil-normal-state)

  (define-key evil-normal-state-map (kbd "M-.") nil)        ; evil-repeat-pop-next
  (define-key evil-motion-state-map (kbd "C-f") nil)
  (define-key evil-motion-state-map (kbd "C-b") nil)
  (define-key evil-motion-state-map (kbd "C-o") nil)		; evil-jump-backward
  (define-key evil-motion-state-map (kbd "C-e") nil)		; evil-scroll-down

  ;; (define-key evil-motion-state-map (kbd "C-n") nil)
  ;; (define-key evil-motion-state-map (kbd "C-p") nil)
  (define-key evil-motion-state-map (kbd "C-h") nil)
  (define-key evil-motion-state-map (kbd "M-h") nil)

  (define-key evil-motion-state-map (kbd "j") 'evil-next-visual-line)
  (define-key evil-motion-state-map (kbd "k") 'evil-previous-visual-line)
  (define-key evil-motion-state-map (kbd "4") 'evil-end-of-line)
  (define-key evil-motion-state-map (kbd "]") 'evil-jump-item)

  (define-key evil-insert-state-map (kbd "C-h") 'delete-backward-char)
  (define-key evil-insert-state-map (kbd "M-h") 'my-backward-kill-word)

  (evil-ex-define-cmd "q[uit]" 'kill-this-buffer)

  (define-key evil-normal-state-map (kbd "TAB") 'indent-or-insert-tab)
  (define-key evil-normal-state-map (kbd "U") 'undo-tree-redo)
  (define-key evil-normal-state-map (kbd "SPC SPC") 'evil-scroll-down)
  (define-key evil-normal-state-map (kbd "S-SPC S-SPC") 'evil-scroll-up)

  (defun evil-return-insert-mode-after-save ()
    (when evil-insert-state-minor-mode
      (funcall (evil-escape--escape-normal-state))))

  (add-hook 'after-save-hook 'evil-return-insert-mode-after-save)

  (defun my-evil-paste (&optional arg)
    (interactive  "P")
    (if (memq last-command '(evil-paste-before evil-paste-after))
        (call-interactively 'evil-paste-pop)
      (call-interactively (if arg 'evil-paste-before 'evil-paste-after))))
  (define-key evil-normal-state-map (kbd "p") 'my-evil-paste)


  (defun my-gg ()
    (interactive)
    (if (eq last-command this-command)
        (exchange-point-and-mark t)
      (push-mark (point) t)
      (goto-char (point-min))))

  (define-key evil-motion-state-map (kbd "gg") 'my-gg)

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
(use-package evil-org
  :after evil
  ;; :diminish evil-org-mode
  :config
)

;; ----------------------------------------------------------------------
(use-package neotree
  :after all-the-icons
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

  (defun counsel-rg-at-point ()
    (interactive)
    (counsel-rg (symbol-name (symbol-at-point))))

  :bind (("M-r"     . counsel-recentf)
         ("M-o"     . counsel-rg-at-point)
         ("C-x C-f" . my-counsel-find-file)
         ;; ("C-s"     . swiper)

         :map ivy-minibuffer-map
         ("M-h" . ivy-backward-delete-char)
         ;; ("TAB" . ivy-partial)

         :map ivy-mode-map
         ("C-'" . ivy-avy))
  )

;; ----------------------------------------------------------------------
(use-package counsel-etags
  ;; :diminish 
  :after counsel

  )

;; ----------------------------------------------------------------------
(use-package counsel-gtags
  :diminish counsel-gtags-mode 
  :after counsel
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

  :bind (:map evil-motion-state-map
              ("f" . avy-goto-word-1)
         :map evil-normal-state-map
              ("g t" . counsel-gtags-dwim)
              ;; ("g t" . counsel-gtags-find-definition)
              ("g r" . counsel-gtags-find-reference)
              ("g s" . counsel-gtags-find-symbol)
              ("g h" . counsel-gtags-go-backward))

)

;; ----------------------------------------------------------------------
(use-package helm
  :disabled
  :diminish helm-mode
  :functions my-font-lighter
  :init
  (setq helm-display-source-at-screen-top nil
        helm-buffers-fuzzy-matching t
        helm-recentf-fuzzy-match t
        helm-ff-auto-update-initial-value nil
        helm-display-header-line nil
        helm-find-files-doc-header nil
        ;; helm-split-window-in-side-p t
        ;; helm-move-to-line-cycle-in-source t
        ;; helm-candidate-number-limit 200
        )

  :config
  (helm-mode 1)
  (helm-autoresize-mode t)

  (setq-default helm-ff-skip-boring-files t)
  (add-to-list 'helm-boring-file-regexp-list "scratch-log-.*$")

  (defun helm-font-families ()
    "helm version of the `anything-font-families' at http://d.hatena.ne.jp/mooz/20110320/p1"
    (require 'cl)
    (interactive)
    (let ((helm-candidate-number-limit 1000))
      (helm :sources (helm-build-sync-source "font-families"
                       :candidates (mapcar '(lambda (f)
                                              (propertize f 'font-lock-face
                                                          (list :family f :height 1.4)))
                                           (sort (delete-duplicates (font-family-list)) 'string<))
                       :fuzzy-match t
                       :action (helm-make-actions
                                "Yank font family name" 'kill-new
                                "Test as 'default" '(lambda (x)
                                                      (set-face-attribute 'default nil
                                                                          :family (format "%s" x)))
                                "Test as 'mode-line" '(lambda (x)
                                                        (set-face-attribute 'mode-line nil
                                                                            :family (format "%s" x)))
                                ))
            :buffer "*helm font families*")))

  (my-font-lighter)

  :bind (("M-x" . helm-M-x)
	     ("M-y" . helm-show-kill-ring)
	     ("C-x b" . helm-mini)
	     ("C-x C-f" . helm-find-files)
	     ("C-x C-b" . helm-buffers-list)

         :map helm-map
         ;; TAB, SHIFT-TABで候補選択を移動
         ([tab]     . helm-next-line)
         ([backtab] . helm-previous-line)
         ([?`]      . helm-select-action))
)

;; ----------------------------------------------------------------------
(use-package helm-ag
  :disabled
  :after helm
  :config
  ;; (setq helm-ag-base-command "ag --nocolor --nogroup")
  (setq helm-ag-base-command "rg --vimgrep --no-heading")		; ripgrep
  (setq helm-ag-insert-at-point 'symbol)

  :bind (("M-o" . helm-do-ag))
)

;; ----------------------------------------------------------------------
(use-package helm-gtags
  :disabled
  :diminish helm-gtags-mode
  :after helm
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
(use-package helm-descbinds
  :disabled
  :diminish helm-descbinds
  :after helm
  :config
  (helm-descbinds-mode)
  )

;; ----------------------------------------------------------------------
(use-package recentf
  :after ivy
  :config
  (setq recentf-max-saved-items 5000) ;; 履歴保存の数
  ;; (setq recentf-auto-cleanup 'never)  ;; 存在しないファイルは消さない
  (setq recentf-exclude '("/recentf" ".recentf"))
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
                      ;; :family (face-attribute 'fixed-pitch-serif :family)
                      :family "x14y24pxHeadUpDaisy"
                      :height 0.85
                      :background (face-attribute 'tool-bar :background)
                      ;; :background (face-attribute 'tabbar-default :background)
                      ;; :foreground (face-attribute 'tool-bar :foreground)
                      ;; :weight 'light
                      :slant 'italic
                      :box nil
                      )

  (set-face-attribute 'tabbar-unselected nil
                      ;; :inherit tabbar-default
                      :background (face-attribute 'tool-bar :background)
                      ;; :foreground (face-attribute 'mode-line-inactive :foreground)
                      :foreground "#080808"
                      ;; :weight 'light
                      :slant 'italic
                      :box nil
                      )

  (set-face-attribute 'tabbar-selected nil
                      :background (face-attribute 'default :background)
                      :foreground (face-attribute 'mode-line :foreground)
                      ;; :foreground "#E8E8E8"
                      ;; :weight 'normal
                      :slant 'italic
                      :box nil
                      )

  (set-face-attribute 'tabbar-selected-modified nil
                      :background (face-attribute 'default :background)
                      :foreground (face-attribute 'mode-line :foreground)
                      ;; :foreground "#E8E8E8"
                      ;; :weight 'normal
                      :slant 'italic
                      :box nil
                      )

  (set-face-attribute 'tabbar-modified nil
                      :background (face-attribute 'tool-bar :background)
                      :foreground "#080808"
                      ;; :weight 'normal
                      :slant 'italic
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
(use-package smartparens
  :diminish smartparens-mode
  :config
  (smartparens-global-mode)
  (show-smartparens-global-mode t)
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
  ;; :load-path "~/git-clone/expand-region.el"
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
  :config
  (require 'cl-lib)
  (require 'color)

  (set-face-foreground 'rainbow-delimiters-depth-1-face "#ca7070")
  (set-face-foreground 'rainbow-delimiters-depth-2-face "#ff5e5e")
  (set-face-foreground 'rainbow-delimiters-depth-3-face "#ffaa77")
  (set-face-foreground 'rainbow-delimiters-depth-4-face "#dddd77")
  (set-face-foreground 'rainbow-delimiters-depth-5-face "#80ee80")
  (set-face-foreground 'rainbow-delimiters-depth-6-face "#66bbff")
  (set-face-foreground 'rainbow-delimiters-depth-7-face "#da6bda")
  (set-face-foreground 'rainbow-delimiters-depth-8-face "#afafaf")
  (set-face-foreground 'rainbow-delimiters-depth-9-face "#f0f0f0")

  (rainbow-delimiters-mode 1)
  (setq rainbow-delimiters-outermost-only-face-count 1)
  (set-face-bold 'rainbow-delimiters-depth-1-face nil)

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
  :config
  (sublimity-mode 1)

  )
;; ----------------------------------------------------------------------
(use-package scratch-log
  :config
  ;; (setq sl-scratch-log-file "~/.emacs.d/.scratch-log")  ;; default
  ;; (setq sl-prev-scratch-string-file "~/.emacs.d/.scratch-log-prev")
  (setq sl-restore-scratch-p t)           ;復元
  (setq sl-prohibit-kill-scratch-buffer-p t) ;削除不能
  ;; *scratch*とscratch-logのメジャーモードをorg-modeにする
  ;; (setq initial-major-mode 'org-mode)
  (add-to-list 'auto-mode-alist '("scratch-log" . org-mode))
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
(use-package gist)

;; ----------------------------------------------------------------------
(use-package git-gutter-fringe
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

  :bind (([M-down] . git-gutter:next-hunk)
         ([M-up]   . git-gutter:previous-hunk))

  )

;; ----------------------------------------------------------------------
(use-package flycheck
  :config
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

  (defun flycheck-c-mode-hook-func ()
    ;; (flycheck-select-checker 'my-c) 
    (flycheck-mode t)
    )
  (add-hook 'c-mode-common-hook 'flycheck-c-mode-hook-func)

  :bind (([S-down] . flycheck-next-error)
         ([S-up]   . flycheck-previous-error))
  )

;; ----------------------------------------------------------------------
(use-package c-mode
  :mode "\\.h$"
  :init
  (add-hook 'c-mode-hook
            (lambda ()
              (c-set-style "stroustrup")
              (define-key c-mode-map "\C-i" 'indent-or-insert-tab)
              (modify-syntax-entry ?_ "w")                ; アンダーバーをワード区切りとしない
              (setq comment-start "// ")                  ; コメントを // にする
              (setq comment-end "")
              (setq compilation-read-command nil)         ; make のオプションの確認は不要
              (setq compilation-ask-about-save nil)       ; make するとき save する
              (setq compile-command "make")               ; make時のデフォルトコマンド
              (setq case-fold-search nil)            ; case sensitive
              (cwarn-mode)
              (which-func-mode 1)
              (display-line-numbers-mode)
              ))
  :config
  (add-to-list 'align-rules-list
                '(tab-stop-assignment
                  (regexp   . "\\(\\s-+\\)")
                  (tab-stop . t)              ; タブ位置でそろえる
                  (modes     . '(c-mode c++-mode))))

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
;; which-func-mode
(setq which-func-unknown "-")
(setq which-func-modes '(c-mode python-mode ruby-mode))
(setq which-func-format '(:propertize which-func-current face which-func))

;; ----------------------------------------------------------------------
;; diminish
(defmacro safe-diminish (file mode &optional new-name)
  "https://github.com/larstvei/dot-emacs/blob/master/init.org"
  `(with-eval-after-load ,file
     (diminish ,mode ,new-name)))

(safe-diminish "undo-tree" 'undo-tree-mode)
(safe-diminish "eldoc" 'eldoc-mode)
(safe-diminish "abbrev" 'abbrev-mode)

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

(defun indent-or-insert-tab ()
  (interactive)
  (let ((pos (point)))
    (funcall indent-line-function)
    (when (= pos (point))
      (insert "\t"))))

(global-set-key "\C-i" 'indent-or-insert-tab)

;; カーソル行をハイライト
;; (global-hl-line-mode t)
(defalias 'hl 'hl-line-mode)

(custom-set-faces
 ;; custom-set-faces was added by Custom. If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance. If there is more than one, they won't work right.
 '(hl-line ((t (:background "#141619")))))

;;;
;;; command aliases
;;;

;; edebug-defun
(eval-after-load "edebug"
  '(defalias 'ede 'edebug-defun))

;; apropos
(defalias 'a 'apropos)
(defalias 'l 'display-line-numbers-mode)


(defalias 'com 'comment-or-uncomment-region)
(defalias 'ind 'indent-region)


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
