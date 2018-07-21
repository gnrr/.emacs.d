;;;; -*- mode: lisp-interaction; syntax: elisp; coding: iso-2022-7bit -*-
;;;;
;;;;    discrete elisp
;;;;
;;;;	Filename: discrete.el
;;;;	Last modified: Mon Mar 04 2013 15:45:58 JST
;;;;
;;;;	based: $Id: discrete.el,v 1.51 2006/02/16 05:13:34 gnrr Exp gnrr $
;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;		    I WROTE
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ common functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun minibuffer-p (&optional window)
  (unless window (setq window (selected-window)))
  (eq window (minibuffer-window (window-frame window))))

(defun get-max-string-length-in-list (list)
  "return maximum value of string length in list"
  (let ((max 0))
    (while list
      (let ((len (length (car list))))
	(setq max (if (< max len) len max)))
      (setq list (cdr list)))
    max))

(defun narrowing-p ()
  "return t if current buffer is narrowed, otherwise return nil."
  (if (string-match "<[0-9]+[ ]*-[ ]*[0-9]+>" (what-cursor-position))
      t
    nil))

(defun show-face-at-point ()
  "Show faces of character in minibuffer."
  (interactive)
  (let* ((s (buffer-substring (point) (1+ (point))))
	 (faces (get-text-property 0 'face s))
	 msg)
    (if faces
	(progn
	  (unless (listp faces)
	    (setq faces (list faces)))
	  (while faces
	    (setq msg (concat msg " \'" (symbol-name (car faces))))
	    (setq faces (cdr faces)))
	  (setq msg (substring msg 1 (length msg))))
      (setq msg "no faces"))
    (message msg)))

(defun get-word-in-string (s &optional offset)
  "Return word string in string."
  (let* ((len (length s))
	 (ofs (if (and offset (> offset 0)) offset 0))
	 (n ofs)
	 (wd (substring s ofs len))
	 ch)
    (while (< n len)
      (setq ch (elt s n))
      (if (or (= ch ?\ ) (= ch ?\t) (= ch ?/))
	  (progn
	    (setq wd (substring s ofs n))
	    (setq n len))
	(setq n (1+ n))))
    wd))

(defun remove-heading-comment-character (s)
  "Return string which is removed heading comment characters.
otherwise return nil,if whole line is consisted of comment characters."
  (let ((len (length s))
	(n 0)
	(nyet t)
	faces)
    (while (and nyet (< n len))
	(setq faces (get-text-property n 'face s))
	(unless (listp faces)
	  (setq faces (list faces)))
	(if (or (memq 'font-lock-comment-face faces)
		(memq 'font-lock-comment-delimiter-face faces))
	    (setq n (1+ n))
	  (setq nyet nil)))
    (if nyet
	nil
      (substring s n len))))

(defun string-comment-p (s)
  "Return t when string is comment, otherwise return nil."
  (when s
    (let ((props (get-text-property 0 'face s)))
      (if (or (eq props 'font-lock-comment-face)
	      (eq props 'font-lock-comment-delimiter-face))
	  t nil))))

;; (defun pt ()
;;   "represent point value at point."
;;   (interactive)
;;   (message "point :%d" (point)))

(defun remove-heading-spaces (s)
  "Return string which is removed heading spaces in string."
  (let ((nyet t)
        (n 0)
        (len (length s))
        beg)
    (while (and nyet (< n len))
      (if (= 32 (char-syntax (elt s n))) ; 32: space class
          (setq n (1+ n))
        (setq nyet nil)))
    (substring s n len)))

(defun get-line-string (num &optional no-prop)
  "Return string of the line which you specified by num. when no-prop is non-nil,
return string with property, otherwise nil return it without property."
  (let (beg)
    (save-excursion
      (goto-char (point-min))
      (if (> (forward-line (1- num)) 0)
	  nil
	(setq beg (point))
	(if no-prop
	    (buffer-substring-no-properties beg (progn (end-of-line) (point)))
	  (buffer-substring beg (progn (end-of-line) (point))))))))

(defun my-replace-match-string (str search replace)
  (let ((s str)
	(pos nil)
	(len (length replace)))
    (while (setq pos (string-match search s pos))
      (setq s (replace-match replace t t s))
      (setq pos (+ pos len)))
    s))

(defun get-variable-name (var)
  "Return string of variable-name."
    (format "%s" (eval var)))

(defun get-directory-from-current-buffer ()
  "return directory of the file which is visited as current buffer
otherwise return nil if current buffer is not visited."
  (let ((curr-path (buffer-file-name))
	dir)
    (if curr-path
	(setq dir (abbreviate-file-name (file-name-directory curr-path)))
      nil)))

(defun get-quote-removed-string (s)
  "Return string which removed quotation characters \(\'\) and
double quotation characters \(\"\) from given string."
  (if (string= s "")
      ""
    (let ((len (length s))
	  (n 0)
	  (new "")
	  c)
      (while (< n len)
	(setq c (elt s n))
	(unless (or (= c ?\') (= c ?\"))
	  (setq new (concat new (string c))))
	(setq n (1+ n)))
      new)))

(defun get-space-removed-string (s)
  "Return string which removed spa ce characters \" \" from given string."
  (if (string= s "")
      ""
    (let ((len (length s))
	  (n 0)
	  (new "")
	  c)
      (while (< n len)
	(setq c (elt s n))
	(unless (= c ? )
	  (setq new (concat new (string c))))
	(setq n (1+ n)))
      new)))

(defun get-beg-end-spaces-removed-string (s)
  "Return string which removed spaces at begin/end of given string."
  (if (string= s "")
      ""
    (let* ((len (length s))
	   (spc ?\ )
	   n c nyet beg end)
      (setq nyet t)
      (setq n 0)
      ;; from begin of string
      (while (and nyet (< n len))
	(setq beg n)
	(setq c (elt s n))
	(unless (= c spc)
	  (setq nyet nil))
	(setq n (1+ n)))

      (setq nyet t)
      (setq n (1- len))
      ;; from end of string
      (while (and nyet (> n -1))
	(setq end n)
	(setq c (elt s n))
	(unless (= c spc)
	  (setq nyet nil))
	(setq n (1- n)))
      (substring s beg (1+ end)))))

(defun buffer-exists-p (buffer-name)
  "Return t when buffer exists which is named `buffer-name', ohterwise return nil."
  (if (member buffer-name
	      (mapcar (function buffer-name) (buffer-list)))
      t
    nil))

(defun find-buffers-by-file-name (file-name)
  "Return buffer when buffer exists which has visiting file name, ohterwise return nil."
  (if (member file-name
	      (mapcar (function buffer-file-name) (buffer-list)))
      t
    nil))


(defun get-current-line-number (&optional pt)
  "Return current line number (starting from 1)"
  (unless pt (setq pt (point)))
  (let (start)
    (save-excursion
	(goto-char (point-min))
	(forward-line 0)
	(setq start (point))
	(goto-char pt)
	(forward-line 0)
	(1+ (count-lines start (point))))))

(defun get-pt-beginning-of-line (&optional pt)
  "Return point value of beginning-of-line."
  (unless pt (setq pt (point)))
  (save-excursion
    (beginning-of-line)
    (point)))

(defun get-pt-end-of-line (&optional pt)
  "Return point value of end-of-line."
  (unless pt (setq pt (point)))
  (save-excursion
    (end-of-line)
    (point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ goto-match-paren
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun goto-match-paren (arg)
;;   "Go to the matching parenthesis if on parenthesis, otherwise insert %.
;; vi style of % jumping to matching brace."
;;   (interactive "p")
;;   (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
;;         ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
;;         (t (self-insert-command (or arg 1)))))

;; (global-set-key (kbd "M-]") 'goto-match-paren)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ goto-match-paren
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-kill-buffer ()
  (interactive)
  (kill-buffer (current-buffer)))

(global-set-key (kbd "C-x k") 'my-kill-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ my-switch-to-buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-switch-to-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(global-set-key "\C-xb" 'my-switch-to-buffer)
;; (global-set-key "\C-xb" 'electric-buffer-list)

;; (add-hook 'electric-buffer-menu-mode-hook
;;   '(lambda ()
;; 	 (view-mode-enter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ my-copy-buffer-file-name
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-copy-buffer-file-name ()
  "copy buffer-file-name to kill-ring."
  (interactive)
  (let ((fn (unwind-protect
		(buffer-file-name)
	      nil)))
    (if fn
	(let ((f (abbreviate-file-name (expand-file-name fn))))
	  (kill-new f)
	  (message "copied: \"%s\"" f))
      (message "no file name"))))

(global-set-key "\C-xf" 'my-copy-buffer-file-name)

;;
;; ;;;@@ copy-buffer-file-name
;;
;; (defun copy-buffer-file-name ()
;;   (interactive)
;;   (let ((str (buffer-file-name)))
;;     (if str
;; 	(progn
;; 	  (setq str (file-name-nondirectory (buffer-file-name)))
;; 	  (kill-new str)			;copy to kill-ring
;; 	  (message (concat "copied \"" str "\"")))
;;       (message (concat "no filename")))))

;; (global-set-key "\C-c\C-v" 'copy-buffer-file-name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ message-buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun message-buffer ()
  "Open `*Messages*' buffer."
  (interactive)
  (let ((buf "*Messages*"))
    (switch-to-buffer-other-window buf t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ my-eval-depth-increase
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-eval-depth-increase ()
  "increase `max-lisp-eval-depth' for large recursive calling."
  (interactive)
  (setq max-lisp-eval-depth (round (* max-lisp-eval-depth 1.5)))
  (message "max-lisp-eval-depth => %d" max-lisp-eval-depth))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ my-customized backward-word, forward-word, backward-kill-word
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun my-backward-word ()
  (interactive)
  (if (bolp)
	 (forward-char -1)
    (let ((pt (save-excursion (forward-word -1) (point))))
      (goto-char (max (get-pt-beginning-of-line) pt)))))

(defun my-forward-word ()
  (interactive)
  (if (eolp)
	 (forward-char 1)
    (let ((pt (save-excursion (forward-word 1) (point))))
      (goto-char (min (get-pt-end-of-line) pt)))))

(defun my-backward-kill-word ()
  (interactive)
  (if (bolp)
	 (delete-backward-char 1)
    (let ((pt (save-excursion (forward-word -1) (point))))
      (delete-region (point) (max (get-pt-beginning-of-line) pt)))))

(global-set-key "\M-b" 'my-backward-word)
(global-set-key "\M-f" 'my-forward-word)
(global-set-key "\M-h" 'my-backward-kill-word)

;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;;;@@ nippo
;; ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defvar nippo-dir "~/biz/nippo/")
;; (defvar work-end-time '(18 . 10)
;;   "hour . minute")

;; (defvar work-end-time-margin 20
;;   "minute")

;; (defun nippo-get-the-date-list (time-list arg)
;;   (let ((ty (nth 5 time-list))
;; 	(tm (nth 4 time-list))
;; 	(td (nth 3 time-list))
;; 	(not-yet t))
;;     (setq td (+ td arg))
;;     (list (nth 0 time-list) (nth 1 time-list) (nth 2 time-list) td tm ty)))

;; (defun nippo-get-proper-file-name (time arg &optional no-today)
;;   (let* ((ext ".txt")
;; 	 (tl (decode-time time))
;; 	 (file-name-today (concat (format-time-string "%y%m%d" time) ext))
;; 	 (file-name file-name-today)
;; 	 (path nil)
;; 	 (is-today nil)
;; 	 (not-yet t)
;; 	 (no-err t))
;;     (while (and not-yet no-err)
;;       (setq path (concat (directory-file-name (expand-file-name nippo-dir)) "/"
;; 			 file-name))
;;       (when (file-exists-p path)
;; 	(setq is-today (if (string= file-name file-name-today) t nil))
;; 	(if no-today
;; 	    (unless is-today
;; 	      (setq not-yet nil))
;; 	  (setq not-yet nil)))
;;       (when not-yet
;; 	(let ((pl (nippo-get-the-date-list tl arg))
;; 	      (pt nil))
;; 	  (if pl
;; 	      (progn
;; 		(setq pt (encode-time (nth 0 pl) (nth 1 pl) (nth 2 pl) (nth 3 pl) (nth 4 pl) (nth 5 pl)))
;; 		(setq file-name (concat (format-time-string "%y%m%d" pt) ext))
;; 		(setq tl pl))
;; 	    (setq no-err nil)))))
;;     (if no-err
;; 	;; found
;; 	(list path is-today)
;;       ;; error (not found)
;;       nil)))

;; (defun nippo-get-todays-file-name (time)
;;   (let* ((ext ".txt"))
;;     (concat (format-time-string "%y%m%d" time) ext)))

;; (defun nippo-get-work-end-time (ct)
;;   (let* ((l (decode-time ct))
;; 	 (cm (nth 1 l))
;; 	 (ch (nth 2 l))
;; 	 (sec (+ (* ch 3600) (* cm 60)))
;; 	 (wh (car work-end-time))
;; 	 (wm (cdr work-end-time)))
;;     (if (< sec (+ (* wh 3600) (* wm 60)))
;; 	(cons wh wm)
;;       (if (< cm (- 60 work-end-time-margin))
;; 	  (cons ch (* (/ (+ cm work-end-time-margin) 10) 10))
;; 	(cons (1+ ch) 0)))))

;; (defun nippo-open-post-process-exists (ct)
;;   ;; time
;;   (goto-char (point-min))
;;   (re-search-forward "- [0-9][0-9]:[0-9][0-9]$")
;;   (let ((we (nippo-get-work-end-time ct)))
;;     (replace-match (format "- %02d:%02d" (car we) (cdr we))))

;;   (set-window-start (selected-window) 1))

;; (defun nippo-open-post-process-new (ct)
;;   (goto-char (point-min))
;;   ;; date
;;   (re-search-forward "[0-9][0-9][0-9][0-9]/[0-9][0-9]/[0-9][0-9] \\(.?\\)$")
;;   (replace-match (format-time-string "%Y/%m/%d (%a)" ct))

;;   ;; time
;;   (re-search-forward "- [0-9][0-9]:[0-9][0-9]$")
;;   (let ((we (nippo-get-work-end-time ct)))
;;     (replace-match (format "- %02d:%02d" (car we) (cdr we))))

;;   ;; next day or next week
;;   (goto-char (point-max))
;;   (re-search-backward "^【.*の予定】$")
;;   (let* ((fri 5)
;; 	 (s (if (< (string-to-number (format-time-string "%w" ct)) fri)
;; 		"【明日の予定】"
;; 	      "【来週の予定】")))
;;     (replace-match s))

;;   (set-window-start (selected-window) 1))


;; (defun nippo ()
;;   (interactive)
;;   (let* ((ct (current-time))
;; 	 (f (nippo-get-proper-file-name ct -1 nil))) ;include today's
;;     (if f
;; 	(let ((file-name (nth 0 f)))
;; 	  (find-file file-name)
;; 	  (if (nth 1 f) ;; today's file exist?
;; 	      (nippo-open-post-process-exists ct)
;; 	    (setq path (concat (directory-file-name (expand-file-name nippo-dir)) "/"
;; 			       (nippo-get-todays-file-name ct)))
;; 	    (write-file path)
;; 	    (nippo-open-post-process-new ct)
;; 	    (write-file path)
;; 	    (message "today's nippo is new created."))
;; 	  (set-buffer-modified-p nil))
;;       (message "can not open nippo."))))

;; (defun nippo-get-the-days-nippo-file-name-relative (dir-list curr-file x)
;;   ""
;;   (if curr-file
;;       (let ((n 0)
;; 	    (found nil))
;; 	(while (and (not found) (< n (length dir-list)))
;; 	  (if (string= (nth n dir-list) curr-file)
;; 	      (setq found t)
;; 	    (setq n (1+ n))))
;; 	(if found
;; 	    (if (< n (length dir-list))
;; 		(progn
;; 		  (setq n (+ n x))
;; 		  (if (>= n 0)
;; 		      (nth (+ n x) dir-list)
;; 		    nil))
;; 	      nil)
;; 	  nil))
;;     (nth (1- (length dir-list)) dir-list)))

;; (defun nippo-open-the-days-nippo-or-memo (arg)
;;   (let* ((cf (expand-file-name (buffer-file-name)))
;; 	 (curr (if (file-exists-p cf) cf nil))
;; 	 (dir (file-name-directory cf))
;; 	 (l (directory-files dir t "[.]txt$"))
;; 	 (f (nippo-get-the-days-nippo-file-name-relative l curr arg)))
;;     (when (and f (not (string= cf f)))
;;       (kill-buffer (current-buffer))
;;       (find-file-read-only f))
;;     f))

;; (defun nippo-open-previous ()
;;   (interactive)
;;   (unless (nippo-open-the-days-nippo-or-memo -1)
;;     (message "can not found previous.")))

;; (defun nippo-open-next ()
;;   (interactive)
;;   (unless (nippo-open-the-days-nippo-or-memo 1)
;;     (message "can not found next.")))

;; (defun nippo-get-the-days-nippo-file-name-absolute (dir-list date)
;;   (let ((ext ".txt")
;; 	(n 0)
;; 	(found nil)
;; 	(nyet t)
;; 	path)
;;     (while (and nyet (< n (length dir-list)))
;;       (setq path (nth n dir-list))
;;       (cond
;;        ((string= path (concat (directory-file-name (expand-file-name nippo-dir))
;; 			      "/"
;; 			      date
;; 			      ext))
;; 	(setq found t)
;; 	(setq nyet nil))
;;        ((< (string-to-number date) (string-to-number
;; 				    (file-name-sans-extension
;; 				     (file-name-nondirectory path))))
;; 	(setq nyet nil))
;;        (t (setq n (1+ n)))))
;;     (if nyet
;; 	(cons "" nil)
;;       (cons path found))))

;; (defun nippo-open-go (arg)
;;   (interactive "sdate:")
;;   (let ((l (directory-files (expand-file-name nippo-dir) t "[.]txt$"))
;; 	(f nil))
;;   (cond
;;    ((or (string= arg "b") (string= arg "beg") (string= arg "s") (string= arg "start"))
;;     (setq f (cons (car l) t)))		  ; (filename . t)
;;    ((or (string= arg "") (string= arg "e") (string= arg "end"))
;;     (setq f (cons (nth (1- (length l)) l) t))) ; (filename . t)
;;    (t
;;     (cond
;;      ((string-match "^[0-9][0-9][0-9][0-9][0-9][0-9]$" arg) ;yymmdd
;;       (setq f (nippo-get-the-days-nippo-file-name-absolute l arg)))
;;      ((string-match "^[0-9][0-9][0-9][0-9]$" arg)		  ;mmdd
;;       (let ((yy (format-time-string "%y" (current-time))))
;; 	(setq f (nippo-get-the-days-nippo-file-name-absolute l
;; 							     (concat yy arg)))))
;;      ((string-match "^[0-9][0-9]$" arg)			  ;dd
;;       (let ((yymm (format-time-string "%y%m" (current-time))))
;; 	(setq f (nippo-get-the-days-nippo-file-name-absolute l
;; 							     (concat yymm arg)))))
;;      ((string-match "^[0-9]$" arg)			  ;d
;;       (let ((yymm (format-time-string "%y%m" (current-time))))
;; 	(setq f (nippo-get-the-days-nippo-file-name-absolute l
;; 							     (concat yymm "0" arg)))))
;;      (t (message "invalid format: s(start), b(begin) | e(end) |d | dd | mmdd | yymmdd")))))
;;   (when f
;;     (cond
;;      ((string= (car f) "")
;;       (message "not found spesified nippo."))
;;      (t
;;       (unless (string= (buffer-file-name (current-buffer)) (car f))
;; 	(kill-buffer (current-buffer))
;; 	(find-file-read-only (car f))
;; 	(unless (cdr f)
;; 	  (message "not found. opened nearest date's."))))))))


;; (define-key text-mode-map "\C-x\C-p" 'nippo-open-previous)
;; (define-key text-mode-map "\C-x\C-n" 'nippo-open-next)
;; (define-key text-mode-map "\C-x\C-g" 'nippo-open-go)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ my-previous-line and my-next-line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
'(progn    ; comment-beg
(defvar next-prev-line-highlight nil)
(make-variable-buffer-local 'next-prev-line-highlight)

(defun previous-line (arg)
  "My customized previous-line"
  (interactive "p")
  (if (interactive-p)
      (progn
	(condition-case nil
	    (unless (= (narrowed-current-line) 1)
	      (line-move (- arg)))
	  (goto-char (point-min)))
	(when next-prev-line-highlight
	  (highlight-current-line)))
    (line-move (- arg)))
  nil)

(defun narrowed-current-line ()
  "Print the current buffer line number and narrowed line number of point."
  (let ((opoint (point)) start)
    (save-excursion
      (save-restriction
	(goto-char (point-min))
	(widen)
	(forward-line 0)
	(setq start (point))
	(goto-char opoint)
	(forward-line 0)
	(1+ (count-lines start (point)))))))

(defun next-line (arg)
  "My customized next-line"
  (interactive "p")
;;   (if (interactive-p)
;;       (progn
	;(call-interactively 'next-line)
;; 	(line-move arg)
;; 	(when next-prev-line-highlight
;; 	  (highlight-current-line)))
;; 	)
  (let ((noerr (if (active-minibuffer-window) t nil)))
    (line-move arg noerr)))
;;   nil)

) ; comment-end

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ insert-tab-character
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun insert-tab-character ()
  (interactive)
  (insert "\t"))

;; bind to C-tab
;; (global-set-key (quote [C-tab]) 'insert-tab-character)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ insert-key-string etc...
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun get-key-string (key)
  (let* ((str (describe-key-briefly key))
	 (end (string-match " runs " str)))
    (unless end
      (setq end (string-match " is " str)))
    (substring str 0 end)))

(defvar insert-key-string (list "" " "))
(defun insert-key-string (key)
  (interactive "kkey:")
  (insert (car insert-key-string)
	  (get-key-string key)
	  (car (cdr insert-key-string))))

(defun get-command-string (key)
  (let* ((str (describe-key-briefly key)))
    (string-match " command " str)
    (setq beg (match-end 0))
    (if beg
	(substring str beg)
      (""))))

(defvar insert-command-string (list "" ""))
(defun insert-command-string (key)
  (interactive "kkey:")
  (insert (car insert-command-string)
	  (get-command-string key)
	  (car (cdr insert-command-string))))

(defvar insert-key-command-string (list "" "\t\t" "\n"))
(defun insert-key-command-string (key)
  (interactive "kkey:")
  (insert (car insert-key-command-string)
	  (get-key-string key)
	  (nth 1 insert-key-command-string)
	  (get-command-string key)
	  (nth 2 insert-key-command-string)))
;; key-bind
;; (global-set-key "\C-x\C-y" 'insert-key-string)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ enum-buffer-names
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun enum-buffer-names ()
  "存在するバッファ名をすべて列挙し、scratchバッファに表示する。"
  (interactive)
  (setq buff-list (buffer-list))
  (set-buffer "*scratch*")
  (end-of-buffer)
  (insert "\n")
  (insert "---- enum-buffer-names start --------------------------\n")
  (while buff-list
    (insert "\"")
    (insert (buffer-name (car buff-list)))
    (insert "\"\n")
    (setq buff-list (cdr buff-list)))
  (insert "---- enum-buffer-names end ----------------------------\n")
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ visible-whole-buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun visible-whole-buffer ()
  "カレントバッファ上でオーバレイが invisible な文字をすべて表示する。"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (< (point) (point-max))
      (let (ol-start ol-list)
	(setq ol-start (next-overlay-change (point)))
	(setq ol-list (overlays-at ol-start))
	(mapcar (function (lambda (ol)
			    (overlay-put ol 'invisible nil))) ol-list))
      (forward-char 1))))

(defun hide-all-comment-lines ()
  "カレントバッファ上のコメント行をすべて隠す。"
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (end-of-line)
    (while (< (point) (point-max))
      (let (start end)
	(if (whole-comment-line-p)
	    (progn
	      (setq start (progn
			    (beginning-of-line)
			    (point)))
	      (setq end (progn
			  (forward-line)
			  (point)))
	      (when (< start end)
		(overlay-put (make-overlay start end) 'invisible t)))
	  (forward-line))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ commentize-and-next-line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun text-syntax-table-not-all (start end class)
  "start - end 間で、syntax-tableを調べて、指定されたclass以外のものが
出現する位置を返す。すべての文字がclassならnilを返す。"
  (let ((pt nil)
	(ret nil))
    (save-excursion
      (goto-char start)
      (while (and (< (point) end) (not pt))
	(setq ret (char-syntax (char-after)))
	(unless (= class ret)
	  (setq pt (point)))
	(forward-char 1)))
    pt))

(defun text-face-not-all (start end name)
  "start - end 間で、指定されたnameを含んでいないfaceが出現する位置を返す。
すべての文字のfaceがnameを含んでいればnilを返す。"
  (let ((pt nil)
	(faces ()))
    (save-excursion
      (goto-char start)
      (while (and (< (point) end) (not pt))
	(setq faces (get-text-property (point) 'face))
	(unless (listp faces)
	  (setq faces (list faces)))
	(unless (or (memq name faces) (memq 'font-lock-comment-delimiter-face faces))
	  (setq pt (point)))
	(forward-char 1)))
    pt))

(defun whole-comment-line-p ()
  "カレント行がすべてコメントならt, そうでなければnilを返す。
一部コメントが含まれている場合もnilを返す。空行の場合もnilを返す。"
  (let (start end nc)
    (save-excursion
      (end-of-line)
      (setq end (point))
      (beginning-of-line)
      (setq start (point)))
    (if (< start end)
      (progn
	;; 白文字クラス以外が最初に出現する位置を探す。
	(setq start (text-syntax-table-not-all start end 32)) ;; 32は白文字クラス
	(if start
	    (setq nc (text-face-not-all start end 'font-lock-comment-face))
	  (setq nc nil))
	(when (integerp nc)
	  (setq nc (text-face-not-all start end 'font-lock-comment-delimiter-face)))
	(if (integerp nc)
	    nil
	  t))
      nil)))

(defun commentize-and-next-line (&optional ARG)
  "カレント行をコメント化／アンコメント化し、次の行へ移動する。
C-uにより前置引数を使うと次の行に移動しない。
ユーザ変数 commentize-and-next-line-set-mark が nil以外のときは
コメント化する最初の行をマークする。（デフォルト）"
  (interactive "P")
  (let (beg end)
    (save-excursion
      (beginning-of-line)
      (setq beg (point))
      (end-of-line)
      (setq end (point)))
    (when (< beg end)
      (if (whole-comment-line-p)
	  (uncomment-region beg end)
	(comment-region beg end))))
  (unless ARG
    (when (and commentize-and-next-line-set-mark
	       (not (eq last-command 'commentize-and-next-line)))
      (push-mark))
    (forward-line 1)
    (beginning-of-line)))

;; (global-set-key "\M-;" 'commentize-and-next-line)

;; (defvar commentize-and-next-line-set-mark t
;;   "*nil以外のときは、コメント化する最初の位置をマークする。
;; C-x C-x (exchange-point-and-mark) 等で便利。")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ indent-for-comment (from xyzzy)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun indent-for-comment-gnrr (&optional ARG)
  (interactive "P")
  (if ARG
      ;; set-comment-column
      (let ((col (current-column)))
        (setq comment-column col)
        (message "set comment-column to %d." col))
    (indent-for-comment)
    (when (eq evil-state 'normal)
      (evil-insert 1))))

;; (global-set-key [?\C-\;] 'indent-for-comment-gnrr)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ my-copy-word
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my-copy-word-thing-n 0)
(make-variable-buffer-local 'my-copy-word-thing-n)
(defvar my-copy-word-prev-str "")
(make-variable-buffer-local 'my-copy-word-prev-str)
(defvar my-copy-word-ini-str "")
(make-variable-buffer-local 'my-copy-word-ini-str)

(defun my-copy-word ()
  (interactive)
  (let ((sel '(word symbol filename))
	(n my-copy-word-thing-n)
	(str nil)
	beg end ol)
    (if (eq last-command 'my-copy-word)
	(if (< n (1- (length sel)))
	    (setq n (1+ n))
	  (setq n 0))
      (setq n 0))
    (while (or (null (prog1
			 (setq str (thing-at-point (nth n sel)))
		       (set-text-properties 0 (length str) nil str)))
	       (and (string= str my-copy-word-prev-str)
		    (not (string= str my-copy-word-ini-str))))
      (if (< n (1- (length sel)))
	  (setq n (1+ n))
	(setq n 0)))
    (kill-new str)			;copy to kill-ring
    (setq my-copy-word-thing-n n)
    (setq my-copy-word-prev-str str)
    (setq my-copy-word-ini-str (when (= n 0) str))
    (message "copied: %s" str)
    (save-excursion			;get beg, end
      (let ((len (length str)))
	(while (null (progn
		       (setq beg (point))
		       (setq end (+ beg len))
		       (string= str
				(buffer-substring-no-properties beg end))))
	  (backward-char))))
    (setq ol (make-overlay beg end))	;hilight word
    (overlay-put ol 'face 'highlight)
    (sit-for 5)
    (delete-overlay ol)))

; key-bind
;; (global-set-key [?\C-=] 'my-copy-word)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ my-save-buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create parent directories when saving a new file
(defvar my-save-buffer-default-path nil
  "default directory where to save")
(make-variable-buffer-local 'my-save-buffer-default-path)

(defun create-directory (directory)
  (if (y-or-n-p (format "Parent directory %s not exists, Create? " directory))
      (let* ((dl "/")
	     (dire "")
	     (list (split-string directory dl)))
	(while list
	  (setq dire (concat dire (car list) dl))
	  (unless (file-exists-p dire)
	    (make-directory (directory-file-name dire)))
	  (setq list (cdr list)))
	t)
    nil))

(defvar my-save-buffer-interactive-arg-active-p nil)
(defun my-save-buffer-interactive-arg (&optional initial)
  (let* ((my-save-buffer-interactive-arg-active-p t)
	 (insert-default-directory (null initial)))
    (if (buffer-modified-p)
	(if buffer-file-name
	    ; buffer has a file-name
	    (let* ((name (expand-file-name buffer-file-name))
		   (directory (file-name-directory name)))
	      (if (file-exists-p directory)
		  (progn (save-buffer)
			 'done)
		(if (create-directory directory)
		    (progn (save-buffer)
			   'done)
		  (error "Abort"))))
	  ; buffer does not have a file-name
	  (let* ((rd (read-file-name "File to save in: " nil nil nil initial))
		 (name (expand-file-name rd))
		 (directory (file-name-directory name)))
	    (if (file-exists-p name)
		(list 'already-exists rd)
	      (if (file-exists-p directory)
		  (progn (write-file name)
			 'done)
		(if (create-directory directory)
		    (progn (write-file name)
			   'done)
		  (list 'cant-create-parent-dir rd))))))
      'buffer-no-mod)))

(add-hook 'minibuffer-setup-hook
	  '(lambda ()
	     (when my-save-buffer-interactive-arg-active-p
	       (end-of-line))))

(defun my-save-buffer ()
  (interactive
   (let ((nyet t)
	 (wait 1)
	 r)
     (while nyet
       (setq r (my-save-buffer-interactive-arg my-save-buffer-default-path))
       (if (listp r)
	   (let ((err (car r))
		 (inp (cadr r)))
	     (cond ((eq err 'already-exists)
		    (message "\(File already exists\)") (sit-for wait))
		   ((eq err 'cant-create-parent-dir)
		    (message "\(Can not create parent directory\)") (sit-for wait))
		   ((eq err 'buffer-no-mod)
		     (setq nyet nil))))
	 (when (symbolp r)
	   (when (eq r 'buffer-no-mod)
	     (message "\(No changes need to be saved\)"))
	   (setq nyet nil)))))
   nil))

;; key-bind
(global-set-key "\C-x\C-s" 'my-save-buffer)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ my-write-file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; create parent directories when writing a new file
(defvar my-write-file-interactive-arg-active-p nil)

;; (defun my-write-file-write-proc (path)
;;   (if (file-exists-p path)
;;       ;同じファイル名が既に存在している場合
;;       (progn
;;         (message "already exists")
;;         (sit-for 5)
;;         (my-write-file-interactive-arg path))
;;     ;同じファイル名が存在しない場合
;;     (write-file path)))

(defun my-write-file-interactive-arg (&optional initial)
  (let* ((my-write-file-interactive-arg-active-p t)
	 (insert-default-directory nil)
	 (path initial))
    (when (and (not initial) (buffer-file-name))
      (setq path (buffer-file-name)))
    (let* ((name (expand-file-name (read-file-name "File to save in: "
						   nil nil nil path)))
	   (directory (file-name-directory name)))
      (if (file-exists-p directory)
	  ;ディレクトリがすでに存在している場合
	  (write-file name t)
	;ディレクトリが存在しない場合
	(if (create-directory directory)
	    ;ディレクトリを作れた場合
	    (my-write-file-interactive-arg name)
	  ;ディレクトリを作れなかった場合
	  (my-save-buffer-interactive-arg name))))))

(defun my-write-file-minibuffer-setup-hook ()
  (when my-write-file-interactive-arg-active-p
    (end-of-line)))

(add-hook 'minibuffer-setup-hook
	  'my-write-file-minibuffer-setup-hook)

(defun my-write-file ()
  (interactive (my-write-file-interactive-arg)))

(global-set-key "\C-x\C-w" 'my-write-file)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ my-find-file
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; thank somebody at 2ch.
(defvar my-find-file-interactive-arg-active-p nil)
(defun my-find-file-interactive-arg (str &optional initial)
  (let* ((my-find-file-interactive-arg-active-p t)
	 (insert-default-directory (null initial))
	 (name (read-file-name str nil nil nil initial))
	 (wild-p current-prefix-arg)
	 (file (if wild-p name (expand-file-name name))))
    (cond ((if wild-p (file-expand-wildcards name)
			 (file-exists-p file))
		   (list file wild-p))
		  ((y-or-n-p (format "%s not exists, New file? " (file-name-nondirectory file)))
		   (list file wild-p))
		  (t (my-find-file-interactive-arg "Find file: " name)))))

(defun my-find-file-minibuffer-setup-hook ()
  (when my-find-file-interactive-arg-active-p
    (end-of-line)))

(add-hook 'minibuffer-setup-hook
	  'my-find-file-minibuffer-setup-hook)

(defun my-find-file (filename &optional wildcards)
  (interactive (my-find-file-interactive-arg "Find file: "))
  (find-file filename wildcards))

(defun my-find-alternate-file (filename &optional wildcards)
  (interactive (my-find-file-interactive-arg "Find alternate file: "))
  (find-alternate-file filename wildcards))

(defun my-find-file-pre-process (arg)
  "switch `my-find-file' or `my-find-alternate-file' by arg."
  (interactive "P")
  (call-interactively (if arg
			  'my-find-alternate-file
			'my-find-file)))

;; (global-set-key "\C-x\C-f" 'my-find-file-pre-process)
;; (global-unset-key "\C-x\C-v")		; find-alternate-file


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ my-insert-filename
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my-insert-filename-start "Filename:"
  "*The filename string is inserted following this string.")
(defvar my-insert-filename-lines 10
  "*Its applied upto this line number from head of file.")

(defun my-insert-filename ()
  "Insert filename like as time-stamp."
  (interactive)
  (save-excursion
    (save-restriction
      (narrow-to-region (point-min) (progn
				      (goto-char (point-min))
				      (end-of-line)
				      (let ((lines my-insert-filename-lines))
					(while (and (not (eobp)) (> lines 1))
					  (next-line 1)
					  (end-of-line)
					  (setq lines (1- lines))))
				      (point)))
      (goto-char (point-min))
      (let ((cs case-fold-search))
	;;case insesitive
	(setq case-fold-search nil)
	(when (search-forward my-insert-filename-start nil t)
	  (while (not (eolp))
	    (delete-char 1))
	  (insert " ")
	  (insert (file-name-nondirectory buffer-file-name)))
	(setq case-fold-search cs))))
  nil)

;; (add-hook 'write-file-hooks 'my-insert-filename)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ my-just-one-space
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defvar my-just-one-space-state nil)
;; (defun my-just-one-space ()
;;   (interactive)
;;   (if (and (eq last-command 'my-just-one-space) (null my-just-one-space-state))
;;       (progn
;; 	(backward-delete-char 1)
;; 	(setq my-just-one-space-state t))
;;     (progn
;;       (just-one-space)
;;       (setq my-just-one-space-state nil))))

;; (global-set-key "\M- " 'my-just-one-space)
;; (global-set-key "\C- " 'my-just-one-space)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ toggle-narrowing-region
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar toggle-narrowing-region-window-start nil)
(defvar toggle-narrowing-region-previous-rend nil)
(defun toggle-narrowing-region (beg end)
  "Toggle narrowing/widening region."
  (interactive "r")
  (if (narrowing-p)
      ; now nallowed
      (progn
	(widen)
;; 	(when (integerp toggle-narrowing-region-window-start)
;; 	  (set-window-start nil toggle-narrowing-region-window-start))
	(message "<< Widened >>"))
    ; now widened
    (let ((rend end))
      (when (and (= beg end)
		 (integerp toggle-narrowing-region-previous-rend))
	(setq rend toggle-narrowing-region-previous-rend))
      (if (= beg rend)
	  (message "No region.")
	(setq toggle-narrowing-region-window-start (window-start))
	(narrow-to-region beg rend)
	(goto-char (point-min))
	(setq toggle-narrowing-region-previous-rend rend)
	(message ">> Narrowed <<")))))

(global-set-key "\C-xnn" 'toggle-narrowing-region)
(global-unset-key "\C-xnw")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ insert-paren-*    [], {}, <>, "", ''
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar insert-paren-kaku-state nil)
;; (defun insert-paren-kaku ()
;;   "Insert paired [] or {} working like insert-parenthesis."
;;   (interactive)
;;   (if (eq last-command 'insert-paren-kaku)
;;       (progn
;; 	(forward-char -1)
;; 	(delete-char 2)
;; 	(if (null insert-paren-kaku-state)
;; 	    (progn
;; 	      (insert ?\{)
;; 	      (save-excursion
;; 		(insert ?\}))
;; 	      (setq insert-paren-kaku-state t))
;; 	  (progn
;; 	    (insert ?\[)
;; 	    (save-excursion
;; 	      (insert ?\]))
;; 	    (setq insert-paren-kaku-state nil))))
;;     (progn
;;       (insert ?\[)
;;       (save-excursion
;; 	(insert ?\]))
;;       (setq insert-paren-kaku-state nil))))

(defun insert-paren-kaku ()
  "Insert paired [] or {} working like insert-parenthesis."
  (interactive)
  (if (eq last-command 'insert-paren-kaku)
      (progn
	(forward-char -1)
	(delete-char 2)
	(if (null insert-paren-kaku-state)
	    (progn
	      (insert ?\[)
	      (save-excursion
		(insert ?\]))
	      (setq insert-paren-kaku-state t))
	  (progn
	    (insert ?\{)
	    (save-excursion
	      (insert ?\}))
	    (setq insert-paren-kaku-state nil))))
    (progn
      (insert ?\{)
      (save-excursion
	(insert ?\}))
      (setq insert-paren-kaku-state nil))))

;; (global-set-key "\M-[" 'insert-paren-kaku)

(defvar insert-paren-quote-state nil)
(defun insert-paren-quote ()
  "Insert paired single-quote or double-quote working like insert-parenthesis."
  (interactive)
  (if (eq last-command 'insert-paren-quote)
      (progn
	(forward-char -1)
	(delete-char 2)
	(if (null insert-paren-quote-state)
	    (progn
	      (insert ?\")
	      (save-excursion
		(insert ?\"))
	      (setq insert-paren-quote-state t))
	  (progn
	    (insert ?\')
	    (save-excursion
	      (insert ?\'))
	    (setq insert-paren-quote-state nil))))
    (progn
      (insert ?\')
      (save-excursion
	(insert ?\'))
      (setq insert-paren-quote-state nil))))
;; (global-set-key "\M-'" 'insert-paren-quote)

(defun insert-paren-gtlt()
  "Insert paired <> working like insert-parenthesis."
  (interactive)
  (insert ?\<)
  (save-excursion
    (insert ?\>)))
;; (global-set-key "\M-<" 'insert-paren-gtlt)
;; (global-unset-key "\M->")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ beginning-of-buffer-without-marking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun beginning-of-buffer-without-marking ()
  "more simple beginning-of-buffer without marking."
  (interactive)
  (goto-char (point-min)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ end-of-buffer-without-marking
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun end-of-buffer-without-marking ()
  "more simple end-of-buffer without marking."
  (interactive)
  (goto-char (point-max)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ my-forward-word
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun my-forward-word ()
;;   "For people who dislike working of default forward-word."
;;   (interactive)
;;   (let ((nword 1))
;;     (when (string= (string (char-syntax (char-after))) "w")
;;	 (setq nword 2))
;;   (forward-word nword)
;;   (forward-word -1)))

;; (global-set-key "\M-f" 'my-forward-word)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ my-kill-word
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-kill-word ()
  "For people who dislike working of default kill-word."
  (interactive)
  (let ((start-class (char-syntax (char-after))))
    (while (= start-class (char-syntax (char-after)))
      (delete-char 1))))

(global-set-key "\M-d" 'my-kill-word)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ my-backward-kill-word
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun my-backward-kill-word ()
;;   "For people who dislike working of default backward-kill-word."
;;   (interactive)
;;   (let ((start-class (char-syntax (char-before))))
;;     (while (= start-class (char-syntax (char-before)))
;;       (backward-delete-char 1)
;;       (when (= 10 (char-before)) ;; 10 = carriage return
;; 	(setq start-class 0)))))

;; (global-set-key "\M-h" 'my-backward-kill-word)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ my-backward-kill-word-minibuffer
;;  and some fix for me
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-backward-kill-word-minibuffer-sub (delimiter)
  "for path delimiter"
  (let (buf)
    (when (> (point) (minibuffer-prompt-end))
      (setq buf (concat (string (char-before)) buf))
      (backward-delete-char 1))
    (while (and (not (= delimiter (char-before)))
		(> (point) (minibuffer-prompt-end)))
      (setq buf (concat (string (char-before)) buf))
      (backward-delete-char 1))
    (setq dir-list (cons buf dir-list))))

(defun my-backward-kill-word-minibuffer ()
  "replace for path delimiter, word delimiter, white space "
  (interactive)
  (let ((ch (cond
	     ;; elisp functions --> "-"
	     ((and minibuffer-completion-table
		   (sequencep minibuffer-completion-table))
	      ?-)
	     ;; path string --> "/"
	     ((eq minibuffer-completion-table 'read-file-name-internal)
	      ?/)
	     ;; others --> " "
	     (t ?\ ))))
    (my-backward-kill-word-minibuffer-sub ch)))

(defun my-forward-word-minibuffer ()
  "undo path string"
  (interactive)
  (unless (null dir-list)
    (insert (car dir-list))
    (setq dir-list (cdr dir-list))))

(defun my-backward-delete-char-minibuffer ()
  "replace backward-delete-char-minibuffer"
  (interactive)
  (when (> (point) (minibuffer-prompt-end))
    (backward-delete-char 1)))

(defun my-backward-char-minibuffer ()
  "replace backward-char-minibuffer"
  (interactive)
  (when (> (point) (minibuffer-prompt-end))
    (backward-char 1)))

(defun my-forward-char-minibuffer ()
  "replace forward-char-minibuffer"
  (interactive)
  (when (< (point) (point-max))
    (forward-char 1)))


;; (add-hook 'minibuffer-setup-hook
;; 	    (function
;; 	      (lambda ()
;; 		(setq dir-list '())
;; 		(local-set-key "\M-h" 'my-backward-kill-word-minibuffer)
;; 		(local-set-key "\M-f" 'my-forward-word-minibuffer)
;; 		(local-set-key "\C-h" 'my-backward-delete-char-minibuffer)
;; 		(local-set-key "\C-b" 'my-backward-char-minibuffer)
;; 		(local-set-key "\C-f" 'my-forward-char-minibuffer)
;; 		)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ my-undo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require 'redo)
;; (defun my-undo-redo (&optional ARG)
;;   "This function invocates either undo or redo according to ARG.
;; When ARG is nil, undo is called.
;; Otherwise,  ARG is t, redo is called."
;;   (interactive "*P")
;;   (if ARG
;;       (redo 1)
;;     (undo 1)))

;; (global-set-key "\C-z" 'my-undo-redo)
;; ;
                                        ; (global-set-key "\M-z" 'my-undo-redo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ find the next tags
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun find-tag-next ()
;;   "Search for another tag that matches the last tagname or regexp used."
;;   (interactive)
;;   (find-tag tags-file-name t))

;; (global-set-key "\M-," 'find-tag-next)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ multiply current line.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun duplicate-line (&optional ARG)
  "Multiply current line."
  (interactive"*p")
  (let ((cnt 0)
	(pt (point)))
    (save-excursion
	(end-of-line)
	(setq str
	      (buffer-substring-no-properties (point) (progn
							(beginning-of-line)
							(point))))
	(if (null ARG)
	    (setq ARG 1))
	(while (< cnt ARG)
	  (insert str)
	  (newline)
	  (setq cnt (1+ cnt))))
    (goto-char pt)
    (next-line 1)))

;; (global-set-key [?\M-=] 'duplicate-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ set-mark w/ fringe-indicator
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar fringe-indicator-ol nil)
(defun fringe-indicator (pt bitmap)
  (let ((s (make-string 1 ?x)))
	(when fringe-indicator-ol (delete-overlay fringe-indicator-ol))
	(setq fringe-indicator-ol (make-overlay pt (1+ pt)))
	(put-text-property 0 1 'display (list 'left-fringe bitmap) s)
	(overlay-put fringe-indicator-ol 'before-string s)))

(defadvice set-mark-command (after fringe-indicator-adv activate)
  "indicate mark-position at fringe."
  (fringe-indicator (point) 'right-triangle))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;		    SOMEBODY WROTE. THANKS A LOT.		   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ my-calc
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://d.hatena.ne.jp/uhiaha888/20110117/1295273922
(defun my-calc (beg end)
  (interactive (list (point) (mark t)))
  (if (and (interactive-p) transient-mark-mode (not mark-active))
      (calculator)
    (calc-grab-region beg end nil)))
(global-set-key "\C-c\C-c" 'my-calc)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ create tag-file automatically
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.bookshelf.jp/cgi-bin/goto.cgi?file=meadow&node=tagsfile%20maker
;; (defadvice find-tag (before c-tag-file activate)
;;   "Automatically create tags file."
;;   (let ((tag-file (concat default-directory "TAGS")))
;;     (unless (file-exists-p tag-file)
;;       (shell-command "etags *.[ch] *.el .*.el -o TAGS 2>/dev/null"))
;;     (visit-tags-table tag-file)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ my-keyboard-quit
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www-tsujii.is.s.u-tokyo.ac.jp/~yoshinag/tips/elisp_tips.html#yankundo
;; keyboard-quit で連続実行する種類のコマンドの結果を元に戻す

;; ;; dabbrev-expand 用 marker
;; (defvar my-dabbrev-marker nil)
;; (defadvice dabbrev-expand-by-category (before my-mark activate)
;;   (unless (eq last-command 'dabbrev-expand)
;;     (setq my-dabbrev-marker (point))))

;; ;; kill-ring-yank-pointer の backup 用の変数
;; (defvar my-kill-ring-yank-pointer-backup nil)
;; ;; yank するときには kill-ring-pointer の位置を覚えておく
;; (defadvice yank (before my-kill-ring-yank-pointer-backup activate)
;;   (setq my-kill-ring-yank-pointer-backup kill-ring-yank-pointer))

;; (defun my-keyboard-quit ()
;;     "Wrapped keyboard-quit for yank(-pop)/dabbrev-expand/undo.
;; This command executes keyboard-quit, it deletes the inserted text when
;; the last-command is yank(-pop) or dabbrev-expand, recovers
;; kill-ring-yank-pointer when yank(-pop), and repeats redo as possible."
;;   (interactive)
;;   (cond ((eq last-command 'yank)
;;	 (let ((inhibit-read-only t))
;;	   (delete-region (point) (mark t)))
;;	 ;; yank/yank-pop したテキストを消す
;;	 (setq kill-ring-yank-pointer my-kill-ring-yank-pointer-backup)
;;	 ;; kill-ring-yank-pointer の位置を yank 前に戻す
;;	 )
;;	((eq last-command 'dabbrev-expand)
;;	 ;; dabbrev-expand したテキストを消す
;;	   (delete-region (point) my-dabbrev-marker)))
;;	((and (featurep 'redo) (eq last-command 'undo))
;;	 ;; undo している最中なら redo できるだけする
;;	   (while 1 (redo 1)))
;;	((or (eq last-command 'my-bury-buffer)
;;	     (eq last-command 'my-grub-buffer))
;;	 ;; バッファを循環中なら最初のバッファに戻る
;;	 (switch-to-buffer (car my-visible-blst))))
;;   (keyboard-quit))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@ beginning-of-minibuffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;http://www-tsujii.is.s.u-tokyo.ac.jp/~yoshinag/tips/elisp_tips.html#minibuf
;;ミニバッファにて C-a でプロンプトの後に移動するようにする

;; (defun beginning-of-minibuffer ()
;;   (interactive)
;;   (goto-char (minibuffer-prompt-end)))

;; (when (= emacs-major-version 21)
;;   (add-hook 'minibuffer-setup-hook
;; 	    (function
;; 	      (lambda ()
;; 		(local-set-key "\C-a" 'beginning-of-minibuffer)))))


;;;
;;; end
;;;
(provide 'discrete)

;;; discrete.el ends here
