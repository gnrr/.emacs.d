;;; -*- Mode: Lisp; Package: editor -*-
;;;
;;; discrete.l
;;;

(provide "discrete")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@@ my-recenter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my-recenter-cnt)
(defun my-recenter ()
  (interactive)
  (setq my-recenter-cnt (if (eq *last-command* 'my-recenter) (mod (1+ my-recenter-cnt) 3) 0))
  (case my-recenter-cnt
    (0 (recenter 0))
    (1 (recenter))
    (t (recenter -1))))

(global-set-key #\C-l 'my-recenter)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@@ util
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require "emacs")

;;
(defun split-sex-region (beg end)
  (interactive "*r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (replace-buffer ") +" (concat ") " (string #\C-j)) :regexp t)))
  (indent-region (mark) (point)))

;; buffer-content
(defun buffer-content (buffer)
  "return list of buffer contens."
  (with-input-from-buffer (buffer)
    (let (l lines)
      (loop
        (handler-case
            (setq l (read-line nil t))
          (error (c)
            (return lines)))
        (push l lines)))))

;; test to lisp
(defun test ()
  (dbg-msgbox "test, override this function 'test'."))

(defun test-interactive ()
  (interactive)
  (test))

(global-set-key #\M-2 'test-interactive)


;; posion of beginning/end of buffer
(defun bob ()
  "return position of beginning of buffer."
  (save-excursion
    (goto-char (point-min))
    (point)))

(defun eob ()
  "return position of end of buffer."
  (save-excursion
    (goto-char (point-max))
    (point)))

;; scan-files-invisible
(defun scan-files-invisible (pattern dir match-func &key once wild case-fold regexp)
  ""
  (let ((files (directory dir :absolute t :recursive t :wild wild :file-only t ))
        result fr)
    (dolist (f files)
      (setq fr (scan-file-single-invisible pattern f match-func :once once :case-fold case-fold :regexp regexp))
      (when fr (push (cons f fr) result) (when :once (return))))
    result))

(defun scan-file-single-invisible (pattern file match-func &key once case-fold regexp)
  ""
  (let ((buf (create-new-buffer "*scan*"))
        result)
    (when regexp (compile-regexp pattern case-fold))
    (set-buffer buf)
    (insert-file-contents file)
    (while (scan-buffer pattern :no-dup t :case-fold case-fold :regexp regexp)
      (push (funcall match-func) result)
      (when :once (return)))
    (delete-buffer buf)
    result))

;; usage example
(defun find-lisp-definition-scan (pattern dir-list)
  ""
  (let (result)
    (dolist (d dir-list)
      (when (and (file-exist-p d)
                 (file-directory-p d))
        (setq result (car (scan-files-invisible pattern d #'match-beginning :once t :wild "*.l" :regexp t))))
      (when result (return)))
    (values (car result) (second result))))



;; ?r???g?C?Åg?O?Åh?c?C???c?d?O?ÅE
(defun builtin-function-p (symbol)
  "symbol ?a?r???g?C?Åg?O?Åh?c?C???c?d?O?ÅE?B"
  (and (fboundp symbol)
       (si::*builtin-function-p (symbol-function symbol))))


;; ?d?!?s?d?i??
(defun uniq-line-region (from to &optional case-insensitive)
  "?d?!?s?d?i???ÅE?e?Bcase-insensitive ?d?E?a?ÅE?e?c nil ?I?e?Åˆ?I?AÅea?Å˜???Å ?Å˜???I?a?E?d?ÅE?e?B
non-nil ?I?A?Å·?IÅea?Å˜???Å ?Å˜???d?a?E?É ?E?Åë?B"
  (interactive "*r")
  (save-excursion (save-restriction
    (narrow-to-region from to)
    (goto-char (point-min))
    (let (l)
      (loop
        (let ((s (buffer-substring (progn (goto-eol) (point)) (progn (goto-bol) (point)))))
          (unless (member s l :test (if case-insensitive #'string-equal #'string=))
            (setq l (cons s l)))
          (unless (forward-line 1) (return))))
      (delete-region from to)
      (with-output-to-selected-buffer
        (map nil #'(lambda (x) (format t "~A~%" x)) (nreverse l)))))))

(defun uniq-line-region-case-insensitive (from to)
  "?d?!?s?d?i???ÅE?e?BÅea?Å˜???Å ?Å˜???d?a?E?É ?E?Åë?B"
  (interactive "*r")
  (uniq-line-region from to t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@@ functional
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; @@@ my-write-file
(defun my-write-file (path)
  (interactive "FWrite file: ")
  (let ((buf (selected-buffer)))
    (write-file path)
;;  (message (format nil "wrote to: ~A" path))
    (find-file path)
    (kill-buffer buf)))

(global-set-key '(#\C-x #\C-w) 'my-write-file)

;;; @@@ find-file-at-point
(defun find-file-at-point ()
  (interactive)
  (flet ((near-point-c ()
           (let ((skip "[a-zA-Z0-9]/\.\\_-"))
             (save-excursion
               (let ((end (progn (skip-chars-forward skip) (point)))
                     (beg (progn (skip-chars-backward skip) (point))))
                 (buffer-substring beg end))))))
    (let ((path-str (case buffer-mode
                  ('(lisp-mode lisp-interaction-mode) (word-near-point))
                  (t (near-point-c))))
          (buffer-path (get-buffer-file-name)))
      (cond
       ((null buffer-path) (message "this buffer has no file."))
       (t (let ((target (concat-path (directory-namestring buffer-path) path-str)))
            (cond ((file-exist-p target) (find-file target))
                  (t (message "not found ~S" target)))))))))

(global-set-key #\M-0 'find-file-at-point)

;;; @@@ aliases
;(defalias 'ap 'apropos)

;;; @@@ diff-with-next-buffer
(defvar diff-with-next-buffer-history nil)

(setf (get 'diff-with-next-buffer 'ed::minibuffer-history-variable)
      'diff-with-next-buffer-history)

(defun diff-with-next-buffer ()
  (interactive)
  (flet ((next-buffer ()
           (get-next-buffer (selected-buffer) nil t nil)))
    (let ((buf-new (read-buffer-name "Buffer new: "
                                     :default (buffer-name (selected-buffer))
                                     :history 'diff-with-next-buffer))
          (buf-old (read-buffer-name "Buffer old: "
                                     :default (buffer-name (next-buffer))
                                     :history 'diff-with-next-buffer)))
      (set-buffer-fold-width nil)                ; ?? doesnot work.
      (diff (get-buffer-file-name buf-new) (get-buffer-file-name buf-old)))))


;;;@@@ grep-xyzzy-lisp
;;;    xyzzy ?I?O?Åh???A?c?d grep
(defun grep-xyzzy-lisp (name)
  (interactive "sName: " :default0 (word-near-point))
  (let ((dir-list *load-path*))
    (dolist (d dir-list)
             (when (file-exist-p d)
               (grep-xyzzy-lisp-scan-files name d))))
  (switch-to-buffer "*grep*"))

(defun grep-xyzzy-lisp-scan-files (name dir)
  (let ((mask "*.l"))
    (ed::scan-files name mask dir)))


;;;@@@ find-symbol-c-proj
;;; l_mat.c ?d?Q?AÅfÅı?E?V?Åg?{???dÅfT?É ?A?X?e?[?^?X?o?[?E?\?|?ÅE?e
(defun find-symbol-from-c-mat (sym fn)
  (let ((pattern (concat "^volatile +const +\\(.+\\) *" sym " *= *(\\1)(\\(.+\\));")))
    (save-excursion
      (scan-file-single-invisible pattern fn #'(lambda () (match-string 2)) :once t :regexp t))))

(defun find-symbol-from-h (sym fn)
  (let ((pattern (concat "^ *#define +" sym " +\\(.+\\)$")))
    (save-excursion
      (scan-file-single-invisible pattern fn #'(lambda () (match-string 1)) :once t :regexp t))))

;; find-symbol function for c-proj
(defun find-symbol-c-proj (sym fn)
  (flet ((get-fn-h (fn)
           (concat-path (directory-namestring fn)
                        (concat (car (split-string (pathname-name fn) #\_)) ".h")))
         (get-fn-c-mat (fn)
           (substitute-string fn "_l_mat\.c$" "_c_mat.c")))
    (let* (val)
      (cond ((string-matchp "_l_mat\.c$" fn)
             (setq val (cond ((find-symbol-from-h sym (get-fn-h fn)))
                             ((find-symbol-from-c-mat sym (get-fn-c-mat fn)))
                             (t '("?c?A?c?e?U?A?n?B"))))
             (message (car val)))
            (t (message "c_mat.c ?Å˜?a?E?Åë?A?ÅE?B"))))))

;; global-command
(defun find-symbol-gnrr ()
  (interactive)
  (let ((sym (word-near-point))
        (fn (get-buffer-file-name)))
    (cond ((string-matchp "_l_mat\.c$" fn) ; c-proj?p
           (find-symbol-c-proj sym fn))
          (t (message "?Å}?I?o?b?t?@?A?I?@Åh\?É ?U?1?n?B")))))

(global-set-key #\M-s 'find-symbol-gnrr)


;;;@@@ find-path-from-top-directory
;;;    ?t?@?C?????d?f?B???N?g???c?cÅfT?É ?A?t???p?X?d?O?ÅE
(defun find-path-from-top-directory (name dirs)
  "name ?A?Åë???t?@?C?????d?f?B???N?g??ÅhzÅÒo?A???o?É ?A?A?t???p?X?d?O?ÅE?BÅeÅ˜?Y?É ?E?ÅP?e?I nil ?d?O?ÅE?B
ÅgÅP???t?@?C???a?!?Åh?AÅeÅ˜?Y?É ???A?É ?A?a?A?A?ÅÒ?E?c?A?ÅP??1?A?É ?c?O?3?E?Åë?B
???oÅeI?U?I?f?B???N?g???I???X?g?A?^?|?A?a?a?Åë?B?f?B???N?g?????I???o?E?I?g?|?E?Åë?B"
  (flet ((get-path-list (dir wild)
           (directory dir :absolute t :recursive t :file-only t :wild wild))
         (name-filter (name path)
           (let ((n (car (last (split-string path #\/)))))
             (if (string-equal name n) path nil))))
    (unless (consp dirs) (setq dirs (cons dirs nil)))
    (let* ((ext (pathname-type name))
           (w (if ext (concat "*." ext) "*"))
          find)
    (dolist (d dirs)
      (when (file-exist-p d)
        (setq find (find name (get-path-list d w) :test #'name-filter)))
      (when find (return find))))))

;;;@@@ sort
(defun sort-line-region(from to &optional (test #'string-lessp))
 (interactive "*r")
 (if (< to from)
      (rotatef from to))
 (let ((start nil) line result)
    (while (string-match "^.*\n" (buffer-substring from to) start)
        (setq start (match-end 0))
        (push (match-string 0) line))
    (setq line (stable-sort line test))
    (dolist (item line result)
      (setq result (concat result item)))
    (delete-region from to)
    (insert result)))


;;;@@@ uniq
(defun uniq-line-region (from to &optional case-insensitive)
  "?d?!?s?d?i???ÅE?e?Bunix ?R?}?Åg?h?I uniq Åe?Åg?(?)?B
case-insensitive ?d?E?a?ÅE?e?c nil ?I?e?Åˆ?I?AÅea?Å˜???Å ?Å˜???I?a?E?d?ÅE?e?B
non-nil ?I?A?Å·?IÅea?Å˜???Å ?Å˜???d?a?E?É ?E?Åë?B"
  (interactive "*r")
  (save-excursion (save-restriction
    (narrow-to-region from to)
    (goto-char (point-min))
    (let (l)
      (loop
        (let ((s (buffer-substring (progn (goto-eol) (point)) (progn (goto-bol) (point)))))
          (unless (member s l :test (if case-insensitive #'string-equal #'string=))
            (setq l (cons s l)))
          (unless (forward-line 1) (return))))
      (delete-region from to)
      (with-output-to-selected-buffer
        (map nil #'(lambda (x) (format t "~A~%" x)) (nreverse l)))))))

(defun uniq-line-region-case-insensitive (from to)
  "?d?!?s?d?i???ÅE?e?Bunix ?R?}?Åg?h?I uniq Åe?Åg?(?)?BÅea?Å˜???Å ?Å˜???d?a?E?É ?E?Åë?B"
  (interactive "*r")
  (uniq-line-region from to t))


;;;@@@ for test --> \M-2
(defun call-test ()
  (interactive)
  (test))

(defun test ()
  (dbg-msgbox 'test))

(global-set-key #\M-\2 'call-test)

;;;@@@ defalias
(defmacro defalias (alias orig)
  `(setf (symbol-function ,alias) (symbol-function ,orig)))

;;;@@@ string-double-quote
;;; ?Å˜???n?d?_?u???N?H?[?g?A?I?T
(defun string-double-quote (s)
  (let ((dq "\""))
    (concat dq s dq)))

;;;@@@ get-tmp-dir
(defun get-tmp-dir ()
  (truename (si:getenv "TEMP")))

;;;@@@ concat-path
(defun concat-path (path1 &rest path)
  (let ((ret (string-right-trim "/" (map-backslash-to-slash path1))))
    (dolist (p path ret)
      (setq ret (concat ret "/" (string-trim "/" (map-backslash-to-slash p)))))))

;;;@@@ rotate-list
;;; ???X?g?d?P?AÅÒnÅg]?ÅE?e
(defun rotate-list (l &optional reverse)
  (let ((out '()))
    (if reverse
        (progn
          (setq out (last l))
          (setq out (append out (butlast l))))
      (progn
        (setq out (cdr l))
        (setq out (append out (list (car l))))))
    out))

;;; @@@ looking-at ?I?a?uÅhA (?3?K?\?Å‚?A?a?u?}?b?`) XyzzyWiki ???aÅh /175 ?a?e
(defun looking-at-backward (regexp &optional case-fold)
  (save-excursion
    (save-restriction
      (narrow-to-region (point-min) (point))
      (goto-char (point-min))
      (scan-buffer (format nil "~A\\'" regexp)
                   :regexp t :case-fold case-fold))))

;;; @@@ msgbox for lisp debug
;;; usage: (dbg-msgbox args)
(defmacro dbg-msgbox (&rest vars)
  `(msgbox
    (concat ,@(make-list (length vars) :initial-element "~S\n"))
    ,@vars))

;;;@@@ funcall-region
;;; ?e?s?ÅC?AÅÒ??c?I?O?Åh?d?A?s?ÅE?e
;;; e.g. caller:    (funcall-region #'test 23 429)
;;;              or (funcall-region #'test)
;;;
;;;      callee: (defun test (beg end)
;;;                (msgbox "~A" (buffer-substring beg end)))
;;;
(defun funcall-region (func &optional from to)
  (save-excursion
    (unless from (setq from (progn (goto-char (point-min)) (point))))
    (unless to (setq to (progn (goto-char (point-max)) (point)))))
  (when (> from to)
    (rotatef from to))
  (let (out)
    (save-excursion
      (save-restriction
        (narrow-to-region from to)
        (goto-char from)
        (let ((end (progn (goto-eol) (point)))
              (beg (progn (goto-bol) (point))))
          (push (funcall func beg end) out))
        (while (forward-line)
          (let ((end (progn (goto-eol) (point)))
                (beg (progn (goto-bol) (point))))
            (push (funcall func beg end) out)))))
    out))

;;;
;;; trace
;;  http://www.geocities.jp/kiaswebsite/xyzzy/encap.html
;;;
; (trace function1 function2 ...) makes the functions `traced'.
; (trace) returns `traced' functions.
; (untrace function1 function2 ...) makes the functions `untraced'.
; (untrace) makes all `traced' functions `untraced'.

;(require "encap")

;(defvar *trace-function-list* nil)
;(defvar *trace-depth* 0)

;; mod
;(defun trace-encap (func)
;  (unless (encapsulated-p func 'traced-function)
;    (encapsulate func 'traced-function
;                 `(
;;?i??             ;(ed::setup-trace-output-buffer)
;                   (setq *trace-depth* (1+ *trace-depth*))
;;?I?X             ;(format *error-output* "~ACalling ~S~%" (make-sequence 'string *trace-depth* :initial-element #\SPC) (cons ',func argument-list))
;                   (format t "~ACalling ~S~%" (make-sequence 'string *trace-depth* :initial-element #\SPC) (cons ',func argument-list))
;                   (let ((#1=#:result (multiple-value-list (apply basic-definition argument-list))))
;;?I?X               ;(format *error-output* "~A~S returned~{ ~A~}~%" (make-sequence 'string *trace-depth* :initial-element #\SPC) ',func #1#)
;                     (format t "~A~S returned~{ ~A~}~%" (make-sequence 'string *trace-depth* :initial-element #\SPC) ',func #1#)
;                     (setq *trace-depth* (1- *trace-depth*))
;                     (values-list #1#))))
;    (push func *trace-function-list*)
;    func))
;
;(defun trace-unencap (func)
;  (when (encapsulated-p func 'traced-function)
;    (unencapsulate func 'traced-function)
;    (setq *trace-function-list* (remove func *trace-function-list* :test #'eq))
;    func))
;
;(defmacro trace (&rest args)
;  (setq *trace-depth* 0)        ; mod
;  (if (null args)
;      '*trace-function-list*
;    `(let (lst)
;       (dolist (func ',args (reverse lst))
;         (when (trace-encap func)
;           (setq lst (cons func lst)))))))
;
;(defmacro untrace (&rest args)
;  (if (null args)
;      '(let (lst)
;        (dolist (func *trace-function-list* lst)
;          (when (trace-unencap func)
;            (setq lst (cons func lst)))))
;    `(let (lst)
;       (dolist (func ',args (reverse lst))
;         (when (trace-unencap func)
;           (setq lst (cons func lst)))))))
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@@ eval-buffer mod
;;; ?J???Åg?g?o?b?t?@?d eval-buffer ?É ???Åë?I?E?A?Åë???Åë???t?@?C?????d?ÅE?c?e?e?B?A?É ???B
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *eval-buffer-function-orig* (function eval-buffer))

(defun eval-buffer (&optional buf)
  (interactive)
  (if buf
      (funcall *eval-buffer-function-orig* buf)
    (progn
      (funcall *eval-buffer-function-orig* (selected-buffer))
      (message (buffer-name (selected-buffer))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@@ mouse-wheel-scroll-keeping-position
;;; ?z?C?[???X?N???[???É ?A?a?|?C?Åg?g?d?A?Å·?e???ÅP?AÅfe?ÅE?e
;;; http://wiki.livedoor.jp/staygoldtak/d/xyzzy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *mouse-wheel-scroll-origin* nil)

(defun mouse-wheel-scroll-back-to-origin ()
  (goto-char *mouse-wheel-scroll-origin*)
  (setq *mouse-wheel-scroll-origin* nil)
  (delete-hook '*pre-command-hook* 'mouse-wheel-scroll-back-to-origin))

(defun mouse-wheel-scroll-keeping-position (window n lines)
  (unless lines
    (setq lines (window-lines window)))
  (with-selected-window
    (set-window window)
    (let* ((pos (or *mouse-wheel-scroll-origin*
                    (point))))
      (scroll-window (* n lines))
      (if (and (pos-visible-in-window-p pos)
               (save-excursion
                 (forward-line (1+ *scroll-margin*))
                 (pos-visible-in-window-p (point)))
               (save-excursion
                 (backward-line (1+ *scroll-margin*))
                 (pos-visible-in-window-p (point))))
          (goto-char pos)
        (unless *mouse-wheel-scroll-origin*
          (setq *mouse-wheel-scroll-origin* pos)
          (add-hook '*pre-command-hook* 'mouse-wheel-scroll-back-to-origin))))))

(setq mouse-wheel-handler #'mouse-wheel-scroll-keeping-position)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@@ revert-buffer mod
;;; ?I?X?I?Y?I?o?b?t?@?A?u?I?X?3?e?A?Åë?U?ÅE?BÅhj?u?É ?U?ÅE?c?v?d?Åë???Åë???ÅE?c?e?e?B?A?É ???B
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; orig: lisp/buffer.l

;; mod
(defun revert-buffer (&optional encoding)
  (interactive "0zEncoding: ")
  (let ((*expected-fileio-encoding* (or encoding *expected-fileio-encoding*)))
    (declare (special *expected-fileio-encoding*))
    (set-buffer-modified-p nil)     ; mod
    (if revert-buffer-function
    (funcall revert-buffer-function)
      (revert-buffer-normal)))
  (message "reverted.")         ; mod
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@@ indent-for-comment mod
;;; ?ÅE?A?E /* */?a? ?e?s?A C-; ?ÅE?e?A?J?[?\???a?e?A?a?e?E?ÅC?e?e?I?dFIX
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; lisp/cmds.l

;; add
(defun indent-for-comment-delete-trailing-spaces-at-line ()
  (save-excursion
    (let ((bol (progn (goto-bol) (point)))
          (eol (progn (goto-eol) (point))))
      (save-restriction
        (narrow-to-region bol eol)
        (goto-char (point-min))
        (replace-buffer "[ \t]+$" "" :regexp t)))))

;; add
(defun indent-for-comment-adjust-continuous-spaces-in-comment ()
  (save-excursion
    (let ((bol (progn (goto-bol) (point)))
          (eol (progn (goto-eol) (point)))
          (beg nil)
          (end nil))
      (save-restriction
        (narrow-to-region bol eol)
        (goto-char (point-min))
        (when (scan-buffer comment-start)
          (forward-char (length comment-start))
          (setq beg (point)))
        (when (scan-buffer comment-end)
          (setq end (point))))
      (when (and beg end)
        (save-restriction
          (narrow-to-region beg end)
          (goto-char (point-min))
          (replace-buffer "\\(^[ \t]+\\|[ \t]+$\\)" "" :regexp t)
          )))))

;; mod
(defun indent-for-comment ()
  (interactive "*")
  (unless (and comment-start comment-end comment-start-skip) ; mod
    (plain-error "No comment syntax defined"))
  (indent-for-comment-delete-trailing-spaces-at-line)        ; mod
  (indent-for-comment-adjust-continuous-spaces-in-comment)   ; mod
  (let (match-pos)
    (goto-bol)
    (if comment-start-skip-function
        (setq match-pos (funcall comment-start-skip-function))
      (let ((eol (save-excursion (goto-eol) (point))))
        (while (and (scan-buffer comment-start-skip
                                 :regexp t :tail t :limit eol)
                    (or (eq (parse-point-syntax (point)) :string)
                        (progn
                          (setq match-pos (point))
                          (save-restriction
                            (narrow-to-region (match-beginning 0) (point))
                            (skip-syntax-spec-backward " ")
                            (skip-syntax-spec-backward "^ "))
                          nil))))))
    (unless match-pos
      (goto-eol))
    (let ((opoint (point))                       ;
          (indent (save-excursion (funcall comment-indent-function))))
      (unless (= (current-column) indent)
        (delete-horizontal-spaces)
        (indent-to indent))
      (if match-pos
          (goto-char (+ match-pos (- (point) opoint)))
        (progn
          (insert comment-start)
          (save-excursion
            (insert comment-end)))))))

;; c-mode
(add-hook 'ed::*c-mode-hook*
          #'(lambda ()
              ;;                  orig "/\\(\\*+\\|/\\)[ \t]*"
              (setq comment-start-skip "/\\(\\*+\\|/\\)[ \t]")))

;; lisp-mode, lisp-interaction-mode
(add-hook 'ed::*lisp-mode-hook*
          #'(lambda ()
              ;;                  orig ";+[ \t]*"
              (setq comment-start-skip ";+[ \t]")))
(add-hook 'ed::*lisp-interaction-mode-hook*
          #'(lambda ()
              ;;                  orig ";+[ \t]*"
              (setq comment-start-skip ";+[ \t]")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@@ count-line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun count-line (from to)
  (interactive "*r")
  (let (lbeg lend lcnt wbeg wend wcnt cbeg cend ccnt)
    (save-excursion
      (setq lbeg (progn (goto-char from) (current-line-number)))
      (setq lend (progn (goto-char to) (current-line-number)))

      (setq wbeg (progn (goto-char from) 0)) ; fix it
      (setq wend (progn (goto-char to) 0))   ; fix it

      (setq cbeg (progn (goto-char from) (point)))
      (setq cend (progn (goto-char to) (point))))

    (setq lcnt (abs (- lbeg lend)))
    (setq wcnt (abs (- wbeg wend)))
    (setq ccnt (abs (- cbeg cend)))
    (if (interactive-p)
        (message "line: ~D, word: ~D, character: ~D" lcnt wcnt ccnt)
      (values lcnt wcnt ccnt))))

;; (defun count-line (from to)
;;   "return line count between point and mark."
;;   (interactive "r")
;;   (let (beg end cnt)
;;     (save-excursion
;;       (setq beg (progn (goto-char from) (current-line-number)))
;;       (setq end (progn (goto-char to) (current-line-number))))
;;     (setq cnt (abs (- beg end)))
;;     (if (interactive-p)
;;         (message "count line: ~D" cnt)
;;       cnt)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@@ todo-xyzzy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defvar todo-file "~/lisp/xyzzy-todo.txt")
;;
;; (defun todo-xyzzy-kill-buffer ()
;;   (interactive)
;;   (write-file todo-file t)
;;   (kill-buffer (selected-buffer)))
;;
;; (defun todo-xyzzy ()
;;   (interactive)
;;   ;; (if (> (count-windows) 1)
;;       ;; (find-file todo-file)
;;     ;; (find-file-other-window todo-file)))
;;   (find-file todo-file)
;;   (defvar *todo-map* nil)
;;   (unless *todo-map*
;;     (setq *todo-map* (make-sparse-keymap))
;;     (define-key *todo-map* '(#\C-x #\k) 'todo-xyzzy-kill-buffer))
;;   (use-keymap *todo-map*))

;; (defun todo-xyzzy ()
;;   (interactive)
;;   ;; (if (> (count-windows) 1)
;;       ;; (find-file todo-file)
;;     ;; (find-file-other-window todo-file)))
;;   (find-file todo-file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@@ double click --> copy-word-near-point
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun word-near-point ()
  (save-excursion
    (let ((end (progn (skip-syntax-spec-forward "w_") (point)))
          (beg (progn (skip-syntax-spec-backward "w_") (point))))
      (buffer-substring beg end))))

(defun copy-word-near-point ()
  (interactive)
  (let ((s (word-near-point)))
    (copy-to-clipboard s)
    (message "copied \"~A\"" s)))

(defun mouse-select ()
  "call from #\LBtnUp"
  (interactive)
  (continue-pre-selection)
  (if (= (mod *last-mouse-click-count* 6) 2)
      ;; double click
      (copy-word-near-point)
    ;; single click
    (when nil ;;(pre-selection-p) ;;?Z???N?V?Åˆ?Åg?\?|?a?A?|?e?I?A?3?o?E?É ?A?ÅN?-
        (let ((s (progn (selection-start-end (beg end)
                          (buffer-substring beg end)))))
          ;; (stop-selection)
          ;; (goto-last-mouse-point)
          (copy-to-clipboard s)
          (message "copied \"~A\"" s)))))

(global-set-key #\LBtnUp 'mouse-select)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@@ insert-paren
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; insert ()
;;;
(defun insert-paired-paren ()
  (interactive)
  (insert "()")
  (forward-char -1))

(global-set-key #\M-9 'insert-paired-paren)
;; (global-set-key #\( 'insert-paired-paren)

(defun insert-paired-bracket ()
  (interactive)
  (insert "[]")
  (forward-char -1))
(global-set-key #\[ 'insert-paired-bracket)

(defun insert-paired-brace ()
  (interactive)
  (insert "{}")
  (forward-char -1))
;; (global-set-key #\{ 'insert-paired-bracket)

;;;
;;; insert [] or {}
;;;
(defvar *insert-paired-bracket-state* nil)
(defun insert-paired-bracket ()
  (interactive)
  (let ((br "[]")
        (cbr "{}"))
    (when (eq *last-command* 'insert-paired-bracket)
      (forward-char -1)
      (delete-char 2))
    (if *insert-paired-bracket-state*
        (insert cbr)
      (insert br))
    (forward-char -1))
  (setq *insert-paired-bracket-state* (not *insert-paired-bracket-state*)))

(global-set-key #\M-\[ 'insert-paired-bracket)


;;;
;;; insert '' or ""
;;;
(defvar *insert-paired-quote-state* nil)
(defun insert-paired-quote ()
  (interactive)
  (let ((sq "\'\'")
        (dq"\"\""))
    (when (eq *last-command* 'insert-paired-quote)
      (forward-char -1)
      (delete-char 2))
    (if *insert-paired-quote-state*
        (insert dq)
      (insert sq))
    (forward-char -1))
  (setq *insert-paired-quote-state* (not *insert-paired-quote-state*)))

(global-set-key #\M-\' 'insert-paired-quote)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@@ indent-for-comment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun indent-for-comment-gnrr (&optional ARG)
  (interactive "p")
  (if ARG
      ;; set-comment-column
      (let ((col (current-column)))
        (setq comment-column col)
        (message "set comment-column to ~D." col))
    (indent-for-comment)))

(global-set-key #\C-\; 'indent-for-comment-gnrr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@@ indent-for-comment-region
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun indent-for-comment-region ()
  (interactive)
  (narrow-to-region (region-beginning) (region-end))
  (goto-char (point-min))
  (loop
    (end-of-line)
    (when (eq (parse-point-syntax) :comment)
      (indent-for-comment))
    (unless (next-line) (return)))
  (widen))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@@ use regexp or fixed string in isearch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun isearch-forward-gnrr (&optional re)
  (interactive "p")
  (setq *isearch-scanner-hook* #'(lambda (p) (setq ed::*isearch-regexp* (if re t nil)) p))
  (call-interactively 'isearch-forward))

(defun isearch-backward-gnrr (&optional re)
  (interactive "p")
  (setq *isearch-scanner-hook* #'(lambda (p) (setq ed::*isearch-regexp* (if re t nil)) p))
  (call-interactively 'isearch-backward))

(global-set-key '#\C-s 'isearch-forward-gnrr)
(global-set-key '#\C-r 'isearch-backward-gnrr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@@ use regexp or fixed string in query-replace
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun query-replace-mod (pattern replacement &optional noerror)
  (interactive "*sQuery replace: \nswith: "
    :default0 (word-near-point) :history0 'search
    :default1 (word-near-point) :history1 'search)
  (perform-replace pattern replacement t nil (interactive-p) noerror))

(defun query-replace-regexp-mod (regexp replacement &optional noerror)
  (interactive "*sQuery replace regexp: \nswith: "
    :default0 (word-near-point) :history0 'search
    :default1 (word-near-point) :history1 'search)
  (perform-replace regexp replacement t t (interactive-p) noerror))

(defun query-replace-gnrr (&optional re)
  (interactive "p")
  (if re
      (call-interactively 'query-replace-regexp-mod)
    (call-interactively 'query-replace-mod)))

(global-set-key '#\M-% 'query-replace-gnrr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@@ ?~?j?o?b?t?@?EÅgu?A???A?Å·IME(FEP)?dOFF?E?ÅE?e
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www2.ocn.ne.jp/~cheerful/script/xyzzy/library/buffer.html#ime-off-in-minibuf
;(provide "minibuffer")
;(in-package "editor")

(export '(*ime-mode-into-minibuffer*))
(defvar *ime-mode-into-minibuffer* nil)

(defun ime-state-get-and-setoff (bef-buffer file-name)
  (interactive)
  (setq *ime-mode-into-minibuffer* (get-ime-mode))
  (toggle-ime nil))

(defun ime-state-set (bef-buffer file-name)
  (interactive)
  (toggle-ime *ime-mode-into-minibuffer*))

(add-hook '*enter-minibuffer-hook* 'ime-state-get-and-setoff)
(add-hook '*exit-minibuffer-hook* 'ime-state-set)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@@ find-file-gnrr
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *find-file-read-only-exts* '("h" "c"))
(export '(*find-file-read-only-exts*))

(defvar find-file-orig (function ed::find-file)) ; ?3?I find-file ?O?Åh?dÅeTÅhd

(defun find-file-gnrr-internal (path &optional encoding nomsg)
  (if (member (pathname-type path) *find-file-read-only-exts* :test #'equal)
      (find-file-read-only path encoding nomsg)
    (funcall find-file-orig path encoding nomsg)))

(defun find-file (pathes &optional encoding nomsg)
  (interactive "FFind file: ")
  (unless (listp pathes)
    (setq pathes (list pathes)))
  (while pathes
    (if (file-exist-p (car pathes))
        (find-file-gnrr-internal (car pathes) encoding nomsg)
      (when (y-or-n-p "new file? ")
        (funcall find-file-orig (car pathes) encoding nomsg)))
    (setq pathes (cdr pathes))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@@ lazy-mark
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; for debug
(defun print-mark-list ()
  (interactive)
  (newline)
  (let ((l ed::*global-mark-list*))
    (while l
      (let* ((m (car l))
             (buf (marker-buffer m))
             (pt (marker-point m)))
        (insert (format nil "buf:~S pt:~S raw:~S\n" buf pt m)))
      (setq l (cdr l)))))

(defun repair-mark-list ()
  (let ((l ed::*global-mark-list*))
    (setq ed::*global-mark-list* '())
    (while l
      (let* ((m (car l)))
        (when (marker-buffer m)
          (setq ed::*global-mark-list* (append ed::*global-mark-list* (list m)))))
      (setq l (cdr l)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@@ han2zen
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun map-all-to-full-width-katakana-region (start end)
  "Åh??p -> ÅeS?p?i?ÅE?Å~?A?uÅ˜A -> ?J?i?v?j"
  ;; (interactive "*r")
  (ed::map-to-full-width-region start end
                                :ascii nil
                                :hiragana nil
                                :katakana t
                                :greek t
                                :cyrillic t))

(defun word-near-point-jp (pt)
  (save-excursion
    (goto-char pt)
    (cond ((skip-syntax-spec-forward " ")
           (values (point) pt))
        ((skip-syntax-spec-forward "^jkw_ ")
         (values (point) pt))
        (t
         (values (progn (forward-word 1) (point))
                 (progn (forward-word -1) (point)))))))

(defun han2zen ()
  (interactive)
  (multiple-value-bind (end beg) (word-near-point-jp (point))
    (map-all-to-full-width-katakana-region beg end)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@@ goto
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun goto (pos)
  "interactive version of goto-char"
  (interactive "npos:")
  (goto-char pos))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@@ narrow-to-function-c
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun narrow-to-function-c ()
  (interactive)
  (save-excursion
    (narrow-to-region-internal (progn
                                 (beginning-of-defun)
                                 (while (and (not (bobp))
                                             (looking-at "[^ \t/]"))
                                   (backward-line)
                                   (beginning-of-line))
                                 (while (looking-at "[ \t/]")
                                   (forward-line)
                                   (beginning-of-line))
                                 (point))
                               (progn (end-of-defun) (point))))
  (message "><"))

;; ;; http://www.hollenback.net/emacs/emacs.el
;; (defun narrow-to-function-c ()
;;   "Narrows to the function which the point is in."
;;   (interactive)
;;   (save-excursion
;;     (narrow-to-region-internal (progn (beginning-of-defun)
;;                                  (beginning-of-line)
;;                                  (while (not (or (looking-at "^[ \t]*$")
;;                                                  (bobp)))
;;                                    (forward-line -1))
;;                                  (if (looking-at "^[ \t]*$")
;;                                      (forward-line))
;;                                  (point))
;;                                (progn (end-of-defun) (point)))))

(global-set-key '(#\C-x #\n #\f) 'narrow-to-function-c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@@ step for debug
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setf *step-buffer-name* "*step*")

(defmacro step (form &environment env)
  `(let* ((buffer (get-buffer-create *step-buffer-name*))
          (stream (make-buffer-stream
                   buffer
                   (progn (set-buffer buffer)
                     (erase-buffer buffer)
                     (setup-temp-buffer buffer)
                     (point-max))))
          (i 0))
     (labels ((step2-apply-hook (fn args)
                (let ((values (multiple-value-list
                               (applyhook fn args nil #'step2-apply-hook))))
                  (format stream "~D (~A ~{ ~S~})~%  => ~{ ~S~}~%~%"
                          (setf i (1+ i)) fn args values)
                  (values-list values))))
       (prog1
           (evalhook ',form nil #'step2-apply-hook ',env)
         (pop-to-buffer buffer t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@@ lf-eol-code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun lf-eol-code ()
  (interactive)
  (let ((code-lf 0)
        (code-crlf 1)
        (code-cr 2))
    (unless (= (buffer-eol-code) code-lf)
      (setq buffer-read-only nil)
      (set-buffer-eol-code code-lf)
      (set-buffer-modified-p t)
      (message "changed EOL => LF"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@@ crlf-eol-code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun crlf-eol-code ()
  (interactive)
  (let ((code-lf 0)
        (code-crlf 1)
        (code-cr 2))
    (unless (= (buffer-eol-code) code-crlf)
      (setq buffer-read-only nil)
      (set-buffer-eol-code code-crlf)
      (set-buffer-modified-p t)
      (message "changed EOL => CRLF"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@@ beginning-of-defun-c
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar beginning-of-defun-c-pos nil)
(defvar beginning-of-defun-c-stat nil)
(defun beginning-of-defun-c ()
  (interactive)
  (let ((flist (funcall ed::build-summary-function))
        (l '())
        (pt (point))
        (found nil))
    (setq beginning-of-defun-c-stat (if (eq *last-command* 'beginning-of-defun-c) (not beginning-of-defun-c-stat) nil))
    (if beginning-of-defun-c-stat
        (goto-char beginning-of-defun-c-pos)
      (progn
        ;; get beginning/end of function's definition
        (while flist
          (let ((line (car (car flist)))
                (func-name (cadr (car flist)))
                (beg nil)
                (end nil))
            (save-excursion
              (goto-line line)
              (setq beg (point))
              (when (scan-buffer "{")
                ;; (setq beg (match-beginning 0))
                (setq end (progn
                            (goto-char (match-beginning 0))
                            (ed::goto-matched-parenthesis)
                            (point)))))
            (setq l (cons (list beg end func-name line) l)))
          (setq flist (cdr flist)))
        ;; find function's name
        (while l
          (let* ((f (car l))
                 (beg (car f))
                 (end (cadr f)))
            (when (and (numberp beg) (numberp end))
              (when (and (>= pt beg) (<= pt end))
                (setq found (list (nth 2 f) (nth 3 f)))))       ;; ("func-name" line-number)
            (setq l (cdr l))))

        (if found
            (progn
              (setq beginning-of-defun-c-pos pt)
              (goto-line (cadr found))
              (let ((func-name (substitute-string (car found) "\(.*\)" "()")))
                (message func-name)))
          (msgbox "not found function's definition"))))))

;; c-mode
(add-hook 'ed::*c-mode-hook*
          #'(lambda ()
              (define-key ed::*c-mode-map* #\M-\7 'beginning-of-defun-c)))
;; (global-set-key #\M-\` 'beginning-of-defun-c)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@@ pt
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun pt ()
  (interactive)
  (message "~D" (point)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@@ ?}?E?X???ÅP?A?R?s?[?E?y?[?X?g
;;; http://chez-sugi.net/xyzzy/xyzzy001.html
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun mouse-right-press ()
;;   (interactive)
;;   (mouse-left-press)
;;   (paste-from-clipboard))
;; (global-set-key #\LBtnUp 'copy-selection-to-clipboard)
;; (global-set-key #\RBtnUp 'mouse-right-press)
;; (global-set-key #\MBtnUp 'mouse-menu-popup)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@@ occur
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; moved --> .xyzzy
;; (defun occur ()
;;   (interactive)
;;   (call-interactively 'grep))
;;
;; (add-hook '*grep-hook* #'(lambda ()
;;                            (defvar *grep-map* nil)
;;                            (unless *grep-map*
;;                              (setq *grep-map* (make-sparse-keymap))
;;                              (define-key *grep-map* #\RET 'first-error-gnrr))
;;                            (use-keymap *grep-map*)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@@ copy-buffer-file-name
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun copy-buffer-file-name (&optional ARG)
  (interactive "p")
  (let ((fn nil)
        (ro buffer-read-only))
    (setq buffer-read-only nil)
    (setq fn (get-buffer-file-name))
    (unless ARG
      (setq fn (file-namestring fn)))
    (if fn
        (progn
          (copy-to-clipboard fn)
          (ed::kill-new fn)
          (message "~A" (concat "copied: " fn)))
      (message "~A" "no filename"))
    (setq buffer-read-only ro)))

(global-set-key '(#\C-x #\f) 'copy-buffer-file-name) ;     C-x f --> copy filename
                                                     ; C-u C-x f --> copy full path

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@@ beginning-of-buffer-no-mark, end-of-buffer-no-mark
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun beginning-of-buffer-no-mark ()
  (interactive)
  (goto-char (point-min)))

(defun end-of-buffer-no-mark ()
  (interactive)
  (goto-char (point-max)))

(global-set-key #\M-P 'beginning-of-buffer-no-mark)
(global-set-key #\M-N 'end-of-buffer-no-mark)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@@ toggle-narrowing-region
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar-local *narrowing-region-state* nil)

(defun narrow-to-region-internal (from to)
  (narrow-to-region from to)
  (setq *narrowing-region-state* t))

(defun widen-internal ()
  (widen)
  (setq *narrowing-region-state* nil))

(defun toggle-narrowing-region ()
  (interactive)
  (if *narrowing-region-state*
        (widen-internal)
    (let ((beg (mark t))
          (end (point)))
      (unless beg
        (setq beg 1))
      (narrow-to-region-internal beg end))))

(global-set-key '(#\C-x #\n #\n) 'toggle-narrowing-region)
(global-unset-key '(#\C-x #\n #\w))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@@ c(cmd)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun c ()
  (interactive)
  (let ((path (get-buffer-file-name)))
    (setq dir (if path (directory-namestring path) "/Users/4302"))
    (launch-application (concat "cmd.exe /K cd " dir))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@@ e(xplorer)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun remove-last-slash-or-backslash (dir)
  (let ((last (substring dir -1 (length dir))))
    (if (or (string= last "/")
            (string= last "\\"))
        (substring dir 0 (1- (length dir)))
      dir)))

(defun get-path-string-local (path)
  (let ((s path)
        (last (substring path -1 (length path)))
        (l ())
        (drv (pathname-device path))
        (out ""))
    (when (or (string= last "\\")
              (string= last "/"))
      (setq s (concat s "dummy")))
    (setq l (pathname-directory s))
    (when drv
      (setq out (concat drv ":")))
    (while l
      (setq out (concat out "\\" (car l)))
      (setq l (cdr l)))
    out))

(defun get-path-string-network (path)
  (let ((s path)
        (last (substring path -1 (length path)))
        (l ())
        (out "\\\\"))
    (when (or (string= last "\\")
              (string= last "/"))
      (setq s (concat s "dummy")))
    (setq out (concat out (pathname-host s)))
    (setq l (pathname-directory s))
    (while l
      (setq out (concat out "\\" (car l)))
      (setq l (cdr l)))
    out))

(defun local-file-p (path)
  (let((sf (substring path 0 2)))
    (if (or (string= sf "\\\\")
            (string= sf "//"))
        nil
      t)))

(defun e ()
  (interactive)
  (let ((cmd "explorer.exe ")
        (name (get-buffer-file-name))
        opt dir file)
  (if name
      (progn
        (if (local-file-p name)
            (setq dir (get-path-string-local name))
          (setq dir (get-path-string-network name)))
        (setq file (concat "\\" (file-namestring name)))
        (setq opt "/e,/select,"))
    (progn
      (setq opt "/e,")
      (setq dir (get-path-string-local (default-directory)))
      (setq file "")))
    (setq cmd (concat cmd opt dir file))
    (call-process cmd :wait nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@@ backward-delete-word-gnrr
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun char-in-str-p (ch s)
  (let ((nfound t)
        (pos 0)
        (end-pos (length s)))
    (while (and nfound
                (< pos end-pos))
      (when (eq (char s pos) ch)
        (setq nfound nil))
      (setq pos (1+ pos)))
    (not nfound)))

;;(defun alpha-l-p (ch)
;;  (char-in-str-p ch "abcdefghijklmnopqrstuvwxyz"))

;;(defun alpha-u-p (ch)
;;  (char-in-str-p ch "ABCDEFGHIJKLMNOPQRSTUVWXYZ"))

;;(defun number-p (ch)
;;  (char-in-str-p ch "0123456789"))

(defun alpha-num-p (ch)
  (let ((alpha-num (concat "0123456789"
                           "abcdefghijklmnopqrstuvwxyz"
                           "ABCDEFGHIJKLMNOPQRSTUVWXYZ")))
    (char-in-str-p ch alpha-num)))

;; (defun backward-delete-word-gnrr ()
;;   (interactive)
;;   (let ((i 0)
;;         (cha (char-after (point)))
;;         (chb (char-before (point)))
;;         (chbb (char-before (1- (point)))))
;;     (save-excursion
;;       (cond ((and (alpha-num-p cha) (alpha-num-p chb))
;;              ;(alpha-num-p cha) && (alpha-num-p chb)
;;              (while (alpha-num-p chb)
;;                (backward-char 1)
;;                (setq cha (char-after (point)))
;;                (setq chb (char-before (point)))
;;                (setq i (1+ i))))
;;             ((and (alpha-num-p cha) (not (alpha-num-p chb)))
;;              ;(alpha-num-p cha) && ! (alpha-num-p chb)
;;              (if (eq chbb chb)
;;                  (progn
;;                    (while (eq chbb chb)
;;                      (backward-char 1)
;;                      (setq chb (char-before (point)))
;;                      (setq chbb (char-before (1- (point))))
;;                      (setq i (1+ i)))
;;                    (backward-char 1)
;;                    (setq i (1+ i)))
;;                (while (not (alpha-num-p chb))
;;                  (backward-char 1)
;;                  (setq cha (char-after (point)))
;;                  (setq chb (char-before (point)))
;;                  (setq i (1+ i)))))
;;             ((and (not (alpha-num-p cha)) (alpha-num-p chb))
;;              ; ! (alpha-num-p cha) && (alpha-num-p chb)
;;              (while (alpha-num-p chb)
;;                (backward-char 1)
;;                (setq cha (char-after (point)))
;;                (setq chb (char-before (point)))
;;                (setq i (1+ i))))
;;             ((and (not (alpha-num-p cha)) (not (alpha-num-p chb)))
;;              ;! (alpha-num-p cha) && ! (alpha-num-p chb)
;;              (if (eq chbb chb)
;;                  (progn
;;                    (while (eq chbb chb)
;;                      (backward-char 1)
;;                      (setq chb (char-before (point)))
;;                      (setq chbb (char-before (1- (point))))
;;                      (setq i (1+ i)))
;;                    (backward-char 1)
;;                    (setq i (1+ i)))
;;                (while (not (alpha-num-p chb))
;;                  (backward-char 1)
;;                  (setq cha (char-after (point)))
;;                  (setq chb (char-before (point)))
;;                  (setq i (1+ i))))))
;;       (delete-char i))))
;;
;; (global-set-key #\M-h 'backward-delete-word-gnrr)
;;
;; use backward-delete-word-no-kill

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ bak
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun bak ()
  (interactive)
  (if (get-buffer-file-name)
      (call-interactively #'(lambda (name)
                              (interactive "sBAK as: " :default0 (concat
                                                                  (get-buffer-file-name)
                                                                  ".bak"))
                              (write-file name)))
    (call-interactively 'save-buffer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@ commentize-and-next-line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (require "emacs") ; for push-mark

;; (defun text-syntax-table-not-all (start end class)
;;   "start - end ?O?A?Asyntax-table?dÅf2?Å~?A?A?wÅfe?3?e??class?E?O?I?a?I?a
;; ?o?Å‚?ÅE?e?EÅfu?d?O?ÅE?B?ÅE?Å~?A?I?Å˜???aclass?E?cnil?d?O?ÅE?B"
;;   (let ((pt nil)
;;     (ret nil))
;;     (save-excursion
;;       (goto-char start)
;;       (while (and (< (point) end) (not pt))
;;     (setq ret (char-syntax (char-after)))
;;     (unless (= class ret)
;;       (setq pt (point)))
;;     (forward-char 1)))
;;     pt))

;; (defun text-syntax-table-not-all (start end)
;;   "start - end ?O?A?Asyntax-table?dÅf2?Å~?A?A?oÅhÅf?N?ÅÒ?X?E?O?I?a?I?a
;; ?o?Å‚?ÅE?e?EÅfu?d?O?ÅE?B?ÅE?Å~?A?I?Å˜???aclass?E?cnil?d?O?ÅE?B"
;;   (let ((pt nil)
;;     (ret nil))
;;     (save-excursion
;;       (goto-char start)
;;       (while (and (< (point) end) (not pt))
;;     (unless (syntax-whitespace-p (char-after (point)))
;;       (setq pt (point)))
;;     (forward-char 1)))
;;     pt))
;;
;; (defun text-face-not-all (start end name)
;;   "start - end ?O?A?A?wÅfe?3?e??name?d?U?n?A?Åë?E?Åëface?a?o?Å‚?ÅE?e?EÅfu?d?O?ÅE?B
;; ?ÅE?Å~?A?I?Å˜???Iface?aname?d?U?n?A?Åë?e?Inil?d?O?ÅE?B"
;;   (let ((pt nil)
;;     (faces ()))
;;     (save-excursion
;;       (goto-char start)
;;       (while (and (< (point) end) (not pt))
;; ;;    (setq faces (get-text-property (point) 'face))
;;     (setq faces (find-text-attribute 'face))
;;     (unless (listp faces)
;;       (setq faces (list faces)))
;; ;;  (unless (memq name faces)
;;     (unless (or (member name faces) (member 'font-lock-comment-delimiter-face faces))
;;       (setq pt (point)))
;;     (forward-char 1)))
;;     pt))
;;
;; (defun whole-comment-line-p ()
;;   "?J???Åg?g?s?a?ÅE?Å~?A?R???Åg?g?E?ct, ?Å‚???A?E?ÅP?e?Inil?d?O?ÅE?B
;; ?e?Åh?R???Åg?g?a?U?U?e?A?Åë?e?e?Åˆ?anil?d?O?ÅE?B?o?s?I?e?Åˆ?anil?d?O?ÅE?B"
;;   (let (start end nc)
;;     (save-excursion
;;       (end-of-line)
;;       (setq end (point))
;;       (beginning-of-line)
;;       (setq start (point)))
;;     (if (< start end)
;;       (progn
;;     ;; ÅhÅf?Å˜???N?ÅÒ?X?E?O?a?A?ÅÒ?E?o?Å‚?ÅE?e?EÅfu?dÅfT?ÅE?B
;;     (setq start (text-syntax-table-not-all start end))
;;     (if start
;;         (setq nc (text-face-not-all start end 'font-lock-comment-face))
;;       (setq nc nil))
;;     (when (integerp nc)
;;       (setq nc (text-face-not-all start end 'font-lock-comment-delimiter-face)))
;;     (if (integerp nc)
;;         nil
;;       t))
;;       nil)))
;;
;; (defun commentize-and-next-line (&optional ARG)
;;   "?J???Åg?g?s?d?R???Åg?gÅÒÅ‚?^?A?Åg?R???Åg?gÅÒÅ‚?É ?A???I?s?O?UÅgR?ÅE?e?B
;; C-u?E?a?eÅeOÅfu?o?Åh?d?g???A???I?s?E?UÅgR?É ?E?Åë?B
;; ?Åı?[?U?I?Åh commentize-and-next-line-set-mark ?a nil?E?O?I?A?Å·?I
;; ?R???Åg?gÅÒÅ‚?ÅE?e?A?ÅÒ?I?s?d?}?[?N?ÅE?e?B?i?f?t?H???g?j"
;;   (interactive "P")
;;   (let (beg end)
;;     (save-excursion
;;       (beginning-of-line)
;;       (setq beg (point))
;;       (end-of-line)
;;       (setq end (point)))
;;     (when (< beg end)
;;       (if (whole-comment-line-p)
;;       (uncomment-region beg end)
;;     (comment-out-region beg end))))
;;   (unless ARG
;; ;;     (when (and commentize-and-next-line-set-mark
;; ;;            (not (eq *last-command* 'commentize-and-next-line)))
;; ;;       (push-mark))
;;     (forward-line 1)
;;     (beginning-of-line)))
;;
;; (global-set-key #\M-\; 'commentize-and-next-line)
;;
;; (defvar commentize-and-next-line-set-mark t
;;   "*nil?E?O?I?A?Å·?I?A?R???Åg?gÅÒÅ‚?ÅE?e?A?ÅÒ?I?EÅfu?d?}?[?N?ÅE?e?B
;; C-x C-x (exchange-point-and-mark) Åg??A?O???B")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@@ toggle-one-line-comment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 1 ?s?R???Åg?g?d?g?O??
;; http://hie.s64.xrea.com/xyzzy/note/edit.html#toggle-one-line-comment
;;
(defvar *one-line-comment-alist*
  '((awk-mode   . ("# "))
    (c-mode   . ("/* " " */"))
    (c++-mode   . ("// "))
    (css-mode   . ("/* " " */"))
    (html+-mode . ("<!-- " " -->"))
    (lisp-mode  . (";; "))
    (lisp-interaction-mode . (";; "))
    (py-mode  . ("# ")) ;; ??ÅeO?a?OÅgE?ÅE?e???s?E?E?E?a?o?e
    (perl-mode  . ("# "))
    (php-mode   . ("// "))
    (sql-mode   . ("-- "))
    (make-mode   . ("# "))
    (ruby-mode   . ("# "))
    ))

(defun toggle-one-line-comment ()
  (interactive)
  (let ((li (cdr (assoc buffer-mode *one-line-comment-alist*)))
        bol eol str keyreg)
    (when li
      (save-excursion
        (goto-eol)
        (setq eol (point))
        (back-to-indentation)
        (setq bol (point))
        (setq str (buffer-substring bol eol))
        (if (= (length li) 1)
            (let ((key (car li)))
              (setq keyreg (format nil "^~A+[ \t]*" (regexp-quote key)))
              (if (string-match keyreg str)
                  (delete-region (+ bol (match-beginning 0))
                                 (+ bol (match-end 0)))
                (progn
                  (back-to-indentation) (insert key))))
          (let ((key1 (car li))
                (key2 (cadr li)))
            (setq keyreg (format nil
                                 "^\\(~A\\)+[ \t]*\\(.*\\)[ \t]*\\(~A\\)+$"
                                 (regexp-quote key1)
                                 (regexp-quote key2)))
            (if (string-match keyreg str)
                (progn
                  (setq str (string-replace-match str "\\2"))
                  (delete-region (+ bol (match-beginning 0))
                                 (+ bol (match-end 0)))
                  (insert str))
              (progn
                (back-to-indentation) (insert key1)
                (goto-eol) (insert key2)))))))))

;; add
(defun toggle-comment-and-nextline ()
  (interactive)
  (toggle-one-line-comment)
  (forward-line 1))

(global-set-key #\M-\; 'toggle-comment-and-nextline)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;@@@ my-just-one-space
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar my-just-one-space-state t)
(make-local-variable 'my-just-one-space-state)

(defun my-just-one-space ()
 (interactive)
  (if (and (eq *last-command* 'my-just-one-space)
           (eq my-just-one-space-state nil))
      (delete-backward-char 1)
    (just-one-space))
  (setq my-just-one-space-state (not my-just-one-space-state)))

 (global-set-key #\M-SPC  'my-just-one-space)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@@ next-page-gnrr, previous-page-gnrr
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun next-page-gnrr ()
  (interactive)
  (let ((end-line (+ (get-window-start-line) (window-lines))))
    (next-page)
    (when (> end-line (buffer-lines))
      (goto-char (point-max)))))

(global-set-key #\M-n 'next-page-gnrr)


(defun previous-page-gnrr ()
  (interactive)
  (let ((start-line (get-window-start-line)))
    (previous-page)
    (when (= start-line 1)
      (goto-char (point-min)))))

(global-set-key #\M-p 'previous-page-gnrr)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@@ comment-region
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun comment-region ()
  (interactive)
  (comment-out-region))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@@ exit-from-dos-shell
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (defun exit-from-dos-shell ()
  ;; (interactive)
  ;; (goto-char (point-max))
  ;; (insert "\n")
  ;; (insert "exit\n")
  ;; (kill-buffer (selected-buffer)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@@ toggle-truncate-lines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defun toggle-buffer-fold-type (&optional dir)
;http://xyzzy.s53.xrea.com/wiki/index.php?tips%2F%A5%D0%A5%C3%A5%D5%A5%A1%A4%CE%C9%BD%BC%A8%A4%F2%A5%C8%A5%B0%A5%EB
(defun toggle-truncate-lines (&optional dir)
  (interactive "p")
  (let* ((table `((nil                . "?U?e?O?É ?F ?3?É ")
                  (t                  . "?U?e?O?É ?F ?E?B?Åg?h?E??")
;                 (,buffer-fold-width . "?wÅfe?EÅfu?A?U?e?O?É ")))
                  ))
         (current (nth (mod
                        (+(position (buffer-fold-width) table
                                    :test (lambda (x y) (eql x (car y))))(if dir 1 -1))
                        (length table))
                       table)))
    (set-buffer-fold-width (car current))
    (message (cdr current))))

;; (defvar toggle-truncate-lines-p nil)
;; (make-variable-buffer-local ' toggle-truncate-lines-p)
;; (defun toggle-truncate-lines ()
;;   (interactive)
;;   (if toggle-truncate-lines-p
;;    (set-buffer-fold-type-none)
;;  (set-buffer-fold-type-window))
;;   (setq toggle-truncate-lines-p (not toggle-truncate-lines-p))
;;   )

(global-set-key '(#\C-x #\t) 'toggle-truncate-lines) ;; C-x t

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@@ isearch-yank-word-with-mark
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun isearch-yank-word-with-mark ()
  (interactive)
  (let (beg)
    (if (eq *last-command* 'isearch-yank-word-with-mark)
        (progn
          (isearch-yank-word))
      (progn
        (setq beg (point))
        (isearch-yank-word)
        (funcall 'copy-region-as-kill beg (point))))))

(define-key *isearch-map* #\C-w 'isearch-yank-word-with-mark)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@@ backward-delete-word-no-kill
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun backward-delete-word-no-kill ()
  (interactive)
  (let ((end (point))
        beg len)
    (save-excursion
      (backward-word)
      (setq beg (point)))
    (setq len (- end beg))
    (backward-delete-char-untabify len)))

(global-set-key #\M-h 'backward-delete-word-no-kill)

;; for minibuffer
(define-key minibuffer-local-must-match-map #\M-h 'backward-delete-word-no-kill)
(define-key minibuffer-local-completion-map #\M-h 'backward-delete-word-no-kill)
(define-key minibuffer-local-map #\M-h 'backward-delete-word-no-kill)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@@ kill-word-no-kill
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun kill-word-no-kill ()
  (interactive)
  (let ((beg (point))
        end len)
    (save-excursion
      (forward-word)
      (setq end (point)))
    (setq len (- end beg))
    (delete-char len)))

(global-set-key #\M-d 'kill-word-no-kill)

;; for minibuffer
(define-key minibuffer-local-must-match-map #\M-d 'kill-word-no-kill)
(define-key minibuffer-local-completion-map #\M-d 'kill-word-no-kill)
(define-key minibuffer-local-map #\M-d 'kill-word-no-kill)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@@ kill-shell-process
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar kill-shell-process-buffers '("\*Command Output\*" "\*Shell\*" "Python"))
(defun kill-shell-process ()
  (interactive)
  (let ((buf (buffer-name (selected-buffer)))
		(bns kill-shell-process-buffers))
	(dolist (b bns)
	  (when (string-match b buf)
		(kill-process (buffer-process))
		(setq buf nil)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@@  copy-word
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; not yet
;; (defun copy-word ()
;;   (interactive)
;;   (mark-word)
;;   (copy-region-as-kill))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;@@@ adjust-spaces
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ;; not yet
;; (defun adjust-spaces ()
;;   (interactive)
;;   (save-excursion
;;     (while (= (following-char) #\SPC )
;;       (delete-char 1))))
;;
;; ;(global-set-key '(#\C-#\SPC) 'adjust-spaces)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; @@@ my-kill-buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; C-u C-x k     kill all buffer
;;     C-x k     kill selected buffer
(defvar *my-kill-buffer-excludes* '("*scratch*"))
(defvar *my-kill-buffer-window-close-list* '("*Help*" "*recentf*"))

(defun count-visible-buffer ()
  (let ((bufs (mapcar #'buffer-name (buffer-list)))
        (n 0))
    (while bufs
      (unless (string= (car bufs)  " " :start1 0 :end1 1)
        (setq n (1+ n)))
      (setq bufs (cdr bufs)))
    n))

(defun my-kill-buffer (&optional ARG)
  (interactive "p")
  (if (not ARG)
      (let ((buf (selected-buffer)))
        (when (and (> (count-windows) 1)
                   (member (buffer-name buf) *my-kill-buffer-window-close-list* :test #'equal))
          (delete-window))
        (kill-buffer buf))
    (let ((buf-names (mapcar #'buffer-name (buffer-list)))
          name)
      (while buf-names
        (setq name (car buf-names))
        (unless (member name *my-kill-buffer-excludes* :test #'equal)
          (delete-buffer name))
        (setq buf-names (cdr buf-names)))))
  (when (> 2 (count-visible-buffer))
    (delete-other-windows)))

(global-set-key '(#\C-x #\k) 'my-kill-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; @@@ my-switch-to-buffer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-switch-to-buffer ()
  (interactive)
  (bury-buffer))

(global-set-key '(#\C-x #\b) 'my-switch-to-buffer)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; @@@ my-undo-redo
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-undo-redo (&optional ARG)
  "This function invocates either undo or redo according to ARG.
When ARG is nil, undo is called.
Otherwise,  ARG is t, redo is called."
  (interactive "*P")
  (if ARG
      (redo)
    (undo)))

(global-set-key #\C-z 'my-undo-redo)

;; undo ?a?I?J?[?\???EÅfu?I?ÅgÅgR?d Emacs ??(?)?E?ÅE?e
;; http://seaoak.cocolog-nifty.com/read/2005/10/xyzzy__51de.html
(setq *move-forward-after-undo-deletion* t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; @@@ previous-page , next-page
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(defun my-previous-page ()
;  (interactive)
;  (let ((pt_old (point)))
;   (if (and (eq *last-command* 'my-previous-page) (= (point) pt-old))
;       (beginning-of-buffer)
;     (previous-page))))
;
;(defun my-next-page ()
;  (interactive)
;  (let ((pt_old (point))
;       (pt_new (point-max)))
;   (save-excursion
;     (next-page)
;     (setq pt-new (point)))
;   (if (and (eq *last-command* 'my-next-page) (= pt_new pt_old))
;       (progn
;         (goto-char pt_new)
;         (message "!!"))
;     (next-page))))
;
;
;(global-set-key #\M-p 'my-previous-page)
;(global-set-key #\M-n 'my-next-page)
;(global-set-key #\M-p 'previous-page)
;(global-set-key #\M-n 'next-page)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; @@@ duplicate-line
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-duplicate-line ()
  "?Å‚?Y?s?dÅgn?dÅÒÅ‚?É ?U?ÅE"
  (interactive)
  (let (line a z f)
    (save-excursion
      (goto-bol)
      (setq a (point))
      (goto-eol)
      (setq f (eobp))
      (forward-line)
      (setq z (point))
      (setq line (buffer-substring a z))
      (if f (insert "\n"))
      (insert line)
      )))

(global-set-key #\M-= 'my-duplicate-line)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; @@@ shift-[rl]-region-hold
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun shift-r-region-hold()
  (interactive "*")
  (save-excursion
    (if (mark)
        (shift-region (region-beginning) (region-end) 1)
      )))

(defun shift-l-region-hold()
  (interactive "*")
  (save-excursion
    (if (mark)
    (unshift-region (region-beginning) (region-end) 1)
      )))

(global-set-key #\M-\\ 'shift-r-region-hold)  ; M-\
(global-set-key #\M-^  'shift-l-region-hold)  ; M-^

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; @@@ color isearch
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(require "csearch")
; (in-package "editor")
; (defvar *csearch-buffer-colors* #(0 #xe0ffe0))
; (defun search-continuously (pattern reverse regexp interactive-p noerror)
;   (set-buffer-colors *csearch-buffer-colors*)
;   (while (search-command pattern reverse regexp interactive-p t)
;     (unless (pos-visible-in-window-p (point))
;       (recenter))
;     (refresh-screen)
;     (loop
;       (let ((def (lookup-keymap *continuous-search-map* (read-char *keyboard*) t)))
;       (cond ((eq def 'next)
;              (return))
;             ((eq def 'quit)
;              (hide-match)
;              (set-buffer-colors nil)
;              (quit))
;             ((eq def 'backward)
;              (setq reverse t)
;              (return))
;             ((eq def 'forward)
;              (setq reverse nil)
;              (return))
;             ((eq def 'recenter)
;              (recenter)
;              (refresh-screen))
;             (t
;              (message "(RET)?pÅeÅ}, (C-g)ÅfÅı?~, (s)ÅeO?O, (r)?a?e?O"))))))
;   (hide-match)
;   (set-buffer-colors nil)
;   (or noerror
;       (plain-error "?Å˜???n?a?c?A?c?e?U?1?n")))
; (in-package "user")


;;;
;;; hatena-insert
;;;
(defvar *hatena-insert-alist* '((">|lisp|\n||<"  . "ÉXÅ[ÉpÅ[ pre lisp")
                                ;(">||\n||<"      . "ÉXÅ[ÉpÅ[ pre")
                                ("[:title=ÉRÉR]" . "ÉäÉìÉN")
                                ("<br>"          . "â¸çs")
                                ("(())"          . "ãríç")
                                (">>\n<<"        . "à¯óp")
                                ;("-"             . "ÉäÉXÉg")
                                ;("+"             . "ÉäÉXÉg î‘çÜÇ¬Ç´")
                                ("**"            . "è¨å©èoÇµ")
                                ("><hr><"        . "ãÊêÿÇËê¸")))

(defvar *hatena-insert-last-num* 0)

(defun hatena-insert ()
  (interactive)
  (let ((lst *hatena-insert-alist*))
    (setq *hatena-insert-last-num*
          (if (eq *last-command* 'hatena-insert)
              (progn
                (delete-char (1+ (length (car (nth *hatena-insert-last-num* lst)))))
                ;(delete-char (length (car (nth *hatena-insert-last-num* lst))))
                (mod (incf *hatena-insert-last-num*) (length *hatena-insert-alist*)))
            0))
    (let ((e (nth *hatena-insert-last-num* lst)))
      (save-excursion (insert (car e) "\n"))
      ;(save-excursion (car e))
      (message "~D/~D ~A" (1+ *hatena-insert-last-num*) (length *hatena-insert-alist*)
               (cdr e)))))

(global-set-key #\M-3 'hatena-insert)



;;
;; discrete.l ends here
;;
