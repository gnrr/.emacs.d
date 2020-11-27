;; またこんど
(require 'posframe)

(defvar eltt/info-list '(eltt/info-point eltt/info-overlays eltt/info-face-properties))
(setq eltt/info-list '(eltt/info-point eltt/info-overlays eltt/info-face-properties))
;; (defvar eltt/info-list '(eltt/info-point))

(defun eltt/info-point ()
  (format "Point: %S" pt))

(defun eltt/info-line-column ()
  (format "Line, Col %S" pt))

(defun eltt/info-overlays ()
  (mapconcat #'(lambda (ov) (pp-properties (overlay-properties ov))) (overlays-at pt) "\n"))

(defun eltt/info-face-properties () ;; fixme
  (let ((faces (face-at))
        (s ""))
    (dolist (f faces)
      (setq s (concat s "Face: " (symbol-name f) "\n"))
      (setq s (concat s (mapconcat #'(lambda (x) (format " %-20S%S" (car x) (cdr x))) (face-all-attributes f) "\n"))))
    s))
;;----

(defvar eltt/posframe-buf-name " *eltt/posframe*")
(defvar pt 1)

(defun eltt/posframe-hookee ()
  (let ((pt (point)))
    (with-current-buffer eltt/posframe-buf-name
      (erase-buffer)
      (insert (mapconcat #'(lambda (x) (funcall x)) eltt/info-list "\n"))
      (posframe-refresh eltt/posframe-buf-name))))

(defun eltt/posframe-show ()
  (let ((buf (mapconcat #'(lambda (x) (funcall x)) eltt/info-list "\n")))
    (posframe-show eltt/posframe-buf-name
                   :string buf
                   :position '(1000 . 50)
                   :internal-border-width 1
                   :internal-border-color "#555"
                   :handler 'posframe-poshandler-frame-top-left-corner)))

(defun eltt/posframe-delete ()
  (posframe-delete eltt/posframe-buf-name))
;;----

(defvar elisp-tooltip nil)
(defun elisp-tooltip ()
  (interactive)
  (setq elisp-tooltip (not elisp-tooltip))
  (cond (elisp-tooltip (eltt/posframe-show)
                       (add-hook 'post-command-hook #'eltt/posframe-hookee))
        (t (remove-hook 'post-command-hook #'eltt/posframe-hookee)
           (eltt/posframe-delete))))

(define-key evil-normal-state-map (kbd "1 1") #'elisp-tooltip)

(provide 'elisp-tooltip)

   ;; (defvar buf " *test*")
   ;; (posframe-show buf)

   ;; (with-current-buffer buf
   ;;   (erase-buffer)
   ;;   (insert "ffffffffffffff")
   ;;   (posframe-refresh buf))

(posframe-delete-all)

(defun pp-properties (plist)
  (let ((s "")
        (name nil))
    (mapc #'(lambda (x)
              (if (null name)
                  (setq name x)
                (setq s (format "%s  %-10s %s\n" s name x))
                (setq name nil)))
          plist)
    s))

(pp-properties '(hoge 2 fuga 44 egg 55))

(defun my-ov ()
  (interactive)
  (let ((pt (point)))
    (message (pp-properties (mapcar #'overlay-properties (overlays-at pt))))))

(message "%s" (mapcar #'overlay-properties (overlays-at (pt))))
(define-key evil-normal-state-map (kbd "1 2") #'my-ov)

(defun my-ovk ()
  (interactive)
  (let ((pt (point)))
    ;; (message "%s" (mapconcat #'(lambda (ov) (pp-properties (overlay-properties ov))) (overlays-at pt) "\n"))))
    (message (mapconcat #'(lambda (ov) (pp-properties (overlay-properties ov))) (overlays-at pt) "\n"))))

(define-key evil-normal-state-map (kbd "1 9") #'my-ovk)
;--------

;; mod face-at-point
(defun face-at ()
  (let (faces)
    ;; Add the named faces that the `read-face-name' or `face' property uses.
    (let ((faceprop (or (get-char-property pt 'read-face-name)
                        (get-char-property pt 'face))))
      (cond ((facep faceprop)
             (push faceprop faces))
            ((face-list-p faceprop)
             (dolist (face faceprop)
               (if (facep face)
                   (push face faces))))))
    (if multiple
        (delete-dups (nreverse faces))
      (car (last faces)))))


;(#<overlay from 2317 to 2371 in elisp-tooltip.el> #<overlay from 2317 to 2318 in elisp-tooltip.el> #<overlay from 2317 to 2318 in elisp-tooltip.el>)
