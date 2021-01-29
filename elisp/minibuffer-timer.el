;;; minibuffer-timer.el --- Timer in mode-line -*- lexical-binding: t; -*-

;;; Commentary:

;; minibuffer-timer.el provides showing timer in minibuffer (echo area)
;;
;; This code is fully based on `mode-line-timer.el' by shohex. Thanks!
;;
;; Start timer
;;   M-x minibuffer-timer-start
;;
;; Stop timer
;;   M-x minibuffer-timer-stop
;;
;; Optional
;; (add-hook 'minibuffer-timer-expired-hook '(lambda () (message "Expired!!")))

(defvar minibuffer-timer-expired-hook nil
  "Hook run after timer expired.")

(defvar minibuffer-timer--timer nil)
(defvar minibuffer-timer--remainder-seconds 0)

(defun minibuffer-timer--time-string ()
  (let ((seconds minibuffer-timer--remainder-seconds))
    (format "%02d:%02d" (/ seconds 60) (mod seconds 60))))

(defun minibuffer-timer--tick ()
  (let ((seconds (1- minibuffer-timer--remainder-seconds)))
    (if (< seconds 0)
        (progn
          (minibuffer-timer-stop)
          (run-hooks 'minibuffer-timer-expired-hook))
      (setq minibuffer-timer--remainder-seconds seconds)
      (unless (window-minibuffer-p)
        (message (minibuffer-timer--time-string))))))

(defun minibuffer-timer--set-remainder-second (minutes)
  (setq minibuffer-timer--remainder-seconds (* 60 minutes)))

(defun minibuffer-timer-start (&optional minutes)
  (interactive)
  (if minibuffer-timer--timer (error "Already running timer")
    (unless minutes
      (setq minutes (read-number "Minutes: " 3)))
    (minibuffer-timer--set-remainder-second minutes)
    (setq minibuffer-timer--timer (run-with-timer 0 1 #'minibuffer-timer--tick))))

(defun minibuffer-timer-start-force (&optional minutes)
  (interactive)
  (when minibuffer-timer--timer
    (cancel-timer minibuffer-timer--timer)
    (setq minibuffer-timer--timer nil))
  (minibuffer-timer-start minutes))

(defun minibuffer-timer-stop ()
  (interactive)
  (if minibuffer-timer--timer
      (progn
        (cancel-timer minibuffer-timer--timer)
        (setq minibuffer-timer--timer nil))
    (when (interactive-p)
      (error "No running timer"))))

(provide 'minibuffer-timer)

;;; mibibuffer-timer.el ends here
