
(require 'pcre2el)

(defun eshell-are-you-there ()
  (print "eshell.el is here."))


(defun eshell-get-buffer-list ()
  (mapconcat #'identity (seq-map #'buffer-name (buffer-list)) "\n"))



(defun eshell-create-shell-buffer (name)
  "Invoke shell test"
  (interactive "MName of shell buffer to create: ")
   (pop-to-buffer (get-buffer-create (generate-new-buffer-name name)))
   (shell (current-buffer))
   	(remove-hook 'comint-output-filter-functions
				 'comint-watch-for-password-prompt))





;; (defun eshell-delete-shell-buffer (buffer-name)
;;   (let ((buffer (get-buffer buffer-name)))
;; 	(save-excursion
;; 	  (set-buffer buffer)
;; 	  (set-buffer-modified-p nil) ; set modified state of the buffer to nil
;; 	  (let ((p (get-buffer-process (current-buffer))))
;; 		(if p (kill-buffer p))) ; kill the process attached to the buffer
;; 	  (kill-buffer buffer-name))))


;; https://emacs.stackexchange.com/questions/24330/have-a-function-to-disable-close-confirmation-on-terms-work-on-all-terms-but-sh

(defun eshell-delete-shell-buffer (buffer-name)
  (kill-buffer buffer-name))

(defun set-no-process-query-on-exit ()
  (let ((proc (get-buffer-process (current-buffer))))
	(when (processp proc)
	       (set-process-query-on-exit-flag proc nil))))

(add-hook 'shell-mode-hook 'set-no-process-query-on-exit)





(defun eshell-get-prompt-pos-list (buffer pattern result count)
  (with-current-buffer buffer
	(if (>= (length result) count)
		result
	  (if (eq (length result) 0)
		  (eshell-get-prompt-pos-list
		   buffer
		   pattern
		   (cons (point-max) result)
		   count)
		(eshell-get-prompt-pos-list
		 buffer
		 pattern
		 (cons
		  (eshell-goto-prev-prompt buffer pattern)
		  result)
		 count)))))



(defun eshell-goto-prev-prompt (buffer pattern)
  (with-current-buffer buffer
    ;; (goto-char (point-max))
	(let ((current-line (count-lines (point-min) (point))))
	  (if (> current-line 2)
		  (progn
	;;	   (move-to-window-prev-line 2)
		   (re-search-backward (rxt-pcre-to-elisp pattern)))
		(goto-char (point-min))))))



(defun eshell-get-buffer-substring-within-range (buffer pmin pmax)
  (with-current-buffer buffer
	  (buffer-substring-no-properties pmin pmax)))






(defun eshell-what-line (buffer)
  (with-current-buffer buffer
	(what-line)))


(defun eshell-point (buffer)
  (with-current-buffer buffer
	(point)))



;; (defun eshell-send-command (buffer command)
;;   (with-current-buffer buffer
;;     (goto-char (point-max))
;;     (insert command)
;;     (comint-send-input)))


(defun eshell-send-ctrl-D (buffer)
  (with-current-buffer buffer
    (goto-char (point-max))
	(comint-delchar-or-mayb-eof)))


(defun eshell-send-ctrl-C (buffer)
  (with-current-buffer buffer
    (goto-char (point-max))
	(comint-interrupt-subjobs)))




(defun eshell-hide-password (buffer password point)
  (with-current-buffer buffer
	(goto-char point)
	(word-search-forward password nil t)
	  (replace-match "*****")))
  



(defun eshell-send-command (buffer password)
  (with-current-buffer buffer
	;; (remove-hook 'comint-output-filter-functions
	;; 			 'comint-watch-for-password-prompt)
    (goto-char (point-max))
	(let ((p (point)))
	  (insert password)
	  (comint-send-input)
	  p)))



  



;; (defun eshell-send-password (buffer password)
;;   ;;(interactive "Mpassword: ")
;;   ;;(with-current-buffer buffer
;;     ;;(select-window (active-minibuffer-window))
;; 	;;(send-invisible) ;; goes to the minibuffer for imputting a password.
;; 	(run-with-timer 1.0 nil 'execute-kbd-macro (kbd (eval password)))
;;     ;;(run-with-timer .2 nil 'insert password)
;;     (run-with-timer 0.5 nil 'execute-kbd-macro (kbd "RET")))
;; ;;)



(defun eshell-get-buffer-string (buffer)
  (with-current-buffer buffer
    (buffer-substring-no-properties 1 (point-max))))


(defun eshell-get-buffer-tail500 (buffer)
  (with-current-buffer buffer
	(goto-char (point-max))
	(let ((pmin (max (- (point-max) 500) 1))
		  (pmax (point-max)))
	  (buffer-substring-no-properties pmin pmax))))



(defun eshell-get-buffer-tail (buffer len)
  (with-current-buffer buffer
	(goto-char (point-max))
	(let ((pmin (max (- (point-max) len) 1))
		  (pmax (point-max)))
	  (buffer-substring-no-properties pmin pmax))))



(defun eshell-get-num-of-lines (buffer)
  (with-current-buffer buffer
      (let ((pmin (max (- (point-max) len) 1))
	    (pmax (point-max)))
	(count-lines pmin pmax))))


(defun eshell-get-num-of-chars (buffer)
  (with-current-buffer buffer
    (point-max)))




(defun eshell-goto-point-max (buffer)
  (with-current-buffer buffer
	(goto-char (point-max))))


(defun eshell-goto-point-min (buffer)
  (with-current-buffer buffer
	(goto-char (point-min))))


(defun eshell-goto-point (buffer point)
  (with-current-buffer buffer
	(goto-char point)))



;; (defun eshell-send-string-to-minibuffer (string)
;;   ;;(interactive "Mpassword: ")
;;   (select-window (active-minibuffer-window))
;;    (run-with-timer .2 nil 'insert password)
;;   (run-with-timer .2 nil 'execute-kbd-macro (kbd string))
;;   (run-with-timer .3 nil 'execute-kbd-macro (kbd "RET")))



(defun eshell-get-minibuffer-content ()
  (progn 
    (select-window (active-minibuffer-window))
    (minibuffer-contents)))



 
;;; Proper setting of [UP] and [DOWN] keys.
(defun move-to-window-prev-line (arg)
  "move window previous line"
  (interactive "p")
  (progn
    (if (equal arg nil)
	(setq arg 1))
    (while (< 0 arg)
      (let ((cc (current-column))
	    (ww (window-width))
	    (lw nil)
	    (wc (% (current-column) (1- (window-width)))))
	(setq arg (1- arg))
	(if (>= cc (1- ww))
	    (move-to-column (1+ (- cc ww)))
	  (progn
	    (previous-line 1)
	    (end-of-line 1)
	    (setq lw (current-column))
    (if (>= lw ww)
		(move-to-column (+ (* (/ lw (1- ww)) (1- ww)) wc ))
	      (move-to-column wc))))))))
(defun my-next-line (arg)
  (interactive "p")
  (if (fboundp 'line-move)
      (line-move arg)
    (if (fboundp 'next-line-internal)
	(next-line-internal arg)
      (next-line arg))))

(defun move-to-window-next-line (arg)
  "move window next line"
  (interactive "p")
  (progn
    (if (equal arg nil)
	(setq arg 1))
    (while (< 0 arg)
      (let ((cc (current-column))
	    (ww (window-width))
	    (lw nil)
	    (wc (% (current-column) (1- (window-width)))))
	(setq arg (1- arg))
	(end-of-line 1)
	(setq lw (current-column))
	(if (< (+ cc (1- ww)) lw)
	    (move-to-column (+ cc (1- ww)))
	  (progn
	    (my-next-line 1)
	    (move-to-column (% cc (1- ww)))))))))
