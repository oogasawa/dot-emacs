

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(require 'package)


(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;;(add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/") t)
;;(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)


(package-initialize)
;;(package-refresh-contents)


(load-theme 'wombat)
;;(load-theme 'whiteboard)

;; adwaita
;; deeper-blue
;; dichromacy
;; light-blue
;; manoj-dark
;; misterioso
;; tango
;; tango-dark
;; tsdh-dark
;; tsdh-light
;; wheatgrass
;; whiteboard
;; wombat

(load-file "~/.emacs.d/bs.el")
(load-file "~/.emacs.d/buffer-move.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Configuration file for Emacs 
;;      Osamu Ogasawara 
;;      2020.11.05 (for Emacs v.26)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq visible-bell t)       ;; Enable Visual Bell (disable beep sound)
(setq-default tab-width 4)
(column-number-mode t)


;;;=================================================
;;; Buffer Move
;;;=================================================
(require 'buffer-move)
;; To use it, simply put a (require 'buffer-move) in your ~/.emacs and
;; define some keybindings. For example, i use :

(global-set-key (kbd "<C-S-up>")     'buf-move-up)
(global-set-key (kbd "<C-S-down>")   'buf-move-down)
(global-set-key (kbd "<C-S-left>")   'buf-move-left)
(global-set-key (kbd "<C-S-right>")  'buf-move-right)

(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)


;;;=================================================
;;; Buffer Switcher
;;;=================================================
(require 'bs)
;(global-unset-key "\C-x\C-b")
;(global-set-key "\C-x\C-b" 'bs-show)

(global-set-key [(f7)]  'bs-cycle-next)
(global-set-key [(f8)]  'bs-cycle-previous)



;;;=================================================
;;; gtags mode
;;;=================================================

(require 'gtags)
(global-set-key "\M-t" 'gtags-find-tag)
(global-set-key "\M-r" 'gtags-find-rtag)
(global-set-key "\M-s" 'gtags-find-symbol)
(global-set-key "\C-t" 'gtags-pop-stack)

;;;=================================================
;;; toggle-truncate-line
;;;=================================================
(defun toggle-truncate-lines()
  "toggle truncate lines"
  (interactive)
  (if truncate-lines
      (setq truncate-lines nil)
    (setq truncate-lines t))
  (recenter))

(global-set-key "\C-c\C-l" 'toggle-truncate-lines)



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

;(define-key global-map "\C-p" 'move-to-window-prev-line)
;(define-key global-map "\C-n" 'move-to-window-next-line)
(define-key global-map [up]   'move-to-window-prev-line)
(define-key global-map [down] 'move-to-window-next-line)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
	(flymake use-package yaml-mode pcre2el jdee indium w3m tide cypher-mode tss zenburn-theme solarized-theme markdown-mode dash))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )



;;;=================================================
;;; tide (typescript) mode
;;;=================================================

(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  (company-mode +1))

(use-package tide
  :config
  (add-hook 'before-save-hook 'tide-format-before-save)
  (add-hook 'typescript-mode-hook #'setup-tide-mode)
  (add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
  (add-hook 'web-mode-hook
			(lambda ()
			  (when (string-equal "tsx" (file-name-extension buffer-file-name))
				(setup-tide-mode))))
  (flycheck-add-mode 'typescript-tslint 'web-mode))




;;;=================================================
;;; typescript mode
;;;=================================================


;; (require 'typescript)
;; (add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))

;; (require 'tss)
;; (setq tss-popup-help-key "C-:")
;; (setq tss-jump-to-definition-key "C->")
;; (setq tss-implement-definition-key "C-c i")
;; (tss-config-default)
