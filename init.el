;; This is to support loading from a non-standard .emacs.d
;; via emacs -q --load "/path/to/standalone.el"
;; see https://emacs.stackexchange.com/a/4258/22184

(setq user-init-file (or load-file-name (buffer-file-name)))
(setq user-emacs-directory (file-name-directory user-init-file))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))

(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
;;(add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/") t)
;;(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;;(setq package-user-dir (expand-file-name "elpa/" user-emacs-directory))
(package-initialize)

;; Install use-package that we require for managing all other dependencies

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; I find these light-weight and helpful

(use-package which-key
  :ensure
  :init
  (which-key-mode))

(use-package selectrum
  :ensure
  :init
  (selectrum-mode)
  :custom
  (completion-styles '(flex substring partial-completion)))

;;;=================================================
;;; Appearance and key-bindings
;;;=================================================
;; Some common sense settings

;; (load-theme 'manoj-dark)
(load-theme 'whiteboard)
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
;;(load-theme 'leuven t)


(fset 'yes-or-no-p 'y-or-n-p)
(recentf-mode 1)
(setq recentf-max-saved-items 100
      inhibit-startup-message t
      ring-bell-function 'ignore)

(tool-bar-mode 0)
(menu-bar-mode 0)
(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode 0))

(setq visible-bell t)       ;; Enable Visual Bell (disable beep sound)
(setq-default tab-width 4)
(column-number-mode t)


;;; Disable "C-x C-c" key binding.
(global-unset-key (kbd "C-x C-c"))
(defun print-message-for-exiting-emacs ()
  (interactive)
  (message "Use M-x save-buffers-kill-terminal"))
(global-set-key (kbd "C-x C-c") 'print-message-for-exiting-emacs)


;; (cond
;;  ((member "Monaco" (font-family-list))
;;   (set-face-attribute 'default nil :font "Monaco-12"))
;;  ((member "Inconsolata" (font-family-list))
;;   (set-face-attribute 'default nil :font "Inconsolata-12"))
;;  ((member "Consolas" (font-family-list))
;;   (set-face-attribute 'default nil :font "Consolas-11"))
;;  ((member "DejaVu Sans Mono" (font-family-list))
;;   (set-face-attribute 'default nil :font "DejaVu Sans Mono-10")))

(load-file (expand-file-name "rust_settings.el" user-emacs-directory))
(load-file (expand-file-name "general_settings.el" user-emacs-directory))


