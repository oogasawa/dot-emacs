

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

(defun package-install-if-absent (pkg-name)
  (if (not (package-installed-p pkg-name))
    (package-install pkg-name)))



(load-theme 'manoj-dark)
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


(package-install-if-absent 'pcre2el)

(load-file "~/.emacs.d/bs.el")
(load-file "~/.emacs.d/buffer-move.el")
(load-file "~/.emacs.d/eshell.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Configuration file for Emacs 
;;      Osamu Ogasawara 
;;      2020.11.05 (for Emacs v.26)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(setq visible-bell t)       ;; Enable Visual Bell (disable beep sound)
(setq-default tab-width 4)
(column-number-mode t)


;;;=================================================
;;; Buffer Switcher
;;;=================================================
(require 'bs)
;(global-unset-key "\C-x\C-b")
;(global-set-key "\C-x\C-b" 'bs-show)

(global-set-key [(f7)]  'bs-cycle-next)
(global-set-key [(f8)]  'bs-cycle-previous)


;;;=================================================
;;; Customizing dired-mode
;;;   dired reuse directory buffer.
;;; https://www.emacswiki.org/emacs/DiredReuseDirectoryBuffer
;;;=================================================

(put 'dired-find-alternate-file 'disabled nil)

;;;=================================================
;;; gtags mode
;;;=================================================

(package-install-if-absent 'ggtags)

(require 'ggtags)
(global-set-key "\M-t" 'gtags-find-tag)
(global-set-key "\M-r" 'gtags-find-rtag)
(global-set-key "\M-s" 'gtags-find-symbol)
(global-set-key "\C-t" 'gtags-pop-stack)


;;;=================================================
;;; neotree and projectile mode
;;;=================================================

(package-install-if-absent 'projectile)
(package-install-if-absent 'neotree)

(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(require 'neotree)
(defun neotree-project-dir ()
    "Open NeoTree using the git root."
    (interactive)
    (let ((project-dir (projectile-project-root))
          (file-name (buffer-file-name)))
      (neotree-toggle)
      (if project-dir
          (if (neo-global--window-exists-p)
              (progn
                (neotree-dir project-dir)
                (neotree-find file-name)))
        (message "Could not find git project root."))))
;; (global-set-key [f6] 'neotree-toggle)
(global-set-key [f6] 'neotree-project-dir)
(setq projectile-switch-project-action 'neotree-projectile-action)




;; ;;;=================================================
;; ;;; toggle-truncate-line
;; ;;;=================================================
;; (defun toggle-truncate-lines()
;;   "toggle truncate lines"
;;   (interactive)
;;   (if truncate-lines
;;       (setq truncate-lines nil)
;;     (setq truncate-lines t))
;;   (recenter))

;; (global-set-key "\C-c\C-l" 'toggle-truncate-lines)





;;;=================================================
;;; tide (typescript) mode
;;;=================================================

(package-install-if-absent 'tide)
(package-install-if-absent 'company)
(package-install-if-absent 'use-package)
(package-install-if-absent 'flymake)
(package-install-if-absent 'web-mode)

(require `web-mode)
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




;;; ---------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
	(projectile neotree web-mode ctags-update pcre2el use-package tide markdown-mode ibuffer-sidebar ggtags dired-sidebar company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
