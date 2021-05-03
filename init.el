
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Configuration file for Emacs 
;;      Osamu Ogasawara 
;;      2020.11.05 (for Emacs v.26)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


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
;; (package-refresh-contents)


(defun package-install-if-absent (pkg-name)
  (if (not (package-installed-p pkg-name))
    (package-install pkg-name)))
;; (defun package-install-if-absent (pkg-name)
;;   'ignore)




;;;=================================================
;;; Appearance and key-bindings
;;;=================================================

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

(setq visible-bell t)       ;; Enable Visual Bell (disable beep sound)
(setq-default tab-width 4)
(column-number-mode t)


;;; Disable "C-x C-c" key binding.
(global-unset-key (kbd "C-x C-c"))
(defun print-message-for-exiting-emacs ()
  (interactive)
  (message "Use M-x save-buffers-kill-terminal"))
(global-set-key (kbd "C-x C-c") 'print-message-for-exiting-emacs)

;;;=================================================
;;; Buffer Switcher
;;;=================================================
(load-file "~/.emacs.d/bs.el")
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

;; (put 'dired-find-alternate-file 'disabled nil)



;;;=================================================
;;; Eshell
;;;=================================================

(package-install-if-absent 'pcre2el)
(load-file "~/.emacs.d/eshell.el")



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
(package-install-if-absent 'ng2-mode)
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


(with-eval-after-load 'tide
  (flycheck-add-mode 'typescript-tslint 'ng2-ts-mode)
  (flycheck-add-mode 'typescript-tide 'ng2-ts-mode)
)


;;;=================================================
;;; web mode
;;; https://web-mode.org/
;;;
;;; C-c C-n : Jumping between opening / closing HTML tags.
;;; C-c C-f : Code folding for HTML elements and control blocks.
;;;
;;; You can also edit plain js, jsx, css, scss, xml files.
;;;=================================================

(require 'web-mode)

(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))

;;; ---------------------------

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(jdee-server-dir "~/local/jars")
 '(package-selected-packages
   (quote
	(x-path-walker tagedit yatex smartparens jdee ng2-mode ng2-ts-mode projectile neotree web-mode ctags-update pcre2el use-package tide markdown-mode ibuffer-sidebar ggtags dired-sidebar company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
