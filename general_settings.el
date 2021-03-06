
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;    Configuration file for Emacs 
;;      Osamu Ogasawara 
;;      2020.11.05 (for Emacs v.26)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun package-install-if-absent (pkg-name)
  (if (not (package-installed-p pkg-name))
    (package-install pkg-name)))
;; (defun package-install-if-absent (pkg-name)
;;   'ignore)



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
;;; lsp-java : Java Settings
;;;=================================================

;;; https://github.com/emacs-lsp/lsp-java

(require 'lsp-java)
(add-hook 'java-mode-hook #'lsp)

(condition-case nil
    (require 'use-package)
  (file-error
   (require 'package)
   (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
   (package-initialize)
   (package-refresh-contents)
   (package-install 'use-package)
   (setq use-package-always-ensure t)
   (require 'use-package)))


(package-install-if-absent 'projectile)
(package-install-if-absent 'flycheck)
(package-install-if-absent 'yasnippet)
(package-install-if-absent 'lsp-mode)
(package-install-if-absent 'hydra)
(package-install-if-absent 'company)
(package-install-if-absent 'lsp-ui)
(package-install-if-absent 'which-key)
(package-install-if-absent 'lsp-java)
(package-install-if-absent 'dap-mode)
;; (package-install-if-absent 'dap-java)
(package-install-if-absent 'helm-lsp)
(package-install-if-absent 'helm)
(package-install-if-absent 'lsp-treemacs)

(use-package projectile)
(use-package flycheck)
(use-package yasnippet :config (yas-global-mode))
(use-package lsp-mode :hook ((lsp-mode . lsp-enable-which-key-integration))
  :config (setq lsp-completion-enable-additional-text-edit nil))
(use-package hydra)
(use-package company)
(use-package lsp-ui)
(use-package which-key :config (which-key-mode))
(use-package lsp-java :config (add-hook 'java-mode-hook 'lsp))
(use-package dap-mode :after lsp-mode :config (dap-auto-configure-mode))
(use-package dap-java :ensure nil)
(use-package helm-lsp)
(use-package helm
  :config (helm-mode))
(use-package lsp-treemacs)

(setq lsp-java-workspace-dir "~/works/")
;;(setq projectile-project-search-path '("~/projects/" "~/works/"))
(setq lsp-inhibit-message t)
(setq lsp-ui-sideline-update-mode 'point)




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
;;(package-install-if-absent 'flymake)
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

;; ;;; ---------------------------

;; (custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(jdee-server-dir "~/local/jars")
;;  '(package-selected-packages
;;    (quote
;; 	(rustic x-path-walker tagedit yatex smartparens jdee ng2-mode ng2-ts-mode projectile neotree web-mode ctags-update pcre2el use-package tide markdown-mode ibuffer-sidebar ggtags dired-sidebar company))))
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
