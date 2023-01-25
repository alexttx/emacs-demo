;; Emacs initialization file `init.el` sets a varable named `custom-file` to
;; point to this file, which instructs `M-x customize` to store settings here.

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(cscope-overlay-arrow-string ">")
 '(dired-dwim-target 'dired-dwim-target-next)
 '(inhibit-startup-screen t)
 '(load-home-init-file t t)
 '(package-selected-packages
   '(gxref company yasnippet which-key vertico use-package projectile orderless magit lsp-ui lsp-treemacs lsp-ivy ivy-xref consult-lsp highlight))
 '(safe-local-variable-values '((whitespace-mode)))
 '(scroll-bar-mode nil)
 '(tool-bar-mode nil))

;; My hack for verifying this file was loaded
(setq my-custom-loaded t)
