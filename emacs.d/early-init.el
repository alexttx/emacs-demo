;; This is the "early init" file.  It is loaded before the package system and the GUI
;; are initialized, and can be used to customize variables that affect the package
;; initialization process, such as package-enable-at-startup, package-load-list,
;; and package-user-dir.
;; 
;; Note that variables like package-archives which only affect the installation of
;; new packages, and not the process of making already-installed packages
;; available, may be customized in the regular init file.
;; 
;; Most customizations for Emacs should be put in the normal init file.
;; 
;; Since the early init file is read before the GUI is initialized, customizations
;; related to GUI features will not work reliably in early-init.el.  By contrast,
;; the normal init files are read after the GUI is initialized. If you must have
;; customizations in the early init file that rely on GUI features, make them run
;; off hooks provided by the Emacs startup, such as window-setup-hook or
;; tty-setup-hook.


(setq package-user-dir
      (let ((elpa-dir-name (format "elpa_%s_%s" emacs-major-version emacs-minor-version)))
        (file-name-as-directory (expand-file-name elpa-dir-name user-emacs-directory))))

(setq my-early-init-loaded t)
