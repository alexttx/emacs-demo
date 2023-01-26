
;; set user-emacs-directory two dirs up from this file
(setq user-emacs-directory
      (concat (file-name-directory (or load-file-name (buffer-file-name))) "emacs.d"))

(message (format "Demo mode: setting user-emacs-directory to %s" user-emacs-directory))

(let ((init-file-name (expand-file-name "init.el" user-emacs-directory)))
  (message (format "Demo mode: loading %s" init-file-name))
  (load init-file-name))

