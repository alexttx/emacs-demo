;; This emacs init setup is based on:
;; - https://github.com/kaushalmodi/.emacs.d/

;; The early init file should br loaded before this file.  But old versions of
;; emacs may not support early-init, so load it here just in case.
(when (not (and (boundp 'my-early-init-loaded) my-early-init-loaded))
  (let ((filename (expand-file-name "early-init.el" user-emacs-directory)))
    (message (format "Manually loading early init file: %s" filename))
    (load filename)))

;; Load newer version of .el and .elc if both are available.
(setq load-prefer-newer t)

;; Store `M-x customize` settings a dedicated file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file :noerror :nomessage)

;; https://www.reddit.com/r/emacs/comments/3kqt6e/2_easy_little_known_steps_to_speed_up_emacs_start/
(setq gc-cons-threshold (* 100 1024 1024)) ;100 MB before garbage collection

(add-to-list 'load-path (file-name-as-directory (expand-file-name "elisp" user-emacs-directory)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Emacs Package Management
;; ------------------------
;; 
;; References:
;; 
;; - http://ergoemacs.org/emacs/emacs_package_system.html
;; - https://www.emacswiki.org/emacs/InstallingPackages
;;
;; Notes:
;; - By default, installed packages are stored in ~/.emacs.d/elpa.  This file
;;   changes it to ~/.emacs.d/elpa_<major>_<mino> to avoid issues when switching
;;   between different versions of emacs.
;;
;; Troubleshooting:
;; - If there are problems installing or upgrading, go to ~/.emacs.d/elpa/ and
;;   delete packages that are installed multiple times.
;; - To start over and/or bootstrap, remove the appropriate
;;   ~/.emacs.d/elpa_<major>_<minor> dir and restart emacs.  This init file
;;   should download and build packages automatically.
;; - Don't forget to init and update git submodules:
;;     git submodule init
;;     git submodule update
;;
;; Hints for managing packages with "M-x package-list-packages":
;; - Sort by "Status" to see what's installed.
;; - To install:
;;   - Use "i" to mark a package for install
;;   - Use "x" to install marked packages
;; - To upgrade all installed packages:
;;   - Press "U" , then "x".  You'll be prompted to remove the
;;     obsolete versions once the upgrades are install.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'package)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa"        . "https://melpa.org/packages/") t)
;;(add-to-list 'package-archives '("marmalade"    . "https://marmalade-repo.org/packages/") t)

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

;; These packages will be automatically downloaded and installed on startup
;; if they are missing from the user package directory which is initalized
;; in early-init.el which was loaded at the top of this file.

(message (format "User package dir: %s" package-user-dir))

(defconst
  my-packages
  '(
    company
    consult
    consult-lsp
    ;; gtags-mode <-- via git-submodule
    gxref
    highlight
    ivy-xref
    lsp-ivy
    lsp-mode
    lsp-treemacs
    lsp-ui
    magit
    markdown-mode
    orderless
    projectile
    use-package
    vertico
    which-key
    yasnippet
    ))

;; Auto install the required packages
;; https://github.com/bbatsov/prelude/blob/master/core/prelude-packages.el
;; http://toumorokoshi.github.io/emacs-from-scratch-part-2-package-management.html
(defvar my-missing-packages '()
  "List populated at each startup that contains the list of packages that need
to be installed.")

;; Figure out which packages are missing
(dolist (p my-packages)
  (unless (package-installed-p p)
    (add-to-list 'my-missing-packages p :append)))

;; Install missing pacakges
(when my-missing-packages
  (message "Some packages are missing. Refreshing database before installing missing packages.")
  (package-refresh-contents)
  (dolist (p my-missing-packages)
    (message "Installing %s" p)
    (package-install p))
  (setq my-missing-packages '()))

;; For troubleshooting use-package
(setq use-package-verbose 'debug);; values: nil, t, 'debug, 'errors
(require 'use-package)

;; Global settings
(when nil ;; nil=disabled t=enabled
  (setq truncate-lines t)
  (setq-default indent-tabs-mode nil)
  (setq-default fill-column 80)
  )

;; keymap to put my own bindings.
(when nil ;; nil=disabled t=enabled
  (defvar xx-map (make-keymap) "Keymap for custom commands.")
  (global-set-key "\C-x\C-x" xx-map)
  ;; Rebind exchange-point-and-mark since it used to be C-xC-x
  (global-set-key "\C-x\C-m"   'exchange-point-and-mark)
  
  ;; xx-map  bindings
  (define-key xx-map " "     'set-mark-command)
  (define-key xx-map "r"     'revert-buffer)
  (define-key xx-map "\C-e"  'compile)
  )

;; Notes on Use-package
;; --------------------
;; See also https://github.com/jwiegley/use-package/blob/master/README.md
;;
;; To debug `use-package`, add this to `.emacs` before using `use-package`:
;;
;;    (setq use-package-verbose t) ;;  values: nil, t, 'debug, 'errors
;;    (require 'use-package)
;;
;; Use-package sections
;; --------------------
;;   :init        --- Executed before package is loaded. Accepts 1+ forms.
;;   :ensure      --- Avoid using ":ensure t" as it will install a package but makes
;;                    it a pain to update and will never be auto-removed.  I prefer
;;                    using emacs builtin package manager which makes udpate and
;;                    auto-remove easy.
;;   :config      --- Executed after package is loaded.
;;   :commands    --- Create autoloads for given commands (functions).
;;   :bind        --- Set key-bindings and set up to autoload the package. Could also
;;                    do this in the :init section.  Section can be a cons or a list
;;                    of conses.
;;   :bind-keymap --- See 
;;   :repeat-map  --- 
;;   :autoload    --- Set up autoload for non-interactive function.  Could also use
;;                    "autoload" in :init section.
;;   :mode        --- Add to auto-mode-alist
;;   :interpreter --- Add to interpereter-mode-alist
;;   :defer       --- force deferred loading
;;   :magic       --- do something based on contents of file
;;   :hook        --- setup hooks
;;   :custom      --- customize package variables.  Don't need to use "setq".
;;   :custom-face --- customize faces
;;   :if          --- conditional loading
;;   :after       --- specify load order for packages that depend on each other
;;   :requires    --- do not load if dependencies are not available


;; For more info on completion packages, see the "Emacs Completion" comment at
;; and of this file.

(when t
  ;; Enable by changing from nil to t
  (defvar my-use-which-key nil)
  (defvar my-use-orderless nil)
  (defvar my-use-vertico nil)
  (defvar my-use-consult nil)
  (defvar my-use-projectile nil)
  (defvar my-use-gfm-mode nil)
  (defvar my-use-dired nil)
  (defvar my-use-gtags-mode nil)
  (defvar my-use-gxref nil)
  (defvar my-use-ggtags nil)
  (defvar my-use-lsp-mode nil)
  (defvar my-use-lsp-ui nil)
  (defvar my-use-lsp-ivy nil)
  (defvar my-use-lsp-treemacs nil)
  (defvar my-use-ivy-xref nil)
  )

(use-package which-key
  ;; which-key is minor mode that displays key bindings for currently entered
  ;; incomplete commands in a popup.  The popup window can be distracting, but it
  ;; is extremely useful if you haven't memorized all of emacs' 300 million key
  ;; bindings.
  :if my-use-which-key
  :config (which-key-mode))

(use-package orderless
  ;; - This package provides an orderless completion style that divides the pattern
  ;;   into space-separated components, and matches candidates that match all of the
  ;;   components in any order.
  :if my-use-orderless
  :custom
  (completion-styles '(orderless basic))
  ;;(completion-category-overrides '((file (styles basic partial-completion))))
  (orderless-matching-styles '(orderless-literal)))

(use-package vertico
  ;; - Vertico provides a performant and minimalistic vertical completion UI based on
  ;;   the default completion system. The focus of Vertico is to provide a UI which
  ;;   behaves correctly under all circumstances. By reusing the built-in facilities
  ;;   system, Vertico achieves full compatibility with built-in Emacs completion
  ;;   commands and completion tables.
  ;; - Vertico sometimes limits the number of matches you can see comapred to not using
  ;;   vertico, but (1) it lets you use navigate the completion list (next/prev)
  ;;   and, (2) with the orderless package you can easily narrow the list of
  ;;   candidates so you don't need to see as many matches.
  :if my-use-vertico
  :init
  (vertico-mode)
  (vertico-multiform-mode)
  (setq vertico-count 10)
  (setq my-vertico-count-resize-increment (/ vertico-count 2))
  :bind (:map vertico-map
              ;; You can rebind RET and M-RET to swap vertico's normal use of
              ;; these two keys so that RET accepts what is typed and M-RET
              ;; accepts what is matched.  IF you do this,then you need to use
              ;; TAB to finish the completion before using RET to accept it.
              ;;   ("RET" . vertico-exit-input)
              ;;   ("M-RET" . exit-minibuffer)
              ("M-H" . minibuffer-completion-help)
              ("M-l" . vertico-multiform-vertical)
              ("M-g" . vertico-multiform-grid)
              ("M-r" . vertico-multiform-reverse)
              ("M-u" . vertico-multiform-unobtrusive))
  :config
  (defun my-vertico-sort-directories-first (files)
    (setq files (vertico-sort-history-length-alpha files))
    (nconc (seq-filter (lambda (x) (string-suffix-p "/" x)) files)
           (seq-remove (lambda (x) (string-suffix-p "/" x)) files)))
  (setq vertico-multiform-categories
        '((symbol (vertico-sort-function . vertico-sort-alpha))
          (file (vertico-sort-function . my-vertico-sort-directories-first)))))

(use-package consult
  ;; I think consult has been superseded by orderless+vertico, but it has some
  ;; custom commands that are interesting.  In particular:
  ;; - consult-goto-line
  ;; - consult-outline (for markdown files, use w/ vertico)
  :if my-use-consult
  :config
  (define-key (current-global-map) [remap goto-line] 'consult-goto-line)
  (global-set-key (kbd "M-g o") 'consult-outline))

(use-package projectile
  ;; project interaction library: jump to a file, a buffer, grep, find
  ;; refs (w/ xref), etc.
  :if my-use-projectile
  :init (projectile-mode +1)
  :bind (:map projectile-mode-map ("C-c p" . projectile-command-map)))

(use-package gfm-mode
  ;; github flavored markdown mode (part of markdown-mode)
  :if my-use-gfm-mode
  :init
  (add-hook 'gfm-mode-hook (function
                            (lambda ()
                              (outline-minor-mode)
                              (local-set-key "`" 'self-insert-command)
                              (local-set-key "\C-c" outline-mode-prefix-map))))
  :mode ("\\.md\\'"  . gfm-mode)
  :config
  ;; markdown-header-face-1 and -2 are the same because there is (or
  ;; should be) only one level 1 header per markdown document.
  (set-face-attribute 'markdown-header-face   nil :inherit 'font-lock-function-name-face :weight 'bold)
  (set-face-attribute 'markdown-header-face-1 nil :inherit 'markdown-header-face :foreground "blue1")
  (set-face-attribute 'markdown-header-face-2 nil :inherit 'markdown-header-face :foreground "blue1")
  (set-face-attribute 'markdown-header-face-3 nil :inherit 'markdown-header-face :foreground "RoyalBlue1")
  (set-face-attribute 'markdown-header-face-4 nil :inherit 'markdown-header-face :foreground "DodgerBlue1")
  (set-face-attribute 'markdown-header-face-5 nil :inherit 'markdown-header-face :foreground "SteelBlue1")
  (set-face-attribute 'markdown-header-face-6 nil :inherit 'markdown-header-face :foreground "DeepSkyBlue1"))

(use-package gtags-mode
  ;; - Gtags and other tagging systems (etags, cscope, gtags, etc) are the "old"
  ;;   way of navigating source code. Language servers are the "new" way.
  ;; - There's a bit of conflict between gtags using xref and lsp-mode also using
  ;;   xref.  I wonder if different commands can use xref with different backends.
  :if my-use-gtags-mode
  :load-path "elisp/gtags-mode"
  :init
  (when t
    ;; gtags-mode.el uses `:local t` in `defcustom` declaration for these vars,
    ;; which causes errors on emacs 27.1 because the vars evaluate to `nil` a few
    ;; lines later in gtags-mode.el.  Maybe it's a bug in 27.1?
    (setq gtags-mode-global-executable "global")
    (setq gtags-mode-gtags-executable "gtags")
    (if my-use-gxref
        ;; plug gtags into xref
        (add-hook
         'gtags-mode-hook
         (function (lambda () (setq xref-backend-functions '(gxref-xref-backend)))))
      ;; gxref disabled: setup bindings manually (bypasses xref)
      (define-key (current-global-map) [remap xref-find-definitions] 'gtags-find-tag)  ;; usually "M-."
      (define-key (current-global-map) [remap xref-find-references]  'gtags-find-rtag) ;; usually "M-?"
      (define-key (current-global-map) [remap xref-pop-marker-stack] 'gtags-pop-stack) ;; usually "M-,"
      )
    (add-hook 'c-mode-hook (function (lambda () (gtags-mode 1))))
    (add-hook 'c++-mode-hook (function (lambda () (gtags-mode 1))))
    ))

(use-package ggtags
  ;; - A replacement for gtags.
  :if my-use-ggtags
  :init
  (setq ggtags-update-on-save nil)
  (setq ggtags-auto-update nil)
  ;; enable ggtags in when in c-mode
  (add-hook 'c-mode-hook (function (lambda () (ggtags-mode 1))))
  (add-hook 'c++-mode-hook (function (lambda () (ggtags-mode 1)))))

(use-package lsp-mode
  ;; - https://emacs-lsp.github.io/lsp-mode/page/installation/
  ;; - apparently requires yasnippet and company
  ;; - lsp's default xref-find-definitions (C-u M-.) and xref-find-references (M-?)
  ;;   will only offer symbols in the current file.  That's one reason I use
  ;;   lsp-ivy-workspace-symbol (M-?). I don't know how to train  (C-u M-.) to search
  ;;   the workspace.
  ;; Notes on lsp-mode vs gtags:
  ;; - There's a bit of conflict between gtags and lsp-mode, especially when they
  ;;   both use xref.
  ;; - lsp-mode's UI for lsp-ivy-workspace-symbol is far better than gtags+gxref
  ;;   xref-find-references.
  ;; Nice commands:
  ;; - M-?         lsp-ivy-workspace-symbol
  ;; Not-so nice commands:
  ;; - C-u M-.     doesn't offer workspace symbols for completion
  :if my-use-lsp-mode
  :init
  (progn
    (setq lsp-keymap-prefix "C-c l")
    (defun my-hookfn-lsp-mode ()
      (if (fboundp 'lsp-enable-which-key-integration)
          (lsp-enable-which-key-integration)))
    (add-hook 'lsp-mode-hook 'my-hookfn-lsp-mode)
    ;;(setq lsp-enable-snippet nil)
    ;;(setq lsp-ui-imenu-enable nil)
    )
  :hook (
         (c-mode . lsp)
         (c++-mode . lsp)
         )
  :bind (
         ("M-?" . lsp-ivy-workspace-symbol) ; search for symbol w/ auto completion
         )
  :commands lsp)

(use-package lsp-ui
  ;; Higher level UI modules of lsp-mode, like flycheck support and code lenses.
  ;; Nice commands:
  ;; - C-c l G r   lsp-ui-peek-find-references
  :if my-use-lsp-ui
  :init
  :requires lsp
  :commands lsp-ui-mode)

(use-package lsp-ivy
  ;; Use ivy for completion for completion in lsp-mode instead of lsp's default (xref-appropos).
  :if my-use-lsp-ivy
  :requires lsp
  :commands lsp-ivy-workspace-symbol
  :config
  (setq ivy-re-builders-alist '((t . orderless-ivy-re-builder)))
  (add-to-list 'ivy-highlight-functions-alist
               '(orderless-ivy-re-builder . orderless-ivy-highlight)))

(use-package lsp-treemacs
  ;; Integration between lsp-mode and treemacs
  :if my-use-lsp-treemacs
  :requires lsp
  :commands lsp-treemacs-errors-list)

(use-package ivy-xref
  ;; Notes:
  ;; - Ivy-xref is a UI for selecting an item when the xref backend displays
  ;;   multiple choices (e.g., when find references to a symbol).  IMO it is
  ;;   inferior to gtags+gxref.  I haven't used it with other emacs tagging UIs.
  ;; - Helm-xref is similar to ivy-xref (i.e., don't use both).  Helm-xref seems
  ;;   "sticky" -- I had to uinstall the package to disable it.
  :if my-use-ivy-xref
  :init
  (when (>= emacs-major-version 27)
    (setq xref-show-definitions-function #'ivy-xref-show-defs))
  (setq xref-show-xrefs-function #'ivy-xref-show-xrefs))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; # Emacs Completion
;; ## Terminology
;; ### Completing-read API
;; 
;; Completion is a feature that fills in the rest of a name starting from an
;; abbreviation for it.  It is used all over emacs (think: TAB completion).
;; Emacs' "completing-read" API is an interface that allows custom completion
;; systems.
;;
;; Not all completion packages are centered around the completing-read API, but
;; those that do are more composable, interchangable and modular.
;;
;; For more info:
;; - https://www.gnu.org/software/emacs/manual/html_node/elisp/Completion.html
;;
;; ### Completion style
;;
;; A completion style defines criteria for matching user input (in the
;; minibuffer) to completion alternatives. During completion, Emacs tries each
;; completion style in turn. If a style yields one or more matches, that is used
;; as the list of completion alternatives. If a style produces no matches, Emacs
;; falls back on the next style.
;;
;; For more info:
;; - https://www.gnu.org/software/emacs/manual/html_node/emacs/Completion-Styles.html
;; - https://www.gnu.org/software/emacs/manual/html_node/elisp/Completion-Variables.html
;;
;; ## Completion Packages
;;
;; This link provides a great comparison of severl completion packages.
;; - https://github.com/radian-software/selectrum#selectrum-in-comparison-to-other-completion-systems
;;
;; My quick notes on various packages:
;;
;;   Vanilla   -- What I've been using forever.
;;   Ido       -- Follow-on to vanilla, pretty basic.
;;   Helm      -- Lots of folks don't like it. Too complicated.
;;   Ivy       -- Lots of folks love it, but it has become a hack over time and
;;                and doesn't integrate well w/ other packages.
;;   Selectrum -- Selectrum has been replaced by Vertico, a package which provides
;;                essentially the same features in a simpler way, and integrates
;;                more effectively with other packages.  Uses completing-read API.
;;   Vertico   -- Integrates better than ivy w/ other packages. Similar UI as Selectrum.
;;                Uses completing-read API.
;;   Consult   -- Provides new ways to do lots of tasks such as: navigate
;;                outlines, preview files/buffers, find files recursively.
;;                Uses completing-read API.
;;   Company   -- Used for completion in normal buffers (as opposed to
;;                minibuffer). Used by IDEs to complete symbol names.
;;   Yasnippet -- Used for completion in normal buffers (as opposed to
;;                minibuffer). Used by IDEs to complete symbol names.
;;
;; Packages that augment completion frameworks:
;;
;;   Counsel    -- Ivy power features - built on top of ivy.
;;   Marginalia -- Provides extra info for each candidate. IMO the extra info is
;;                 not that useful and it consumes real estate that could be used
;;                 to show more alternatives.
;;   Orderless  -- "foo bar" will match foo-blah-bar and barfly-foo.
;;   Prescient  -- https://github.com/radian-software/prescient.el
;;   Embark     -- Context sensitive completion.  Works in minibuffer and normal
;;                 buffers (like Company). You could use it to recognize Jira or
;;                 Github IDs in text buffers or code comments and show a hover
;;                 action to view the issue.
;;
;; References:
;; - https://www.youtube.com/watch?v=5ffb2at2d7w
;; - https://github.com/radian-software/selectrum#selectrum-in-comparison-to-other-completion-systems
;;
;; ## Some new-to-me packages worth exploring
;;
;;   xref      -- Builtin front-end for finding symbols. It needs a backend such as etags,
;;                gtags, lsp, etc.
;;
;;   helm-xref -- helm interface for xref results
;;
;;   dap-mode  -- Debug Adapter Protocol is a wire protocol for communication
;;                between client and debug Server. It's similar to the LSP but
;;                provides integration with debug server.
;;
;;   dash      -- A modern list API for emacs-lisp
;;
;;   hydra     -- A package that can be used to tie related commands into a
;;                family of short bindings with a common prefix - a Hydra.
;;                Example: Imagine C-c j and C-c k bindings, and you want to
;;                call C-c j and C-c k in some (arbitrary) sequence. Hydra
;;                allows you to bind functions so that pressing C-c jk3j5k is
;;                equivalent to pressing C-c j C-c k M-3 C-c j M-5 C-c k. Any
;;                key other than j or k exits this state.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
