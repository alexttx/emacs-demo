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
;; - Installed packages are stored in ~/.emacs.d/elpa
;;
;; Troubleshooting:
;; - If there are problems installing or upgrading, go to
;;   ~/.emacs.d/elpa/ and delete packages that are installed multiple
;;   times.
;; - To start over and/or bootstrap, see instructions below (search for
;;   "bootstrap").
;;
;; Manage packages with "M-x package-list-packages":
;; - Sort by "Status" to see what's installed.
;; - To install:
;;   - Use "i" to mark a package for install
;;   - Use "x" to install marked packages
;; - To upgrade all installed packages:
;;   - Press "U" , then "x".  You'll be prompted to remove the
;;     obsolete versions once the upgrades are install.
;;
;; You can install packages manually by adding a package name to
;; package-selected-packages and then evaluating
;; "(package-install-selected-packages)".
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
 '(package-selected-packages
   '(consult consult-lsp ivy-xref lsp-ivy lsp-mode lsp-treemacs lsp-ui magit orderless projectile use-package vertico which-key yasnippet company ggtags gtags gxref)))

(setq use-package-verbose 'debug);; values: nil, t, 'debug, 'errors
(require 'use-package)

(when nil
  ;; to bootstrap or start over w/ packages:
  ;;   mv ~/.emacs.d ~/.emacs.XXX
  ;;   emacs -Q ~/.emacs
  ;;   eval the contents of this file above this point
  ;;   eval this code:
  (package-refresh-contents)
  (package-install-selected-packages))

(when t
  ;; Personal settings: if you copy portions of this file into your .emacs file,
  ;; feel free to omit this section.
  (setq-default vc-handled-backends nil)
  (setq-default indent-tabs-mode nil)
  (setq inhibit-startup-screen t)
  (tool-bar-mode -1)
  (menu-bar-mode -1)

  (put 'narrow-to-region 'disabled nil)

  (modify-syntax-entry ?_ "w" (standard-syntax-table))
  (modify-syntax-entry ?- "w" lisp-mode-syntax-table)
  (modify-syntax-entry ?_ "w" lisp-mode-syntax-table)
  (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table)
  (modify-syntax-entry ?_ "w" emacs-lisp-mode-syntax-table)
  (modify-syntax-entry ?- "w" text-mode-syntax-table)
  (modify-syntax-entry ?_ "w" text-mode-syntax-table)

  (defun my-gosmacs-previous-window ()
    "Select the window above or to the left of the window now selected.
From the window at the upper left corner, select the one at the lower right."
    (interactive)
    (select-window (previous-window)))

  (defun my-gosmacs-next-window ()
    "Select the window below or to the right of the window now selected.
From the window at the lower right corner, select the one at the upper left."
    (interactive)
    (select-window (next-window)))

  (defun my-kill-line (&optional arg)
    "Same as kill-line but kills newline if current-column = 0."
    (interactive "P")
    (if (or arg (bolp))
        (kill-line (prefix-numeric-value arg))
      (kill-line)))

  (global-set-key "\C-h"      'backward-delete-char)
  (global-set-key "\C-xB"     (function (lambda () (interactive) (switch-to-buffer "*scratch*"))))
  (global-set-key "\C-x\C-b"  'buffer-menu)
  (global-set-key "\C-x\C-f"  'find-file-at-point)
  (global-set-key "\C-x\C-v"  'find-file)
  (global-set-key "\C-xh"     'help-command)
  (global-set-key "\M-h"      'backward-kill-word)
  (global-set-key "\M-q"      'query-replace)
  (global-set-key "\M-r"      'replace-string)
  (global-set-key "\C-k"      'my-kill-line)
  (global-set-key "\C-xn"     'my-gosmacs-next-window)
  (global-set-key "\C-xp"     'my-gosmacs-previous-window))


;; Use font size 12 for demo.  Not sure if this is the right way to set default
;; font, but it works.
(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-12"))

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

(defvar my-use-which-key nil)  ;; recommend (helps w/ key bindings)

;; completion systems
(defvar my-use-orderless nil)  ;; recommend orderless+vertico combination
(defvar my-use-vertico nil)    ;; recommend orderless+vertico combination
(defvar my-use-projectile nil) ;; recommend (really wants orderless+vertico)
(defvar my-use-consult nil)    ;; has a few nice commands (also benefits 

;; tagging (not bothering w/ cscope, etags, etc)
(defvar my-use-gtags nil)    ;; recommend
(defvar my-use-gxref nil)    ;; recommend (only works w/ gtags)
(defvar my-use-ggtags nil)   ;; skip (gtags+gxref is nicer)

;; language server protocol (requires compile_commands.json)
(defvar my-use-lsp-mode nil)     ;; recommend
(defvar my-use-lsp-ui nil)       ;; recommend
(defvar my-use-lsp-ivy nil)      ;; recommend
(defvar my-use-lsp-treemacs nil) ;; recommend
(defvar my-use-ivy-xref nil)     ;; skip (not a fan)

;; not part of demo, but i find them useful
(defvar my-use-gfm-mode nil)  ;; github flavored markdown mode
(defvar my-use-magit nil)     ;; super-charged git


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
  :bind (:map vertico-map
              ("M-h" . minibuffer-completion-help)
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
  ;; Consult offers several interesting commands.  I think it has been
  ;; superseded by orderless+vertico.  But it has some custom commands
  ;; that are interesting:
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
  ;; github flavored markdown mode (part of markdown--mode)
  :if my-use-gfm-mode
  :init
  (add-hook 'gfm-mode-hook (function
                            (lambda ()
                              ;;(my-disable-mouse)
                              (outline-minor-mode)
                              (my-whitespace-mode-on)
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

(use-package magit
  :if my-use-magit
  :init
  (setq magit-auto-revert-mode nil)
  (setq my-magit-section-recenter-looking-at "@@ \\|modified")
  (add-hook 'magit-mode-hook (function
                              (lambda ()
                                ;;(my-disable-mouse)
                                (local-set-key "n" 'my-magit-section-forward)
                                (local-set-key "p" 'my-magit-section-backward))))
  (defun my-magit-section-forward ()
    (interactive)
    (call-interactively 'magit-section-forward)
    (if (looking-at my-magit-section-recenter-looking-at)
        (recenter 0)))
  (defun my-magit-section-backward ()
    (interactive)
    (call-interactively 'magit-section-backward)
    (if (looking-at my-magit-section-recenter-looking-at)
        (recenter 0)))

  :config
  ;; disable git-rebase-mode
  (setq auto-mode-alist
        (seq-remove
         (lambda (elt) (eq 'git-rebase-mode (cdr elt)))
         auto-mode-alist)))

(use-package gtags
  ;; - Gtags and other tagging systems (etags, cscope, ggtags, etc) are the "old"
  ;;   way of navigating source code. Language servers are the "new" way.
  ;; - There's a bit of conflict between gtags using xref and lsp-mode also using
  ;;   xref.  I wonder if different commands can use xref with different backends.
  :if my-use-gtags
  :init
  (add-hook
   'gtags-mode-hook
   (function (lambda () (setq gtags-auto-update nil))))
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
  (add-hook 'c-mode-hook (function (lambda () (gtags-mode))))
  (setq gtags-auto-update nil))

(use-package ggtags
  ;; - A replacement for gtags.
  :if my-use-ggtags
  :init
  (add-hook 'ggtags-mode-hook
            (function
             (lambda ()
               (setq ggtags-auto-update nil)
               (setq xref-backend-functions '(gxref-xref-backend)))))
  ;; enable ggtags in when in c-mode
  (add-hook 'c-mode-hook (function (lambda () (ggtags-mode))))
  (setq ggtags-auto-update nil))

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
  :init (setq lsp-keymap-prefix "C-c l")
  :hook (
         (c-mode . lsp)
         (c++-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration) ;; optional which-key integration
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
