# Emacs Mini Demo

## Why?

I initially created this emacs demo as a way to experiment with various emacs
packages without having to modify my own emacs configuration.  Along the way I
discovered some new (to me) features that I also wanted to experiment with.  It
turns out the configuration files here can also be used as template for your own
configuration files.

So this demo now serves three purposes:
- it provides a way to experiment with emacs packages in a sandbox environment
- it provides examples of nifty things that can be done during emacs initialization
- it can be used as a template for your `~/.emacs.d` initialization

These three topics are discussed in more detail below.

The code in this repo's [emacs.d](./emacs.d/) is based [Kaushal
Modi's example emacs config files](https://github.com/kaushalmodi/.emacs.d/).

## Important Notes

- Running emacs as described here (with `emacs -q`) bypasses your normal emacs
  startup files.  This is intentional because I wanted to be able to experiment
  with new packages without cross contamination between my configuration and the
  new package.
- The first time you run emacs with these config files, a variety of emacs
  packages will be automatically download to a directory in `emacs.d`.  This
  takes about 5 minutes, and of course an Internet connection is needed.

## TLDR

Notes:
- You must use emacs's `-q` or `-Q` option to disable loading the user init file
- You must use `-l demo-init.el` as the final `-l` option.  The `demo-init.el`
  file fakes emacs into thinking this demo's `emacs.d` directory is the user's
  init directory.

To run emacs using this demo's initializion files, but with no demo features
enabled (i.e., a "vanilla" configuration):
```
emacs -q -l demo-init.el ~/project/src/main.c
```

To run emacs with `orderless` and `vertico` enabled:
```
emacs -q -l demo/orderless.el  -l demo/vertico.el -l demo-init.el  ~/project/src/main.c
```

To run emacs with all demo packages enabled:
```
emacs -q -l demo/*.el -l demo-init.el  ~/project/src/main.c
```

## Experiment with emacs packages

This demo supports the following packages:
- [consult](https://melpa.org/#/consult)
- [ggtags](https://melpa.org/#/ggtags)
- [gtags-mode](https://melpa.org/#/gtags-mode)
- [gxref](https://melpa.org/#/gxref)
- [ivy-xref](https://melpa.org/#/ivy-xref)
- [lsp-ivy](https://melpa.org/#/lsp-ivy)
- [lsp-mode](https://melpa.org/#/lsp-mode)
- [lsp-treemacs](https://melpa.org/#/lsp-treemacs)
- [lsp-ui](https://melpa.org/#/lsp-ui)
- [magit](https://melpa.org/#/magit)
- [orderless](https://melpa.org/#/orderless)
- [projectile](https://melpa.org/#/projectile)
- [vertico](https://melpa.org/#/vertico)
- [which-key](https://melpa.org/#/which-key)


## Nifty things that can be done during emacs initialization
## Using this emacs.d as a template for your ~/.emacs.d

### 
  - using `~/.emacs.d/init.el` instead of `~/.emacs`
- To automatically install my favorite packages when first starting Emacs
  instead of having to navigate into the package menu system to manually select
  them.
- To use Git submodules for packages of interest that weren't available on the
  standard package servers.


- Emacs startup
  - Using ~/.emacs.d/init.el instead of ~/.emacs
  - A self contained configuration, amenable to use with Git
  - Using Git submodules to access packages not found on the standard package servers
- Package management
   - Using different package dirs for different versions of Emacs
   - Automatically downloading and building packages when first starting Emacs
- Emacs packages not specific to software development:




## Important Notes




## TLDR
### Run the demo



### How it works


https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html


This demo runs emacs in a sandbox, isolated from the user's normal
emacs startup initialization.  It uses elisp files in in [emacs.d/](emacs.d/),
[demo/](demo/), and [demo-init.el](demo-init.el).

Directory [emacs.d/](emacs.d/) contains init files designed to be usable in your own
`~/.emacs.d/` directory.  You can us

These init files selectively enable features presented
in this demo.  If you use `emacs.d` as a template for your `~/.emacs.d/`, you
will need to hard-code enable the features you want to use.

There is one file in [demo/](demo/) for each demo-able feature.  These files just set variables
that tell the main init file [emacs.d/init.el](emacs.d/init.el)



Using initialization files"
            echo "from this tutorial's 'emacs.d' directory instead of the user's .emacs"
            echo "file or .emacs.d/ directory."

https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html

These




Note the first time you run eacms

## Intro

I've been using Emacs for software development since forever.  Since before [GNU
Emacs](https://en.wikipedia.org/wiki/Emacs#GNU_Emacs).  Back then the de facto
Emacs was [Gosmacs](https://en.wikipedia.org/wiki/Gosling_Emacs).  To this day
my `.emacs` file has snippets of elisp code from Gosmacs:

```
  (defun gosmacs-next-window ()
    "Select the window below or to the right of the window now selected.
From the window at the lower right corner, select the one at the upper left."
    (interactive)
    (select-window (next-window)))
```

Then we hired a young kid.  He laughed at my misfortune and threw around phrases
like "language server".  It took a few years, but I finally took the bait and
jumped into 21st century Emacs.  While learning about using language servers with
Emacs, I learned of many new (to me) features.  This little demo demonstrates
some of these "new" features, such as:

- Emacs startup
  - Using ~/.emacs.d/init.el instead of ~/.emacs
  - A self contained configuration, amenable to use with Git
  - Using Git submodules to access packages not found on the standard package servers
- Package management
   - Using different package dirs for different versions of Emacs
   - Automatically downloading and building packages when first starting Emacs
- Emacs packages not specific to software development:
  - Which-key
  - Completion styles
  - Projectile
  - Consult
- Emacs packages geared toward software development:
  - General observations
  - Magit (work w/ git)
  - Tagging (xref, cscope, etags, gtags, etc)
  - Language Servers (lsp-mode)

## Emacs Startup

References:

https://www.gnu.org/software/emacs/manual/html_node/emacs/Init-File.html
I switched from a single `$HOME/.emacs


## Package Management

- Walk through init.el

- Features are either built-in (dired), enabled by installing packages (cscope,
  gtags, magit, etc.), or by loading locally hosted Elisp files (e.g., Elisp
  code you wrote or copied from a friend).

## Useful Features
### which-key

- Turn which-key off: `M-x which-key-mode`
- Start a command: `C-x`; pause; enter `?`; note buffer showing possible commands.
- Note that `?` doesn't always work -- it may bound to a command.  Try: `C-x 8 ?`.

- Turn which-key on: `M-x which-key-mode`
- Start a command: `C-x`; pause; note completion list
- This always works

- When there are too many completions to fit, use `C-h C-n` and `C-h C-p` to page next/prev.

### Completion Styles: orderless and vertico

Compare: default, orderless, vertico, orderless+vertico

In each emacs session:
- visit lib/cn, look for "reader block"
- use "C-h v" to get help on a variable, look for "column"

vertico wierdness: get used to using `M-RETURN`

### Projectile

Projectile provides easy project management and navigation. The concept of a
project is pretty basic - just a folder containing special file. Currently
most VCS repos (e.g. git, mercurial, etc) are considered projects by default, as
are directories containing build tools (e.g. maven, leiningen, etc) or framework
markers (e.g. Ruby on Rails). If you want to mark a folder manually as a project
just create an empty .projectile file in it.

Without orderless or vertico:

- `C-c p`   Note useful-ness of which-key
- `C-c p f` Really needs a better completion/selection UI.  Did someone say
            orderless+vertico?

With orderless+vertico:

Jump to a file:
- `C-c p f omf h`  See all omf includce files
- `C-c p f test builder .c`  See all tests related to builders

Toggle between .c and .h files:
- `C-c p f kvset_builder.c` ; `C-c p a`

Grep:
- `C-c p s g REVISIT`

IBuffer - view buffers for files in project:
- `C-c p I`

Kill all project buffers:
- `C-c p k`

Other things it can do that I haven't explored:
- Toggle between code and its test
- find references in project (using xref internally) (should probably just use
  lsp, but this might be a good backup)
- regenerate project etags or gtags (requires ggtags).

### Consult
Compare: consult, consult+orderless, consult+orderless+vertico

- Visit any file
- Use `M-g g`

- Visit an outline/markdown file
- Use `M-g o` to view outline
- Use `C-n` and `C-p` to move up/down

### Magit
### Tagging

Xref provides a unified interface to finding identifiers in a program.

https://www.gnu.org/software/emacs/manual/html_node/emacs/Xref-Commands.html

Examples "tagging" systems:
- etags/ctags -- TAGS files, the orginal?  OLD -- don't bother.
- cscope -- better for C++?
- GNU global -- use with emacs `gtags-mode` or newer ``ggtags-mode`

No demo for `etags`, `cscope` or 'ggtags`.

NOTE: this is how you did it before language servers.

#### gtags

You can run gtags w/o using xref.  But why would you.

Thigs to show:
- `M-.`
- `C-u M-.`
- `M-,`
- `M-?`, then `C-n`/`C-p` and `n`/`p`

If you want more that what `xref` surfaces, use `gtags` directly.
For example:
- `M-x gtags-find-files`

Use gtags w/ orderless and vertico.

- Use `gtags-find-rtag` to see how gtags displays results natively.
- Use `C-u M-.` to see how results are displayed when using xref as a front-end for gtags.
- Use `ivy-xref` to see how results are displayed w/ ivy-show (a step down).

TODO: forget about ivy-xref.  It is provides a new way to show xref results.
But it seems way less powerful than `gxref` default or `gxref+orderless+vertico`.

### Emacs lsp-mode


emacs -q -g 80x34 -l null.el -l demo.el

emacs -q -g 80x34 -l null.el -l demo.el



./demo.sh ~/w/src/hse/hse/lib/cn & sleep 2

./demo.sh                           ~/w/src/hse/hse/lib/cn & sleep 2
./demo.sh orderless.el              ~/w/src/hse/hse/lib/cn & sleep 2
./demo.sh vertico.el                ~/w/src/hse/hse/lib/cn & sleep 2
./demo.sh orderless.el vertico.el   ~/w/src/hse/hse/lib/cn & sleep 2

./demo.sh                                     ~/w/src/hse/hse/docs/cn_omf.md & sleep 2
./demo.sh consult.el                          ~/w/src/hse/hse/docs/cn_omf.md & sleep 2
./demo.sh consult.el vertico.el               ~/w/src/hse/hse/docs/cn_omf.md & sleep 2
./demo.sh consult.el vertico.el orderless.el  ~/w/src/hse/hse/docs/cn_omf.md & sleep 2

./demo.sh                                           +28:11 ~/w/src/hse/hse/blk_list.c & sleep 2
./demo.sh gtags.el                                  +28:11 ~/w/src/hse/hse/blk_list.c & sleep 2
./demo.sh gtags.el gxref.el                         +28:11 ~/w/src/hse/hse/blk_list.c & sleep 2
./demo.sh gtags.el gxref.el orderless.el vertico.el +28:11 ~/w/src/hse/hse/blk_list.c & sleep 2

./demo.sh projectile.el                           ~/w/src/hse/hse & sleep 2
./demo.sh projectile.el orderless.el vertico.el   ~/w/src/hse/hse & sleep 2

./demo.sh gtags.el gxref.el orderless.el vertico.el               +28:11 ~/w/src/hse/hse/blk_list.c & sleep 2
./demo.sh lsp-mode.el orderless.el vertico.el                     +28:11 ~/w/src/hse/hse/blk_list.c & sleep 2
./demo.sh lsp-mode.el gtags.el gxref.el orderless.el vertico.el   +28:11 ~/w/src/hse/hse/blk_list.c & sleep 2
