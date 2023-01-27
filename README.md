# Emacs Mini Demo

I've been using Emacs for software development since forever.  Since before [GNU
Emacs](https://en.wikipedia.org/wiki/Emacs#GNU_Emacs).  Back then the de facto
Emacs was [Gosmacs](https://en.wikipedia.org/wiki/Gosling_Emacs).  To this day
my emacs startup files have snippets of code from Gosmacs:

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
Emacs, I learned of several new (to me) features.

I initially created this emacs demo as a way to experiment with various emacs
packages without having to modify my own emacs configuration.  But it has morphed
into a demo with three distinct values:
- it facilitates experimenting with emacs packages in a sandbox environment
- it contains examples of nifty and useful things you can do during emacs initialization
- it can be used as a template for your `~/.emacs.d` initialization

These three topics are discussed in more detail below.

Most of the "nifty and useful" emacs initialization tricks came from [Kaushal
Modi's emacs config files](https://github.com/kaushalmodi/.emacs.d/).

## TLDR

After cloning:
```
git submodule init
git submodule update
```

The first time you run emacs wiht these init files, it will download and install
about a dozen packages.

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

## Important notes

- You must use emacs's `-q` or `-Q` option to disable loading the user init file.
- The first time you run emacs version `X.Y` with these config files, a variety of
  emacs packages will be automatically download to `emacs.d/elpa_X_Y`.  This
  takes less than 3 minutes on a 2018 laptop with a good Internet connection.
- You must use `-l demo-init.el` as the final `-l` option.

## Experimenting with emacs packages

This demo supports the following packages:
- [consult](https://melpa.org/#/consult) - Consulting completing-read
- [gfm-mode](https://melpa.org/#/markdown-mode) - Major mode for Markdown-formatted text
- [ggtags](https://melpa.org/#/ggtags) - frontend to GNU Global source code tagging system
- [gtags-mode](https://github.com/Ergus/gtags-mode) - GNU Global integration
  with xref, project, completion-at-point (capf) and imenu
- [gxref](https://melpa.org/#/gxref) - xref backend using GNU Global.
- [ivy-xref](https://melpa.org/#/ivy-xref) -Ivy interface for xref results
- [lsp-ivy](https://melpa.org/#/lsp-ivy) - LSP ivy integration
- [lsp-mode](https://melpa.org/#/lsp-mode) - Emacs client/library for the Language Server Protocol
- [lsp-treemacs](https://melpa.org/#/lsp-treemacs) - lsp-mode and treemacs integration
- [lsp-ui](https://melpa.org/#/lsp-ui) - lsp-ui contains a series of useful UI
  integrations for lsp-mode, like flycheck support and code lenses
- [magit](https://melpa.org/#/magit) - A Git porcelain inside Emacs
- [orderless](https://melpa.org/#/orderless) - Completion style for matching regexps in any order
- [projectile](https://melpa.org/#/projectile) - Manage and navigate projects in Emacs easily
- [which-key](https://melpa.org/#/which-key) - Display available keybindings in popup
- [vertico](https://elpa.gnu.org/packages/vertico.html) - VERTical Interactive COmpletion


To run a vanilla emacs with none of the above packages enabled:
```
emacs -q -l demo-init.el  ~/project/src
```

Each of the packages in the above list has a corresponding elisp file in the
`demo` directory.  To enable a package, add `-l demo/<packge>.el` the emacs
command line *before* `-l demo-init.el`.  For example, to enable `which-key`, run:
```
emacs -q -l demo/which-key.el -l demo-init.el  ~/project/src
```

To enable `orderless`, `vertico` and `projectile`:
```
emacs -q -l demo/orderless.el -l demo/vertico.el \
    -l demo/projectile.el -l demo-init.el  ~/project/src
```

### Packages useful outside of software development

#### which-key

Which-key is minor mode that displays key bindings for currently entered
incomplete commands in a popup.  The popup window can be distracting, but it is
extremely useful if you haven't memorized all of emacs' 300 million key
bindings.

Start emacs with which-key enabled:
```
emacs -q -l demo/which-key.el -l demo-init.el
```

You should see a `WK` the mode line indicating which-key is enabled.  Now type
`C-x` and then pause for a few seconds. `C-x` is now an "incomplete command".

Notice the completion list that pops into view in response to the incomplete
command.  You can't not notice it.  This can be annoying at first, but after a
while the benefit of having a list of all possible next keys is quite valuable.

You can page through completions with `C-h n` and `C-h p`.  Note `C-h n n` does
do what you might expect.  The second `n` will fed to the keymap of the
incomplete command, in this case executing whatever command is bound
to `C-x n`.

Other which-key subcommands are:
- `C-h n` - scroll down
- `C-h p` - scroll up
- `C-h d` - show doc strings (try it!)
- `C-h h` - show a help buffer with all bindings base on the incomplete command
- `C-h a` - abort

Now turn which-key mode off by typing `M-x which-key-mode` (this toggles
which-key).  You should see the `WK` disappear from the mode line.

Once agan, type `C-x` and then pause for a few seconds, then type `?`.  You'll
should see help buffer with a list of all key bindings starting with `C-x`.
Typing `C-g` will cancel the incomplete command but leave the help buffer.
You can access this same help buffer when which-key is enabled with `C-h h`.

One more test: type `C-x 8`, pause, then type `?`. Instead of getting the help
buffer you get an upside-down question mark inserted into the buffer.  This is
because `C-x 8 ?` is bound to a command that inserts an upside-down question
mark into the buffer.  Which-key doesn't have this limitation.

Some things you can customize with which-key:
- the delay for detecting an incomplete command
- trigger which-key on demand rather than automatically

For more info:
- https://github.com/justbur/emacs-which-key

#### Completion Styles: orderless and vertico

Compare: default, orderless, vertico, orderless+vertico by running four emacs
sessions side by side:
```
./demo.sh                           ~/w/src/hse/hse/lib/cn & sleep 2
./demo.sh orderless.el              ~/w/src/hse/hse/lib/cn & sleep 2
./demo.sh vertico.el                ~/w/src/hse/hse/lib/cn & sleep 2
./demo.sh orderless.el vertico.el   ~/w/src/hse/hse/lib/cn & sleep 2
```

In each emacs session:
- visit lib/cn, look for "reader block"
- use "C-h v" to get help on a variable, look for "column"

vertico wierdness: get used to using `M-RETURN`

#### Projectile

```
./demo.sh projectile.el                           ~/w/src/hse/hse & sleep 2
./demo.sh projectile.el orderless.el vertico.el   ~/w/src/hse/hse & sleep 2
```

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

#### Consult

Compare: consult, consult+orderless, consult+orderless+vertico
```
./demo.sh                                     ~/w/src/hse/hse/docs/cn_omf.md & sleep 2
./demo.sh consult.el                          ~/w/src/hse/hse/docs/cn_omf.md & sleep 2
./demo.sh consult.el vertico.el               ~/w/src/hse/hse/docs/cn_omf.md & sleep 2
./demo.sh consult.el vertico.el orderless.el  ~/w/src/hse/hse/docs/cn_omf.md & sleep 2
```

- Visit any file
- Use `M-g g`

- Visit an outline/markdown file
- Use `M-g o` to view outline
- Use `C-n` and `C-p` to move up/down

### Packages geared toward software development

### Tagging

Xref provides a unified interface to finding identifiers in a program.

https://www.gnu.org/software/emacs/manual/html_node/emacs/Xref-Commands.html

Examples "tagging" systems:
- etags/ctags -- TAGS files, the orginal?  OLD -- don't bother.
- cscope -- better for C++?
- GNU global -- use with emacs `gtags-mode` or newer ``ggtags-mode`

No demo for `etags`, `cscope` or 'ggtags`.

NOTE: this is how you did it before language servers.


```
./demo.sh                                           +28:11 ~/w/src/hse/hse/blk_list.c & sleep 2
./demo.sh gtags.el                                  +28:11 ~/w/src/hse/hse/blk_list.c & sleep 2
./demo.sh gtags.el gxref.el                         +28:11 ~/w/src/hse/hse/blk_list.c & sleep 2
./demo.sh gtags.el gxref.el orderless.el vertico.el +28:11 ~/w/src/hse/hse/blk_list.c & sleep 2
```

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

#### Magit (work w/ git)


#### Language Servers (lsp-mode)
```
./demo.sh gtags.el gxref.el orderless.el vertico.el               +28:11 ~/w/src/hse/hse/blk_list.c & sleep 2
./demo.sh lsp-mode.el orderless.el vertico.el                     +28:11 ~/w/src/hse/hse/blk_list.c & sleep 2
./demo.sh lsp-mode.el gtags.el gxref.el orderless.el vertico.el   +28:11 ~/w/src/hse/hse/blk_list.c & sleep 2
```

## Nifty things that can be done during emacs initialization

- Automatically install your favorite packages when first starting Emacs
  instead of having to navigate into the package menu system to manually select
  them.  See code and comments in [emacs.d/init.el](emacs.d/init.el).

- Use different package dirs for different versions of Emacs to avoid problems
  when switching between different versions of emacs.

- Use Git submodules for packages of interest that weren't available on the
  package archives you use.  This demo uses a Git submodule for `gtags-mode`
  because the package archives only have `gtags-mode`for emacs version 28 and
  later.

## Using this emacs.d as a template for your ~/.emacs.d

If you are willing to throw away your current emacs configuration, the steps are easy:
```
mv ~/.emacs ~/.emacs.OLD
mv ~/.emacs.d ~/.emacs.d
cp -r emacs.d ~/.emacs.d
```

Then customize `custom.el`, `early-init.el` and `init.el` in your `~/.emacs.d`
directory.  In `init.el` you will probably want to change the `defvar` settings
from `nil` to `t` for packages you want to enable. For example, change `(defvar
my-use-which-key nil)` to `(defvar my-use-which-key t)`.

If you have code in `~/.emacs` or `~/.emacs.d/`, you'll need to merge it
with `emacs.d/*.el` files from this demo.

If you want to use `gtags`, you can put your `.emacs.d` files on GitHub and do
what this demo does with git submodules, or you can find it in a package
archive.  It is available at
[Gnu ELPA](https://elpa.gnu.org/packages/gtags-mode.html),
but only for emacs 28 and later.





