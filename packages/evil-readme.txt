Evil is an extensible vi layer for Emacs. It emulates the main
features of Vim, and provides facilities for writing custom
extensions.

Evil lives in a Git repository. To obtain Evil, do

     git clone git://github.com/emacs-evil/evil.git

Move Evil to ~/.emacs.d/evil (or somewhere else in the `load-path').
Then add the following lines to ~/.emacs:

     (add-to-list 'load-path "~/.emacs.d/evil")
     (require 'evil)
     (evil-mode 1)

Evil requires undo-redo (Emacs 28), undo-fu or undo-tree for redo
functionality.  Otherwise, Evil uses regular Emacs undo.

Evil requires `goto-last-change' and `goto-last-change-reverse'
function for the corresponding motions g; g, as well as the
last-change-register `.'. One package providing these functions is
goto-chg.el:

    http://www.emacswiki.org/emacs/GotoChg

Without this package the corresponding motions will raise an error.
