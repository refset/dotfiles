generally, make changes to the source org files and then reload using:
`emacsclient -e '(org-babel-load-file (expand-file-name "~/.emacs.d/emacs.org"))'`

note: `org-babel-tangle-file` does NOT work for reloading - it requires explicit `:tangle` headers. use `org-babel-load-file` which tangles and loads in one step.

also feel free to test changes via `emacsclient -e ...` if it makes sense to

restart the daemon with:
`systemctl --user restart emacs`

the jt-menu system is intended to help discover and remember commands that we collaborate on, please add to it using the annotations per the examples in emacs.org
