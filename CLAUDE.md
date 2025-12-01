ignore untracked files when deleting anything / preparing commits

generally, make changes to the source org files and then reload using:
`emacsclient -e '(org-babel-load-file (expand-file-name "~/.emacs.d/emacs.org"))'`

note: `org-babel-tangle-file` does NOT work for reloading - it requires explicit `:tangle` headers. use `org-babel-load-file` which tangles and loads in one step.

also feel free to test changes via `emacsclient -e ...` if it makes sense to

restart the daemon with:
`systemctl --user restart emacs`

the jt-menu system is intended to help discover and remember commands that we collaborate on, please add to it using the annotations per the examples in emacs.org

when we're debugging something that requires you to add `(message ...` entries and then ask me to repro something - just read from the *Messages* buffer yourself by adding an entry beforehand so you can find exactly the right lines when I respond to you with "done [...]" e.g. doing `emacsclient -e '(with-current-buffer "*Messages*" (buffer-substring-no-properties (max (point-min) (- (point-max) 5000))       timeout: 
      (point-max)))' 2>&1 | grep -E "TSCROLL-…`
