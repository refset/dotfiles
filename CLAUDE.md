ignore untracked files when deleting anything / preparing commits

generally, make changes to the source org files and then reload using:
`emacsclient -e '(org-babel-load-file (expand-file-name "~/.emacs.d/emacs.org"))'`

note: `org-babel-tangle-file` does NOT work for reloading - it requires explicit `:tangle` headers. use `org-babel-load-file` which tangles and loads in one step.

also feel free to test changes via `emacsclient -e ...` if it makes sense to

restart the daemon with:
`systemctl --user restart emacs`

if emacs is frozen/pegged at 100% CPU and emacsclient hangs, try:
`kill -SIGUSR2 <pid>` (get pid from `pgrep -f 'emacs --daemon'`)
this can unfreeze it by interrupting whatever loop it's stuck in

the jt-menu system is intended to help discover and remember commands that we collaborate on, please add to it using the annotations per the examples in emacs.org

when we're debugging something that requires you to add `(message ...` entries and then ask me to repro something - just read from the *Messages* buffer yourself by adding an entry beforehand so you can find exactly the right lines when I respond to you with "done [...]" e.g. doing `emacsclient -e '(with-current-buffer "*Messages*" (buffer-substring-no-properties (max (point-min) (- (point-max) 5000))       timeout: 
      (point-max)))' 2>&1 | grep -E "TSCROLL-…`


When writing plans, be extremely concise. Sacrifice grammar for the sake of concision.
At the end of each plan, list unresolved questions. Ask about edge cases, error handling, and unclear requirements before proceeding.
End every plan with a numbered list of concrete steps. This should be the last thing visible in the terminal.

(reminder for myself: These tools keep improving. Speed is one of the main areas they're getting better at. Every time I use plan mode, I build intuition about what AI can handle and how to communicate requirements to it. That skill compounds.)

remind me to use planning mode every time I don't use it!
