# majjic

[jjui](https://github.com/idursun/jjui) + [magit-section](https://github.com/magit/magit) inspired jj ui for Emacs!

## Demo

https://github.com/user-attachments/assets/9da48e9c-72da-463d-91a3-b97fefcf597d

## Command Reference

- `M-x majjic` open the log
- `TAB` expand or collapse a revision, file, or hunk
- `RET` open a file or jump from a hunk line
- `n` / `p` move through visible sections
- `M-n` / `M-p` move between siblings
- `^` jump to parent section
- `N` create a new child of the marked revisions, or the current revision if none are marked, and move to `@`
- `e` edit the current revision and move to `@`
- `d` describe the current revision with a minibuffer prompt
- `SPC` toggle a persistent mark on the current revision and move to the next revision
- `M` clear all marks
- `a` abandon the currently marked revisions after confirmation
- `r` enter rebase mode using marked revisions as the source, or the current revision if none are marked; in rebase mode, `o` / `a` / `b` choose onto / after / before, `RET` applies, and `C-g` cancels
- `u` undo the latest operation after confirming a full op-log peek
- `U` redo the latest undone operation after confirming a full op-log peek
- `G f f` fetch from the configured Git remote and refresh
- `G f t` fetch only tracked bookmarks from the configured Git remote and refresh
- `G p c` push the marked visible revisions, or the current revision if none are marked, by `jj git push --change` after a dry-run confirmation
- `G p p` push bookmarks pointing at the marked visible revisions, or the current revision if none are marked, by `jj git push --revision` after a dry-run confirmation
- Mutations and `g` refresh run asynchronously; while one is in flight, navigation, snapshots, and lazy diff expansion stay available, but marks, rebase edits, and refresh are blocked
- `O` reserved for an op-log browser
- `B` reserved for a bookmark browser
- `g` refresh
