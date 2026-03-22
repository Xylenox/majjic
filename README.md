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
- `N` create a new child of the current revision and move to `@`
- `e` edit the current revision and move to `@`
- `a` enter abandon mode; in abandon mode, `SPC` toggles a revision, `RET` applies, and `C-g` cancels
- `r` enter rebase mode; in rebase mode, `o` / `a` / `b` choose onto / after / before, `RET` applies, and `C-g` cancels
- `u` undo the latest operation after confirming a full op-log peek
- `U` redo the latest undone operation after confirming a full op-log peek
- `g` refresh
