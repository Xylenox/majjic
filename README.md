# majjic (MAJ-jik)

[jjui](https://github.com/idursun/jjui) + [magit](https://github.com/magit/magit) inspired jj ui for Emacs!

## Features

### Expand Everything!

<img src="./media/expand_everything.gif" width="50%" height="50%"/>

### Open Files!

<img src="./media/open_files.gif" width="50%" height="50%"/>

### New and Edit Revisions!

<img src="./media/new_and_edit.gif" width="50%" height="50%"/>

### Abandon!

<img src="./media/abandon.gif" width="50%" height="50%"/>

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
- `g` refresh
