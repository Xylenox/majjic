# majjic

Minimal Emacs UI for browsing `jj log`.

## V1

- `M-x majjic` opens a read-only `majjic: <repo name>` buffer for the current Jujutsu repo.
- Each revision is shown as two visible lines by default.
- The changed-file list is collapsed under the second line.
- `TAB` on a revision toggles its changed-file list.
- `TAB` on a file toggles that file's diff, with hunks shown as nested sections.
- `RET` on a file visits the working-tree file for `@`, or the unique parent when `@` is empty; older revisions open a read-only snapshot.
- `RET` on a hunk header or hunk line jumps to the corresponding location; removed lines open the old-side snapshot read-only.
- `n` / `p` use visible-section motion; from revision headings they skip the auxiliary summary/elided/connector rows, but still descend into expanded file sections; `M-n` / `M-p` move among siblings; `^` jumps to the parent section.
- `N` creates a new child of the marked revisions, or the current revision if none are marked, and lands on `@`.
- `e` edits the current revision and lands on `@`.
- `d` describes the current revision with a minibuffer prompt.
- `SPC` toggles a persistent mark on the current revision and moves to the next revision.
- `M` clears all marks.
- `a` abandons the currently marked revisions after confirmation.
- `r` enters rebase mode using marked revisions as the source, or the current revision if none are marked; `s` selects source descendants, `o` / `a` / `b` choose onto / after / before, `RET` applies, and `C-g` cancels.
- `s` enters squash mode using marked revisions as the source, or the current revision if none are marked; Majjic selects the common parent as the initial destination when available, `RET` applies, and `C-g` cancels.
- `u` undoes the latest operation after confirming a full op-log peek.
- `U` redoes the latest undone operation after confirming a full op-log peek.
- `G f f` fetches from the configured Git remote and refreshes.
- `G f t` fetches only tracked bookmarks from the configured Git remote and refreshes.
- `G p c` pushes the marked visible revisions, or the current revision if none are marked, by `jj git push --change` after a dry-run confirmation.
- `G p p` pushes bookmarks pointing at the marked visible revisions, or the current revision if none are marked, by `jj git push --revision` after a dry-run confirmation.
- `:` opens a completion prompt for configured Majjic custom commands.
- Mutations and `g` refresh run asynchronously; while one is in flight, navigation, snapshots, and lazy diff expansion stay available, but marks, rebase edits, and refresh are blocked.
- `O` is reserved for a dedicated op-log browser.
- `B` is reserved for a dedicated bookmark browser.
- `g` refreshes.

Custom commands are configured with `majjic-custom-commands`.  Each command is a
plist with a unique `:name`, a required `:command` argv template, and optional
`:preview`, `:confirm`, `:status`, `:success`, and `:refresh` entries.  Templates support
`:revset` for the selected visible revisions as one revset argument and
`(:revisions PREFIX)` for repeated revision arguments.

```elisp
(setq majjic-custom-commands
      '((:name "Graphite submit"
         :command ("gt-submit" "-r" :revset)
         :preview ("gt-submit" "--dry-run" "-r" :revset)
         :confirm "Submit selected revision with Graphite? "
         :status "Submitting selected revision..."
         :success "Submitted selected revision"
         :refresh t)))
```

## Loading

```elisp
(add-to-list 'load-path "/Users/andyphan/code/majjic")
(require 'majjic)
```

This package depends on `magit-section`.
