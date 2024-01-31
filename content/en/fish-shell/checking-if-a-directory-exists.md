---
title:                "Checking if a directory exists"
date:                  2024-01-19
html_title:           "C recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Checking if a directory exists ensures a program operates on valid paths, preventing errors. It's crucial for tasks that need specific directories, like reading files or logging data.

## How to:
Check if a directory exists with a simple `test` command:
```Fish Shell
if test -d /path/to/dir
    echo "Directory exists"
else
    echo "No such directory"
end
```
Sample Output when the directory exists:
```
Directory exists
```
Sample Output when the directory does not exist:
```
No such directory
```

## Deep Dive
The `test` command (`[ ]` in POSIX shells) has been part of Unix-like systems for decades. In Fish, `test -d` checks for directory existence. This is a better approach than relying on output from commands like `ls`, which can be inconsistent or verbose.

Alternatives:
- `status` can determine if a previous command, like `cd /path/to/dir`, succeeded. However, this isn't recommended purely for existence checks, as it changes the shell's state.
- External tools like `find` or scripting languages (Python, Ruby) can achieve similar tasks but are often overkill for simple checks.

Implementation Details:
Fish's builtin `test` command is efficient and reliable. It avoids common pitfalls with external command invocations and provides a straightforward syntax.

## See Also

- Fish Shell documentation on `test`: https://fishshell.com/docs/current/cmds/test.html
- POSIX specification for `test`: https://pubs.opengroup.org/onlinepubs/9699919799/utilities/test.html
- Discussion on checking file existence: https://unix.stackexchange.com/questions/590694/checking-if-a-directory-exists-in-unix-shell-scripting
