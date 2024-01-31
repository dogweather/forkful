---
title:                "Writing to standard error"
date:                  2024-01-19
html_title:           "Arduino recipe: Writing to standard error"
simple_title:         "Writing to standard error"

category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?

Writing to standard error (stderr) sends error messages and diagnostics separate from main output. Programmers use it to report issues without messing with regular data flow.

## How to:

To write to stderr in Fish, use `echo` with `>&2`:

```Fish Shell
echo "Error: Something went wrong" >&2
```

Output will not show in regular command output but will be visible on the console or can be redirected to a file:

```Fish Shell
echo "Error: Something went wrong" >&2 > /dev/null
```

This command silences standard output but shows the error message.

## Deep Dive

Early on, Unix established separate streams for data and errors: stdout and stderr. Separating them allows for clean data processing and independent error handling. In Fish, like other shells, `>&2` is an operator that directs the output to stderr. Alternatives for signaling errors include exit statuses and custom logging mechanisms, but direct writes to stderr are simple and widely used. Being a shell designed for interactive use, Fish incorporates features from other shells, including this stderr convention.

## See Also

- The Fish Shell documentation: [Using stderr](https://fishshell.com/docs/current/index.html#redirection)
- POSIX shell scripting guidelines, applicable to stderr handling: [GNU Bash Manual](https://www.gnu.org/software/bash/manual/)
