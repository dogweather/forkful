---
title:                "Reading command line arguments"
aliases:
- /en/bash/reading-command-line-arguments.md
date:                  2024-01-20T17:55:14.838810-07:00
model:                 gpt-4-1106-preview
simple_title:         "Reading command line arguments"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## What & Why?

Reading command line arguments lets scripts behave differently based on user input. It's how scripts can be versatile and not just one-trick ponies.

## How to:

```Bash
#!/bin/bash

# Print the name of the script.
echo "Script name: $0"

# Print the first argument.
echo "First argument: $1"

# Print all arguments.
echo "All arguments: $@"
```

Sample output assuming your script is named 'example.sh' and you call `./example.sh arg1 arg2`:

```
Script name: ./example.sh
First argument: arg1
All arguments: arg1 arg2
```

Loop through arguments:

```Bash
#!/bin/bash

# Loop through each argument.
for arg in "$@"; do
  echo "Argument: $arg"
done
```

## Deep Dive

Bash has supported command line arguments for ages; they are positional parameters, `$0` to `$9`, with `$@` and `$*` showing all. `$0` is the script itself, `$1` to `$9` are the first to ninth argument; braces like `${10}` are needed from tenth on.

Using `$@` is usually better than `$*` as it handles arguments containing spaces correctly. `$@` gives each argument as a separate "word"; `$*` combines them all into a single "word".

You can shift through arguments using the `shift` command, which bumps `$2` to `$1`, and so forth, discarding the old `$1`.

Alternatives? Sure. `getopts` and `getopt` provide more control for options (like -h for help) and flag parsing; check them out if `$1`, `$2`,... don't cut it.

## See Also

- Bash Manual on Special Parameters: https://www.gnu.org/software/bash/manual/html_node/Special-Parameters.html
- Advanced Bash-Scripting Guide: https://www.tldp.org/LDP/abs/html/
- `getopts` tutorial: https://wiki.bash-hackers.org/howto/getopts_tutorial
