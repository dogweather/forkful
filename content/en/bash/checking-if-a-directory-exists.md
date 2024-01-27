---
title:                "Checking if a directory exists"
date:                  2024-01-19
html_title:           "C recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?

Checking if a directory exists is about making sure a folder is there before you try to do something with it, like reading files or saving new ones. Programmers do it to avoid errors, keep the flow of their scripts smooth, and handle situations when things aren't quite where they're supposed to be.

## How to:

Here's how you check if a directory exists in Bash:

```Bash
if [ -d "/path/to/dir" ]; then
  echo "Directory exists."
else
  echo "Directory does not exist."
fi
```

Sample output if the directory exists:

```
Directory exists.
```

And if it doesn't:

```
Directory does not exist.
```

Yes, it's that simple. But remember to replace `/path/to/dir` with the actual path you're checking for.

## Deep Dive

Way back when, people pretty much did the same thing, using command-line tests similar to what we do today. Bash has always had a built-in way to check for files and directories because it's a fundamental need.

Now, why `-d` and not something else? In Bash, `-d` specifically tests for the presence of a directory. There are other tests too, like `-f` for files or `-e` for existence (files or directories).

Sometimes you might see:

```Bash
if [[ -d "/path/to/dir" ]]; then
  # Double brackets for a bit more modern, robust approach.
fi
```

Or even `&&` and `||` for short-handers:

```Bash
[ -d "/path/to/dir" ] && echo "Directory exists." || echo "Directory does not exist."
```

Be careful, though—this last one could mislead you if `echo "Directory exists."` fails for some reason, then `echo "Directory does not exist."` will run even if the directory exists. Use it with caution and understanding.

## See Also

- **Bash Conditional Expressions**: [GNU Bash Manual](https://www.gnu.org/software/bash/manual/html_node/Bash-Conditional-Expressions.html)
- **Bash Scripting Tutorial**: [Ryans Tutorials](https://ryanstutorials.net/bash-scripting-tutorial/)
- **Advanced Bash Scripting Guide**: [The Linux Documentation Project](https://tldp.org/LDP/abs/html/)
