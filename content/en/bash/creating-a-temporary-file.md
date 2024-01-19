---
title:                "Creating a temporary file"
html_title:           "C# recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?

Creating a temporary file in Bash means generating an ephemeral, standalone file, often used for storing data temporarily during a Bash session. Programmers do this to manipulate data without affecting original inputs - it's a neat way to handle intermediate results or run tests without any permanent changes.

## How to:

Here's how to create and use a temp file:

```Bash
# Creating a temporary file:
temp_file=$(mktemp)

# Let's write something to it:
echo "Hello, World!" > "$temp_file"

# Now, let's read from the file:
cat "$temp_file"
```

The output should be `Hello, World!`. The `mktemp` command assigns a unique temporary file to 'temp_file', and we then write and read from it.

## Deep Dive:

The utility `mktemp` makes temp files in Bash. This Unix standard dates back to the 90s and safely generates unique temp file names, eliminating risks associated with common ones. 

Alternative methods to create a temp file, notably `tempfile`, exists but isn't as popular or recommended because of lesser safety features. Another alternative is manual naming, but it can lead to file clashes and isn't safe.

When you create a temp file, it resides in the system's temp directory, usually '/tmp' on Linux systems. They are automatically cleaned during reboots or by system polices at preset intervals, usually every few days.

## See Also:

1. `mktemp` Manpage: [https://linux.die.net/man/1/mktemp](https://linux.die.net/man/1/mktemp)
2. Advanced Bash-Scripting Guide - Temp files: [https://tldp.org/LDP/abs/html/tempfiles.html](https://tldp.org/LDP/abs/html/tempfiles.html)
3. GNU mktemp: [https://www.gnu.org/software/autogen/mktemp.html](https://www.gnu.org/software/autogen/mktemp.html)