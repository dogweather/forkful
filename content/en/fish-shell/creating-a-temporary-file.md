---
title:                "Creating a temporary file"
html_title:           "C# recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Creating Temporary Files in Fish Shell

## What & Why?

Creating a temporary file provides a safe and secure workspace to store data, perform operations, or test chunks of code while programming. This is done mainly to reduce the risk of errors affecting your main code while ensuring operational continuity.

## How To:

In Fish Shell, creating a temporary file is as simple as this:
```fish
set tempfile (mktemp)
```
This creates a new, temporary file and assigns its path to the $tempfile variable. To see what it looks like, use the 'cat' command:
```fish
cat $tempfile
```
It'll show an empty output, as the file is empty. You can add some text to it:
```fish
echo "Hello, world!" > $tempfile
cat $tempfile
```
It should now output: `Hello, world!`

## Deep Dive

Historically, temporary files in Unix-like systems were managed using `mktemp`. This CLI tool originated in BSD Unix and creates uniquely-named files and directories in a secure manner.

Alternatively, if you'd like to manage temporary files within your Fish scripts, you could use the `tempfile` function from [Oh My Fish](https://github.com/oh-my-fish).

In implementation, Fish's `(mktemp)` command invokes the system's `mktemp`, creating a file in the system's temporary directory (usually `/tmp`), then outputs the file's name. This name is securely generated with a combination of random characters to avoid potential name collisions.

## See Also

- Official Fish Shell documentation: https://fishshell.com/docs/current/
- `mktemp` man page: http://man7.org/linux/man-pages/man1/mktemp.1.html
- Understanding Fish scripting: https://fishshell.com/docs/current/tutorial.html