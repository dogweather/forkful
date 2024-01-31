---
title:                "Writing a text file"
date:                  2024-01-19
html_title:           "Arduino recipe: Writing a text file"
simple_title:         "Writing a text file"

category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Writing to a text file means saving data such as text or code on your computer. Programmers do it to store configurations, log information, or save data for later use.

## How to:
To write to a text file in Fish, use `echo` or `printf` followed by the `>` or `>>` operators. `>` creates a new file or overwrites an existing one, while `>>` appends to a file.

```fish
echo "Hello, fish!" > hello.txt
cat hello.txt
```
Output:
```
Hello, fish!
```

```fish
printf "Add this line as well." >> hello.txt
cat hello.txt
```
Output:
```
Hello, fish!
Add this line as well.
```

To write multi-line text, use multi-line strings or execute a command multiple times:

```fish
echo "Line 1
Line 2
Line 3" > multiline.txt
cat multiline.txt
```
Output:
```
Line 1
Line 2
Line 3
```

## Deep Dive
Fish shell, born out of frustration with existing shells' scripting languages, is known for its user-friendly scripting language. When comparing to other shells, Fish's redirection commands are similar to those in bash or zsh, but with enhanced scripting syntax.

Alternatives to writing files directly from the shell include using text editors like `vi` or `nano`, or scripting languages like Python or Perl for more complex manipulation.

Understanding how Fish manages file descriptors and the differences between `>` (overwrite) and `>>` (append) are pivotal for proper file management.

## See Also
- Fish Documentation on I/O Redirection: https://fishshell.com/docs/current/commands.html#redirect
- Learn more about text editing with `nano`: https://www.nano-editor.org/
- For a guide to `vi` (Vim): https://vimhelp.org/
