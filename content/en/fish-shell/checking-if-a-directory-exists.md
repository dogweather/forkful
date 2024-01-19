---
title:                "Checking if a directory exists"
html_title:           "C# recipe: Checking if a directory exists"
simple_title:         "Checking if a directory exists"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Fish Shell: Checking if a Directory Exists

## What & Why?

Checking if a directory exists is a way to verify the presence of a specific folder in your file system using code. Programmers use this to avoid potential errors when working with files, such as reading, writing, or changing directories that might not be available.

## How To:

In Fish Shell script, use the `test` builtin function to check if a directory exists, like this:

```Fish Shell
if test -d /path/to/your/directory
    echo "Directory exists"
else
    echo "Directory does not exist"
end
```

This script simply checks the existence of the directory at the specified path. If it exists, it prints "Directory exists"; otherwise, it prints "Directory does not exist".

## Deep Dive 

Historically, the `test` command, also known as `[]`, was introduced as part of the Unix shell scripting language. Despite its old age, it's still commonly used in modern shell scripting for its simplicity and reliability.

Alternatively, you can use the `stat` function in Fish shell. However, it's considered less portable than `test`.

The `-d` flag in the `test` command specifically checks for directories. There are also flags for other file types (like `-f` for regular files) and for further checking such as file permissions.

## See Also

More about Fish shell scripting is available online, these include:
- The official Fish shell documentation: https://fishshell.com/docs/current/index.html
- Tutorials and guides on Fish script: https://fishshell.com/docs/current/tutorial.html
- Fish shell scripting cheatsheets: https://devhints.io/fish-shell-scripting