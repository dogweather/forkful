---
title:                "Checking if a directory exists"
aliases: - /en/fish-shell/checking-if-a-directory-exists.md
date:                  2024-02-03T19:02:29.788794-07:00
model:                 gpt-4-0125-preview
simple_title:         "Checking if a directory exists"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Checking if a directory exists in Fish Shell allows scripts to make decisions based on the presence or absence of directory structures, enabling tasks like conditional file operations, logging, or environment setup. This technique is crucial for writing robust scripts that interact with the filesystem in a predictable way.

## How to:
Fish Shell uses the `test` command to check file types and characteristics, including whether a target is a directory. Here's a basic pattern for checking if a directory exists:

```fish
if test -d /path/to/dir
    echo "Directory exists"
else
    echo "Directory does not exist"
end
```
Sample Output:
```
Directory exists
```

For more streamlined file and directory operations, one might turn to external tools like `fd`, though it's more commonly used for finding files and directories rather than just checking for existence. However, combining it with Fish scripting can yield handy results:

```fish
set dir "/path/to/search"
if fd . $dir --type directory --max-depth 1 | grep -q $dir
    echo "Directory exists"
else
    echo "Directory does not exist"
end
```

This `fd` example searches for the directory at a specified depth, and `grep` checks for the match, making it versatile for nuanced checks. However, for the direct purpose of checking existence, sticking to Fish's built-in `test` is both efficient and straightforward.
