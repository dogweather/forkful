---
title:                "Creating a temporary file"
date:                  2024-01-20T17:40:06.955802-07:00
model:                 gpt-4-1106-preview
simple_title:         "Creating a temporary file"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?

Creating a temporary file means making a file for short-term use. Programmers do it to store data that's only needed during the execution of a program, like intermediate results or to ensure a clean state without cluttering the permanent storage.

## How to:

In Fish Shell, you can create a temporary file using `mktemp`. Here's a quick example:

```fish
set tempfile (mktemp)
echo "Hello, temporary world!" > $tempfile
cat $tempfile
rm $tempfile
```

And you'll see something like this:

```shell
Hello, temporary world!
```

This creates a temporary file, writes a line to it, displays the content, and then deletes the file.

## Deep Dive

Back in the day, temporary files were often created manually, leading to potential naming conflicts and security issues. `mktemp` to the rescue! This command creates a file with a unique name, reducing the risk of file collision.

Alternative methods include writing to `/dev/shm` on Linux or using memory-based file systems. However, these methods are not as portable as `mktemp`.

As for temporary files' lifetimes, it's vital to remember that they should be deleted by the program that creates them. This ensures no leftover files consuming system space. In some systems, the `/tmp` directory gets cleared on reboot, but you shouldn't rely on this behavior for cleanup.

## See Also

- Fish Shell Documentation: [https://fishshell.com/docs/current/](https://fishshell.com/docs/current/)
- `mktemp` Manual: [https://www.gnu.org/software/autogen/mktemp.html](https://www.gnu.org/software/autogen/mktemp.html)
- Filesystem Hierarchy Standard: [https://refspecs.linuxfoundation.org/FHS_3.0/fhs/index.html](https://refspecs.linuxfoundation.org/FHS_3.0/fhs/index.html)