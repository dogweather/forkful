---
title:                "Reading a text file"
html_title:           "Go recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?

Reading a text file is a fundamental task for programmers that involves extracting data from files. It's essential for everything from data analysis to automated tasks.

## How to:

Fish works differently compared to more traditional scripting environments, but it's quite flexible once you get used to it. Let's learn to read a text file:

```Fish Shell
function read_file
    set file_content (cat $argv)
    echo $file_content
end

read_file 'path/to/your/file.txt'
```

In this script, it creates a function called `read_file` that reads content from a text file using `cat` and then echoes its content.

## Deep Dive

Fish Shell dates back to 2005, introduced to offer more interactivity and user-friendly interfaces compared to its counterparts like bash or sh. Unlike these, Fish was not POSIX-compliant but offered more features like autosuggestions, tab completions, etc.

While `cat` is perfect for smaller files, `read` could be a better alternative for reading large files line by line, especially with the '--line' flag. Here's how you could do it:

```Fish Shell
function read_large_file
    while read --line line
        echo $line
    end < 'path/to/your/large_file.txt'
end

read_large_file
```

In this script, it reads a line from the text file in the while loop until the end of the file.

## See Also

- [Fish Shell Tutorial](https://fishshell.com/docs/3.1/tutorial.html)
- [Handling large files with Fish](https://github.com/fish-shell/fish-shell/issues/600)