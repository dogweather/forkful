---
title:                "Searching and replacing text"
html_title:           "Fish Shell recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?
Searching and replacing text is the process of finding specific strings of characters and replacing them with new ones. This is a common task for programmers who need to make changes to a large amount of code or data quickly and efficiently.

## How to:
To search and replace text using the Fish Shell, you can use the `sed` and `awk` commands. These commands can be used within a `for` loop to iterate through multiple files, making it easier to make changes in bulk.

```
Fish Shell:

for file in *.txt
sed -i 's/old_text/new_text/g' $file
awk '{gsub("old_text", "new_text")}' $file
echo "Changes made in $file"
done
```

The first line of code sets up a `for` loop to iterate through all files with a `.txt` extension. The `sed` command then finds and replaces all instances of "old_text" with "new_text" in each file, using the `-i` flag to make changes in the original file. The `awk` command does the same, but using a different syntax. Finally, an `echo` statement is included to show which file the changes were made in.

## Deep Dive:
The `sed` and `awk` commands have been around since the 1970s and are commonly used for text processing. There are other alternatives like `perl` and `grep`, but `sed` and `awk` are typically faster and more efficient.

Searching and replacing text can also be accomplished using regular expressions, which allow for more advanced search patterns. These can be used instead of a specific text string in the `sed` and `awk` commands.

In the Fish Shell, `sed` and `awk` are implemented as external commands, meaning they are not built into the shell itself. This allows for flexibility and the ability to use other tools for searching and replacing text if needed.

## See Also:
- [Fish Shell Documentation](https://fishshell.com/docs/current/)
- [sed command tutorial](https://www.geeksforgeeks.org/sed-command-in-linux-unix-with-examples/)
- [awk command tutorial](https://www.geeksforgeeks.org/awk-command-unixlinux-examples/)