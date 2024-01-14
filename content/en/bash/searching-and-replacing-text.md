---
title:                "Bash recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

As a programming language, Bash is commonly used for automating tasks on a Unix or Linux system. One common task is searching and replacing text within files. This can save time and effort when working with large amounts of data or when trying to make changes across multiple files.

## How To 

To search and replace text in Bash, we can use the `sed` command. This command allows us to specify a pattern to search for and a replacement string, which will be used to replace any instances of the pattern.

Here is an example of how to use `sed` to replace all instances of the word "hello" with "welcome" in a file named "example.txt":

```Bash
sed -i 's/hello/welcome/g' example.txt
```

The `-i` flag tells `sed` to edit the file in place, meaning the changes will be made directly to the file. The `s` indicates that we want to substitute the pattern with the replacement string. The `g` at the end stands for global, meaning it will replace all instances of the pattern in the file.

If we had multiple files that we wanted to make the same changes to, we could use a wildcard `*` to specify all files in the current directory. For example:

```Bash
sed -i 's/hello/welcome/g' *.txt
```

This would make the same "hello" to "welcome" replacement in all text files in the current directory.

## Deep Dive 

The `sed` command has many options and functionalities that allow for more specific and advanced text replacement. For instance, we can use regex (regular expressions) to search for patterns in the text, giving us more control over the replacement process.

Additionally, we can use `sed` with other Bash commands, such as `grep` and `awk`, to create powerful and efficient text manipulation tools.

It's also worth noting that `sed` is not limited to just replacing text in files. We can also use it to make changes to streams of data, making it a valuable tool for scripting and automation.

## See Also

- [Bash Documentation](https://www.gnu.org/software/bash/manual/bash.html)
- [Sed Tutorial](https://www.grymoire.com/Unix/Sed.html) 
- [Regex Cheat Sheet](https://www.rexegg.com/regex-quickstart.html)