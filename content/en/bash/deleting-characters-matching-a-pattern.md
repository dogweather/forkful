---
title:                "Deleting characters matching a pattern"
html_title:           "Bash recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why
Need to clean up some specific characters from a text file or variable? Deleting characters matching a certain pattern can save you time and effort by automatically removing them for you.

## How To
Using the `sed` command, we can easily delete characters that match a given pattern. Here's a simple example where we want to remove all digits from a file named `test.txt`:
```Bash
sed 's/[0-9]//g' test.txt
```
The `s` in `sed` stands for substitute, and we specify the pattern to substitute in between the first and second forward slashes, in this case `[0-9]` for all digits. The global flag `g` at the end tells `sed` to replace all occurrences in the file. Running the command will output the `test.txt` file without any digits.

To delete multiple characters or patterns, we can use the same format by specifying them within brackets and using the `|` symbol as an OR operator. For example, to delete all digits and all periods `.` from `test.txt`, we can use:
```Bash
sed 's/[0-9|.]//g' test.txt
```
This will replace all occurrences of either a digit or period with an empty string.

## Deep Dive 
The `sed` command is a versatile tool that can not only delete characters matching a certain pattern, but also have more advanced uses such as replacing strings and editing specific lines in a file. It supports regular expressions, which provide powerful search and replace capabilities. `sed` also has multiple flags and options that can be used to modify its behavior, such as the `-i` flag which allows us to edit a file in-place without creating a new file.

Keep in mind that the `sed` command is not limited to just Bash, as it is available in most Unix-like operating systems, including macOS and Linux. This makes it a useful and portable tool to have in your arsenal.

## See Also 
- [sed command tutorial](https://www.geeksforgeeks.org/sed-command-in-linuxunix-with-examples/)
- [Regular expressions tutorial](https://www.regular-expressions.info/)
- [Bash scripting guide](https://www.shellscript.sh/index.html)