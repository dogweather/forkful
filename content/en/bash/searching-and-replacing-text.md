---
title:                "Searching and replacing text"
html_title:           "Bash recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why
Have you ever needed to make a quick change to a large text file or multiple files at once? Using the search and replace function in Bash can save you time and effort compared to manually editing each instance.

## How To
To search and replace text in Bash, you can use the "sed" command followed by the search pattern, replacement text, and the file name(s) you want to make the changes in. For example:

```
sed 's/old_text/new_text/g' file.txt
```

In this command, "s" stands for substitute, "g" stands for global (meaning all instances in the file), and "file.txt" is the name of the file you want to make the changes in.

To make the changes in multiple files at once, you can use the "find" command to search for a specific file pattern and pipe it to sed using the "xargs" command. For example:

```
find . -name "*.txt" | xargs sed -i 's/old_text/new_text/g'
```

This command will search for all files ending in .txt in the current directory and subdirectories and make the desired changes in all of them.

You can also use regular expressions in your search and replace pattern for more complex changes. For example, using the following command will replace all instances of words starting with "apple" with "orange":

```
sed -i 's/apple[a-z]*/orange/g' file.txt
```

## Deep Dive
To further customize your search and replace, you can use flags and options in the sed command. Some common ones include:

- "-i" flag to make the changes directly in the file instead of just displaying the updated text in the terminal
- "-e" option to use multiple search and replace patterns in a single command
- "-r" option to use extended regular expressions for more advanced pattern matching

You can also use variables and backreferences in your replacement text. For example, using the following command will replace all instances of a person's name with their name in all caps:

```
sed -i 's/([A-Z][a-z]+) ([A-Z][a-z]+)/\U\1 \U\2/g' file.txt
```

In this case, the "\1" and "\2" are backreferences representing the first and second word in the pattern, respectively.

Remember to use caution when using the "-i" flag as the changes will be made directly in the file without any confirmation.

## See Also
- [GNU Bash Manual](https://www.gnu.org/software/bash/manual/)
- [Using sed to find and replace text in files](https://www.shellhacks.com/regex-find-and-replace-text-files-sed-awk/)
- [13 Examples To Use Linux Sed Command To Delete and Replace Text From File](https://www.cyberciti.biz/faq/howto-use-sed-delete-text-string-from-file-only-once/)