---
title:                "Bash recipe: Searching and replacing text"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why

Have you ever found yourself needing to make changes to a large block of text, but didn't want to manually search and replace each occurrence? That's where text searching and replacing in Bash programming comes in handy. It allows you to quickly and efficiently make changes to multiple occurrences of a specific word or phrase. 

## How To

To search and replace text in Bash programming, we use the sed command. The general syntax for this command is:

```Bash
sed 's/search/replacement/g' input_file > output_file
```

Let's break down each part of this command:

- sed: The command for text editing in Bash.
- 's/search/replacement/g': The search and replace pattern. The 's' denotes the substitution operation, followed by the search term, the replacement term, and the 'g' at the end means global, which will replace all occurrences in the file.
- input_file: The file we want to make changes to.
- output_file: The new file that will contain the updated version.

For example, let's say we have a file called 'sample.txt' with the following content:

```Bash
Hello world! This is a sample text that we want to manipulate.
```

We want to replace all occurrences of 'sample' with 'example'. We can do this using the sed command like this:

```Bash
sed 's/sample/example/g' sample.txt > updated_sample.txt
```

The resulting file, 'updated_sample.txt', will have the following content:

```Bash
Hello world! This is a example text that we want to manipulate.
```

Note that the original file remains unchanged and a new file with the updated content is created.

## Deep Dive

The sed command is a powerful tool for searching and replacing text, and it offers many options and variations. Here are a few key features to keep in mind:

- The 'g' at the end of the command is optional. If you omit it, only the first occurrence of the search term will be replaced.
- You can use regular expressions in the search and replace pattern for more complex replacements.
- To make changes directly to the original file, use the '-i' option: ```Bash sed -i 's/search/replacement/g' file```.
- To only replace specific occurrences, use the '-n' option to suppress automatic printing and then use the 'p' command to only print the lines that match the pattern. For example: ```Bash sed -n '/pattern/ p' file```.

For more information and examples, check out the sed manual page by typing ```man sed``` in your terminal.

## See Also

- [Linuxize: How to Use Sed to Find and Replace String in Files](https://linuxize.com/post/how-to-use-sed-to-find-and-replace-string-in-files/)
- [The Geek Stuff: 5 Sed Command Examples](https://www.thegeekstuff.com/2009/10/unix-sed-tutorial-advanced-sed-substitution-examples/)
- [GNU Sed Manual](https://www.gnu.org/software/sed/manual/sed.html)