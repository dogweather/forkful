---
title:    "Bash recipe: Searching and replacing text"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why
Searching and replacing text is a common task in any programming language, including Bash. This simple yet powerful operation allows us to quickly and efficiently make changes to text files, making it an essential tool for any programmer.

## How To
To search and replace text in Bash, we use the `sed` command. This command stands for "stream editor" and is used to perform operations on text files. Here's an example of how to use `sed` to replace all occurrences of the word "hello" with "hi" in a file named `example.txt`:
```Bash
sed -i 's/hello/hi/g' example.txt
```
Let's break down this command:
- The `-i` flag stands for "in-place", meaning the changes will be made directly in the file instead of printing the result to the terminal.
- The `s` command indicates that we want to substitute something in the file.
- The first "hello" is the text we want to search for.
- The second "hi" is the text we want to replace with.
- The `/g` at the end stands for "globally", which means it will replace all occurrences of "hello" in the file.

If we have a file with the following content:
```
Hello world
Hello everyone
```
After running the `sed` command, the file will now look like this:
```
Hi world
Hi everyone
```
As you can see, all instances of "hello" were replaced with "hi". You can use this command to replace single words, phrases, or even regular expressions.

## Deep Dive
There are many options you can use with the `sed` command to perform different types of search and replace operations. Here are a few that you may find useful:
- To replace only the first occurrence of a string, use the `s` command without the `/g` flag.
- To replace only on a specific line, add the line number before the `s` command.
- You can use the `i` flag to ignore case sensitivity, meaning "hello" and "Hello" will be treated as the same word.
- To replace text in multiple files, you can use wildcards. For example, `sed -i 's/hello/hi/g' *.txt` will replace all instances of "hello" with "hi" in all `.txt` files in the current directory.

For a more detailed explanation of the `sed` command and its options, you can refer to the official documentation or check out some online tutorials.

## See Also
- [Official `sed` documentation](https://www.gnu.org/software/sed/manual/sed.html)
- [A Beginner's Guide to `sed`](https://www.digitalocean.com/community/tutorials/the-basics-of-using-the-sed-stream-editor-to-manipulate-text-in-linux)
- [`sed` tutorial by Linuxize](https://linuxize.com/post/how-to-use-sed-to-find-and-replace-string-in-files/)