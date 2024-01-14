---
title:                "Fish Shell recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to read through a large text file but found it tedious and time-consuming? Or perhaps you just want to quickly search for a specific word or phrase within a text file without scrolling through lines of irrelevant information. In this blog post, we will explore how to efficiently read and search text files using Fish Shell.

## How To

First, let's create a sample text file by typing the following command into your Fish Shell prompt:

```Fish Shell
echo "Hello, World!" > sample.txt
```

This will create a new text file called "sample.txt" and write the text "Hello, World!" into it. Now, let's use the `cat` command to view the contents of our text file:

```Fish Shell
cat sample.txt
```

You should see the output "Hello, World!" in your terminal. The `cat` command is a simple and quick way to view the contents of a text file. Now, let's say we want to search for a specific word within the text file. We can use the `grep` command followed by the word we want to search for and the text file we want to search within:

```Fish Shell
grep "World" sample.txt
```

The output should be "Hello, World!" which is the line that contains the word "World". The `grep` command is useful for searching through large text files for specific words or phrases.

## Deep Dive

Fish Shell also has a built-in command called `ll` which stands for "less is more". It allows us to view large text files in a more readable format, with the ability to scroll up and down, search for keywords, and view line numbers. Let's use the `ll` command to view our sample text file:

```Fish Shell
ll sample.txt
```

You can use the arrow keys to scroll up and down, and the `/` key to search for a specific word or phrase. To quit the `ll` command, simply press the `q` key.

Additionally, Fish Shell allows you to use regular expressions when searching through text files. This adds another layer of flexibility and power when dealing with large and complex text files.

## See Also

- [Fish Shell Documentation](https://fishshell.com/docs/current/)
- [A Guide to Using Cat and Grep for Text File Processing in Linux](https://www.tecmint.com/linux-cat-and-linux-grep-commands-for-text-manipulation/)
- [Mastering Regular Expressions](https://regex.info/blog/2006-09-15/247)

Now that you have learned how to efficiently read and search text files using Fish Shell, go ahead and put your new skills to use. Happy coding!