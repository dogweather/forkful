---
title:                "Reading a text file"
html_title:           "Bash recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Bash is a common Unix shell used to interact with the operating system through the command line. Reading a text file using Bash can be beneficial for those wanting to quickly access and manipulate the contents of a file without having to open a text editor.

## How To

To read a text file using Bash, we can use the `cat` command. This command is used to output the contents of a file onto the command line.

An example of this would be:

```Bash
cat example.txt
```

This would output the contents of `example.txt` onto the command line. If the file is large, we can use the `more` or `less` command to scroll through the file.

```Bash
more largefile.txt
less largefile.txt
```

We can also search for specific keywords within a text file using the `grep` command. This command allows us to filter out specific lines or phrases from a file.

An example of this would be:

```Bash
grep "keyword" example.txt
```

This would output all the lines from `example.txt` that contain the keyword "keyword". We can also use regular expressions with `grep` for more advanced searching.

Lastly, we can read a specific number of lines from a file using the `head` or `tail` command. `head` outputs the first few lines while `tail` outputs the last few lines of a file.

An example of this would be:

```Bash
head -5 example.txt
tail -10 example.txt
```

This would output the first 5 lines and last 10 lines of `example.txt`.

## Deep Dive

Using the `-n` flag with `cat` allows us to read a specific number of lines from a file. For example, `cat -n 15 example.txt` would output the first 15 lines of `example.txt` with line numbers for reference.

We can also use the `wc` command to count the number of words, lines, and characters in a file. The `-l` flag can be used to only count the number of lines in a file.

An example of this would be:

```Bash
wc -l example.txt
```

This would output the number of lines in `example.txt`.

## See Also
- [The Bash Manual](https://www.gnu.org/software/bash/manual/bash.html)
- [Bash by Example](http://www.freeos.com/guides/lsst/)