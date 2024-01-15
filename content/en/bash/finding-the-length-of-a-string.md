---
title:                "Finding the length of a string"
html_title:           "Bash recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

You might need to find the length of a string in Bash for a variety of reasons, such as checking for input validation or formatting data correctly for output. It's a useful skill to have in your programming toolkit.

## How To

To find the length of a string in Bash, you can use the built-in `expr` command with the `-length` option. Here's an example:

```Bash
my_string="Hello World"
length=$(expr length "$my_string")
echo "The length of the string is $length characters."
```

This will output:

```
The length of the string is 11 characters.
```

Another option is to use the `wc` command with the `-c` option, which counts the number of characters in a given file or input. Here's an example:

```Bash
my_string="Hello World"
length=$(echo -n "$my_string" | wc -c)
echo "The length of the string is $length characters."
```

This will also output:

```
The length of the string is 11 characters.
```

## Deep Dive

Behind the scenes, both the `expr` and `wc` commands use the `strlen` function from the standard C library to determine the length of a string. This function calculates the length by counting the number of bytes until it reaches a null character, which marks the end of a string in C.

It's also important to note that the `-length` option in `expr` and the `-c` option in `wc` will include whitespace and special characters in the count, while the `-n` option in `echo` will omit the trailing newline character.

## See Also

- [Bash Scripting Tutorial - String Operations](https://www.tutorialspoint.com/unix/unix-advanced-concepts.htm)
- [Bash string length, search and replace](https://opensource.com/article/18/4/how-find-length-string-bash)
- [A brief introduction to Bash string manipulation](https://blog.fanofyan.com/introduction-to-bash-string-manipulation/)