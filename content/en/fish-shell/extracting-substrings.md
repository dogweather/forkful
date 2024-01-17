---
title:                "Extracting substrings"
html_title:           "Fish Shell recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?

 Sometimes, when writing code, you need to extract a specific part of a larger string. This is called "extracting substrings" and it is a common task for programmers. It allows you to manipulate and use smaller sections of a string instead of the entire thing.

## How to:

To extract substrings with the Fish Shell, use the `string sub -s index -l length` command. The `-s` flag specifies the starting index and the `-l` flag specifies the length of the substring. Here's an example:

```
Fish Shell > set my_string "hello world"
Fish Shell > string sub -s 0 -l 5 $my_string
hello
```

This will extract the first five characters of the string "hello world" and output it as "hello".

You can also combine the `-s` and `-l` flags to extract specific parts of a string. For example, if you wanted to extract the word "world" from "hello world", you would use the command `string sub -s 6 -l 5 $my_string`.

## Deep Dive:

Extracting substrings has been a common task since the early days of programming. It provides a way to work with smaller, more manageable pieces of data. While you can still extract substrings with other programming languages like Python or JavaScript, using a shell like Fish can make it quicker and easier.

There are also other ways to extract substrings with Fish Shell. You can use regular expressions with the `string sub -r regex` command, or use specialized functions such as `string trim` to remove certain characters from the string before extracting the substring.

## See Also:

- [Official Fish Shell Documentation](https://fishshell.com/docs/current/cmds/string.html)
- [Fish Shell Regular Expressions Tutorial](https://fishshell.com/docs/current/tutorial.html?doc=refextract.html#refextract)
- [Fish Shell Function Reference](https://fishshell.com/docs/current/commands.html#appendix-function-summary)