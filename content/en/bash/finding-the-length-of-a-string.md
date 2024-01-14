---
title:                "Bash recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

If you're new to Bash programming, you may have come across the need to find the length of a string. This might be for a variety of reasons, such as manipulating user input or validating data. Whatever the reason may be, learning how to find the length of a string can be a valuable skill in your Bash programming journey.

## How To

Finding the length of a string in Bash is a relatively simple process. First, you need to create a variable containing the string you want to find the length of. Let's use the string "Hello World" as an example.

```Bash
my_string="Hello World"
```

Next, you can use the Bash built-in command `expr` to find the length of the string. This command takes in the string and the option `-length`, which tells it to return the length of the string.

```Bash
expr length "$my_string"
```

The output of this command would be `11`, as "Hello World" consists of 11 characters. You can also combine these two steps into one line, like this:

```Bash
expr length "Hello World"
```

This will give the same output of `11`.

## Deep Dive

Although finding the length of a string in Bash may seem pretty straightforward, there are a few things to keep in mind.

First, the `expr` command only works with regular ASCII text. If your string contains non-ASCII characters, the output may not be accurate.

Second, the `expr` command can also be used to find the length of a variable. This can be useful if the string is dynamically generated or stored in a variable from user input.

Lastly, if you want to find the length of a multi-line string, you can use the `tr` command to remove the line breaks and then use `expr` to find the length.

## See Also

- [Bash Reference Manual: Parameters](https://www.gnu.org/software/bash/manual/html_node/Parameters.html#Parameters)
- [Linuxize: How to Find the Length of a String in Bash](https://linuxize.com/post/how-to-find-the-length-of-a-string-in-bash/)
- [TecAdmin: How to Find String Length in Bash](https://tecadmin.net/find-string-length-in-bash/)