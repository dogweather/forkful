---
title:                "Extracting substrings"
html_title:           "Arduino recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?

Extracting substrings is the process of cutting out a sequence of characters from a string. It's a useful operation in scripting and programming as it allows programmers to manipulate and analyze data accurately.

## How To:

Pulling substrings from a string in Bash is achieved by utilizing the `${string:position:length}` syntax.

```Bash
string="Hello World!"
echo ${string:0:5}
```
Output:
```Bash
Hello
```
This command takes the first 5 characters from "Hello World!", hence the output `Hello`.

```Bash
echo ${string:6}
```
Output:
```Bash
World!
```
This command removes the first 6 characters from the string, hence outputting `World!`.

## Deep Dive

Historically, substring extraction in Unix-like systems was accomplished with external tools (like `cut` or `awk`). The modern version of Bash introduced the built-in substring mechanism, making scripting more efficient.

There are alternatives methods to extract substrings such as `expr substr` and `awk`, however, the native bash method is less complex and more efficient.

The implementation in Bash is accomplished on-the-fly during the script processing and doesn't require additional system resources unlike external tools. This complements bash's design philosophy to be an efficient scripting language.

## See Also

- Bash Guide for Beginners: [https://tldp.org/LDP/Bash-Beginners-Guide/html/](https://tldp.org/LDP/Bash-Beginners-Guide/html/)
- Advanced Bash-Scripting Guide: [https://tldp.org/LDP/abs/html/](https://tldp.org/LDP/abs/html/)
- GNU Bash Manual: [https://www.gnu.org/software/bash/manual/bash.html](https://www.gnu.org/software/bash/manual/bash.html)