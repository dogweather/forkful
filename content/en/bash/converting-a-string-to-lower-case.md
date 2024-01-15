---
title:                "Converting a string to lower case"
html_title:           "Bash recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Converting a string to lower case is a common task in Bash scripting. It allows for consistency in formatting and comparisons when dealing with user input or system outputs.

## How To

To convert a string to lower case in Bash, we can use the built-in `tr` command along with the `[:upper:]` and `[:lower:]` character classes. Here is an example:

```Bash
str="Hello, World!"
lowercase=$(echo $str | tr '[:upper:]' '[:lower:]')
echo $lowercase
```

This code snippet will take the string "Hello, World!" and convert it to "hello, world!" using the `tr` command. We can also use the `awk` command to achieve the same result, like this:

```Bash
str="Hello, World!"
lowercase=$(echo $str | awk '{print tolower($0)}')
echo $lowercase
```

The `awk` command's `tolower()` function will convert all characters in the string to lower case.

## Deep Dive

Bash has a variety of tools and functions that can be used to convert strings to lower case. However, it is important to understand the underlying concepts to choose the right approach for the given task.

One approach we can use is through parameter expansion, specifically the `${parameter,,pattern}` format. This format will convert the string stored in `parameter` to lower case, based on the specified `pattern`. Here is an example:

```Bash
str="Hello, World!"
lowercase=${str,,[a-z]}
echo $lowercase
```

The `pattern` in this case is set to match all lowercase alphabetic characters. The parameter expansion will then convert all those characters to lower case, resulting in "hello, world!".

Another useful tool is the `read` command, which can be used to read user input and manipulate it. We can specify the `-r` flag to preserve backslashes in the input and use the `-p` flag to prompt the user. Here is an example:

```Bash
read -rp "Enter a string: " str
lowercase=$(echo $str | tr '[:upper:]' '[:lower:]')
echo $lowercase
```

This code snippet will prompt the user for a string, convert it to lower case, and then print the result.

## See Also

- [Bash Parameter Expansion](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Shell-Parameter-Expansion)
- [Bash Built-in Commands](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html#Bash-Builtins)
- [Awk Built-in Functions](https://www.gnu.org/software/gawk/manual/html_node/Character-Functions.html#Character-Functions)