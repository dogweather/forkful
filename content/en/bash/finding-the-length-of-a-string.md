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

## What & Why?
Finding the length of a string in Bash is the process of determining the number of characters in a given string. This is useful for various reasons, such as validating user input, manipulating strings, and creating custom output.

## How to:
To find the length of a string in Bash, you can use the built-in `expr` command with the `- length` option. Here's an example:

```
str="Hello World!" 
len=$(expr length "$str") 
echo "The length of the string is $len" 
```

This will output: `The length of the string is 12`. 

Another option is to use the `${#var}` syntax, where `var` is the variable containing the string. Here's an example:

```
str="Hello World!" 
len=${#str} 
echo "The length of the string is $len" 
```

This will also output: `The length of the string is 12`.

## Deep Dive:
The `expr` command has been a part of Unix since the early days and was designed to be used in shell scripts for basic arithmetic operations. However, it can also handle string operations with the `length` option. 

An alternative to using `expr` is to use the `wc` command with the `-m` option. This will count the number of characters in a file or input, but it can also be used to count characters in a string by redirecting the string to the command. Here's an example:

```
str="Hello World!" 
len=$(echo -n "$str" | wc -m) 
echo "The length of the string is $len" 
```

This will also output: `The length of the string is 12`.

## See Also:
You can check out the official Bash documentation on `expr` and `wc` for more information and options. Here are the links:
- [expr](https://www.gnu.org/software/coreutils/manual/html_node/expr-invocation.html)
- [wc](https://www.gnu.org/software/coreutils/manual/html_node/wc-invocation.html)