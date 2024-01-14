---
title:                "Bash recipe: Finding the length of a string"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

Have you ever wondered how long a string of text is? Whether you are a beginner in Bash programming or an experienced coder, being able to find the length of a string is an essential skill when working with strings. It allows you to manipulate and process your data more efficiently. In this blog post, we will explore how to find the length of a string in Bash.

## How To

There are two main ways to find the length of a string in Bash - using the `expr` command or the built-in `${#var}` variable. Let's see how these methods work with some examples.

### Using the `expr` command

The `expr` command is a built-in command in Bash that is used for performing basic arithmetic and string operations. To find the length of a string, we can use the `length` keyword with the string as the argument. Let's consider the string "Hello World!".

```Bash
# Store the string in a variable
str="Hello World!"

# Use `expr` to find the length
result=$(expr length "$str")

# Print the result
echo "The length of the string is $result"
```

The output of the above code will be:

```Bash
The length of the string is 12
```

### Using the `${#var}` variable

Bash provides a built-in `${#var}` variable to find the length of a string. It is a more efficient and cleaner way of finding the length. Let's look at the same example as above using this variable.

```Bash
# Store the string in a variable
str="Hello World!"

# Use the `${#var}` variable to find the length
result=${#str}

# Print the result
echo "The length of the string is $result"
```

The output will be the same as the previous example:

```Bash
The length of the string is 12
```

## Deep Dive

When using the `expr` command, keep in mind that the `length` keyword counts the number of characters, not the number of words. For example, in the string "Hello World!", the space between "Hello" and "World!" counts as a character and is included in the output. To get the number of words in a string, we can use the `-w` option with the `length` keyword.

```Bash
# Store the string in a variable
str="Hello World!"

# Use `expr` with the `-w` option to count words
result=$(expr length -w "$str")

# Print the result
echo "The number of words in the string is $result"
```

The output will be:

```Bash
The number of words in the string is 2
```

## See Also

For more information on working with strings in Bash, check out the following resources:

- [Bash String Manipulation](https://www.tldp.org/LDP/abs/html/string-manipulation.html)
- [Bash Scripting Guide - Manipulating Strings](https://bash.cyberciti.biz/guide/Manipulating_strings)
- [Bash - Length of a String / Length of an Array](https://bash.cyberciti.biz/guide/Length_of_string:_length_of_array)