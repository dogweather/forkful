---
title:    "Bash recipe: Deleting characters matching a pattern"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## Why

Bash programming is a popular choice for automating tasks on Linux systems. It is a powerful tool for performing various string manipulations, including deleting characters that match a specific pattern. So, if you're looking to streamline your coding process and make it more efficient, learning how to delete characters matching a pattern in Bash can be incredibly useful.

## How To

First, let's take a look at the basic syntax for deleting characters that match a pattern in Bash.

```Bash
${variable/pattern/string}
```

In this syntax, `variable` represents the string where the pattern will be searched, `pattern` refers to the regular expression that will be used to match the characters, and `string` is the replacement for the matched characters. 

Now, let's see an example. Suppose we have a variable `name` with the value "John Doe". We want to remove all the lowercase alphabets from this string. We can do that using the following code:

```Bash
${name/[a-z]/}
```

The above code will replace all the lowercase alphabets with an empty string, leaving us with "JD" as the output.

We can also use the `#` (hash) or `%` (percent) symbol to delete characters from the beginning or end of the string, respectively. For example, consider the string "Hello World". If we want to remove all the characters from the beginning up to the fourth character, we can use the `#` symbol, like this:

```Bash
${"Hello World"//#(Hello )/}
```

The above code will output "World", as it deleted the first four characters (Hello ) from the string.

## Deep Dive

As we can see, the syntax for deleting characters matching a pattern in Bash is straightforward and easy to understand. However, it is essential to understand the different types of regular expressions that can be used in the `pattern` section. Here are some examples:

- **[a-z]**: Matches any lowercase alphabet
- **[A-Z]**: Matches any uppercase alphabet
- **[0-9]**: Matches any digit
- **[[:space:]]**: Matches any whitespace character (space, tab, newline, etc.)
- **[[:punct:]]**: Matches any punctuation character (., !, ?, etc.)
- **[[:alnum:]]**: Matches any alphanumeric character (a-z, A-Z, 0-9)
- **[[:alpha:]]**: Matches any alphabetic character (a-z, A-Z)

It is also possible to use more complex regular expressions, such as using the `|` (pipe) symbol to specify multiple patterns. For example, if we want to remove all the lowercase vowels from a string, we can use the following code:

```Bash
${"Hello World"/[a|e|i|o|u]/}
```

This code will output "Hll Wrld", as it has removed all the lowercase vowels from the string "Hello World".

## See Also

- [Bash Guide for Beginners - Advanced Bash-Scripting Guide](https://tldp.org/LDP/abs/html/string-manipulation.html)
- [Regular Expressions - Linux Documentation Project](https://www.grymoire.com/Unix/Regular.html)
- [Bash Scripting Cheat Sheet - Devhints](https://devhints.io/bash)