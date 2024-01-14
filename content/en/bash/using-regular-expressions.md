---
title:                "Bash recipe: Using regular expressions"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why Regular Expressions Are Useful in Bash Programming

Regular expressions, also known as regex, are powerful tools that allow us to search, manipulate, and extract patterns from strings of text. They provide a concise and efficient way to handle complex text matching and parsing tasks in Bash programming. With regular expressions, we can easily perform string operations such as validation, substitution, and extraction, making our code more concise and maintainable.

## How To Use Regular Expressions in Bash

To use regular expressions in bash, we first need to understand their basic syntax. Here are some examples of common regex patterns and their functionalities:

- `^`: matches the beginning of a string.
- `$`: matches the end of a string.
- `.`: matches any single character.
- `*`: matches zero or more occurrences of the previous character or group.
- `?`: matches zero or one occurrence of the previous character or group.
- `[]`: matches any character within the brackets.
- `|`: matches either the expression before or after it.
- `()` : creates a group of characters for capturing or referencing.

Now, let's see some practical examples of using regular expressions in bash:

```
re="^H.*o$"
if [[ $input =~ $re ]]; then
  echo "Input starts with 'H' and ends with 'o'"
fi

output: Input starts with 'H' and ends with 'o'
```

In the above example, we use `^` to match the beginning of the string and `o$` to match the end. The `.*` in the middle matches any characters between "H" and "o". If the input string matches the specified regular expression, it will print the corresponding message.

Now, let's try another example of substituting a part of a string:

```
sentence="I love to eat apples."
echo $sentence | sed -E 's/apples/oranges/'

output: I love to eat oranges.
```

Here, we use the `s` command in sed to substitute "apples" with "oranges". The `-E` option enables extended regular expressions, and the `g` option ensures that all occurrences of "apples" are substituted.

## Deep Dive into Regular Expressions

Regular expressions can get quite complex, and it takes some practice to become proficient in using them. Some tips to remember while working with regular expressions are:

- Use anchors `^` and `$` sparingly.
- Use character classes `[...]` as much as possible to match specific characters.
- Use grouping `()` to separate and access different parts of a regular expression.
- Understand the difference between greedy and lazy matching and use it accordingly.
- Practice, practice, practice!

## See Also
- [Bash Regex Cheat Sheet](https://www.rexegg.com/regex-quickstart.html) 
- [Linux Command Line Regular Expressions Tutorial](https://www.grymoire.com/Unix/Regular.html)
- [The Art of Command Line](https://github.com/jlevy/the-art-of-command-line/blob/master/README.md#regular-expressions)