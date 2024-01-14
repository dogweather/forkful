---
title:                "Fish Shell recipe: Using regular expressions"
programming_language: "Fish Shell"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

Regular expressions, also known as regex, are powerful tools used for string manipulation and pattern matching in programming. As a developer, utilizing regular expressions can help you save time and increase efficiency when working with text data.

## How To

To use regular expressions in Fish Shell, you will first need to understand the syntax and pattern matching rules. Here are some examples to get you started:

```
Fish Shell> set str "Hello, world!"
```
```
Fish Shell> string match -r [a-z]+ $str
Hello
```

In this example, the `string` command is used to apply a regular expression to the variable `str`. The regex pattern `[a-z]+` matches any lowercase letters in the string, and the output `Hello` is returned.

Another commonly used command for regular expressions in Fish Shell is `grep`, which can search for specific patterns in a file or output. For example:

```
Fish Shell> cat data.txt
First name: John
Last name: Smith

Email: johnsmith@example.com
```
```
Fish Shell> grep -o '[a-z]+@[a-z]+\.[a-z]+' data.txt
johnsmith@example.com
```

This command searches for a valid email address in the `data.txt` file and returns the output `johnsmith@example.com`.

## Deep Dive

Regular expressions can go beyond simple pattern matching and offer more advanced capabilities such as grouping, alternation, and quantifiers. Grouping allows you to specify multiple patterns to match, while alternation allows for multiple options to be considered at once. Quantifiers can be used to specify the number of repetitions for a pattern.

Additionally, Fish Shell provides various modifiers that can be added to regular expressions to modify their behavior, such as `i` for case-insensitivity and `m` for multiline matching.

To learn more about the syntax and advanced features of regular expressions in Fish Shell, check out the official documentation.

## See Also

- Fish Shell documentation on regular expressions: https://fishshell.com/docs/current/tutorial.html#tutorial-regular-expressions
- Online regex tester: https://regexr.com/