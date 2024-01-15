---
title:                "Deleting characters matching a pattern"
html_title:           "Kotlin recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

In programming, it is common to encounter situations where certain characters need to be deleted based on a specific pattern or criteria. This could be for data cleaning, formatting, or modifying strings to fit a certain format. With Kotlin, this task can be easily accomplished using efficient functions and methods.

## How To

To delete characters matching a pattern in Kotlin, follow these simple steps:

1. First, declare a string variable that contains the original text with the unwanted characters.
2. Use the `replace()` function and specify the pattern to be matched and the replacement character or string.
3. Assign the result to a new string variable or overwrite the original text variable.

Example:
```
Kotlin
val text = "Th!is#$ is my $t$r.ing"
val cleanText = text.replace(Regex("[^A-Za-z0-9 ]"), "")

println(cleanText) // Outputs: This  is my string
```

In the above example, we replaced all special characters except letters and numbers with an empty string, effectively deleting them from the original text.

Other useful functions for deleting characters based on a pattern include `filter()` and `trim()`. These functions allow you to specify conditions for deleting characters from the beginning or end of a string, or from specific locations within the string.

## Deep Dive

Kotlin's `replace()` function uses regular expressions (or regex) to specify the pattern for matching characters to be replaced. Regex is a powerful tool for manipulating and modifying strings, and it follows a specific syntax for creating patterns.

In the above example, we used `[^A-Za-z0-9 ]` as our regex pattern. This pattern specifies that any character that is not a letter, number, or space should be replaced with an empty string. The `^` symbol inside the square brackets signifies negation, meaning "not one of the following."

To learn more about regex and its syntax, check out the official Kotlin documentation: https://kotlinlang.org/docs/regex.html.

## See Also

- [Kotlin documentation on manipulating strings](https://kotlinlang.org/docs/string-templates.html#string-interpolation)
- [Regular expressions in Kotlin](https://kotlinlang.org/docs/regex.html)
- [Kotlin collections functions for string manipulation](https://kotlinlang.org/docs/basic-types.html#string-manipulation)