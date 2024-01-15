---
title:                "Deleting characters matching a pattern"
html_title:           "Java recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Java"
category:             "Java"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

Deleting characters matching a pattern is a common task in programming. It allows for the manipulation of strings and the removal of unwanted characters. This can be useful for data processing, text formatting, and many other applications.

## How To

To delete characters matching a pattern in Java, we can use the `replaceAll()` method from the `String` class. This method takes in two parameters: the regular expression of the pattern to be matched and the replacement string.
```
String input = "Hello, $World$!";
String result = input.replaceAll("\\$", "");
System.out.println(result); // Output: Hello, World!
```
In this example, we used the `replaceAll()` method to remove all occurrences of the dollar sign `$` from the input string.

We can also use the `replace()` method from the `String` class to delete specific characters. This method takes in two parameters: the character to be replaced and the replacement character.
```
String input = "Hello, World!";
String result = input.replace('o', '');
System.out.println(result); // Output: Hell, Wrld!
```
In this example, we used the `replace()` method to delete all the letter `o` from the input string.

## Deep Dive

The `replaceAll()` and `replace()` methods both work with regular expressions to match and replace characters. Regular expressions are a powerful tool for pattern matching in strings and can be used in a wide range of programming languages.

It is important to understand the syntax of regular expressions when using these methods. Special characters like `$`, `|`, `*`, and `+` have different meanings in regular expressions and need to be escaped with backslashes if we want to match them literally.

We can also use quantifiers in regular expressions to specify how many times a character or group of characters should be matched. For example, `o*` would match zero or more occurrences of the letter `o`, while `o+` would match one or more occurrences of the letter `o`.

Additionally, we can use character classes in regular expressions to match specific ranges of characters. This can be useful when we want to delete all punctuation or special characters from a string.

## See Also

For more information on regular expressions and their usage, check out the following resources:

- [Java Regular Expressions - Oracle Documentation](https://docs.oracle.com/javase/tutorial/essential/regex/)
- [Regex Tutorial - Regular Expressions 101](https://regex101.com/)
- [Regular Expressions - GeeksforGeeks](https://www.geeksforgeeks.org/regular-expressions-in-java/)