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

# What & Why?
Deleting characters matching a pattern is a way to remove specific characters from a string or text that match a given pattern. Programmers often do this to manipulate data or to apply certain formatting to the text, such as removing all punctuation or replacing certain letters with symbols.

# How to:
To delete characters matching a pattern, we can use the ```replaceAll()``` method in Java. This method takes two parameters: the pattern to match and the replacement text. Here is an example of removing all numbers from a string:

```Java
String text = "This is 123 a string 456 with numbers."; 
text = text.replaceAll("[0-9]", ""); // replacing all numbers with empty string
System.out.println(text);

// output: This is a string with numbers.
```

## Deep Dive:
- **Historical Context:** The ```replaceAll()``` method was introduced in Java 1.4. It was an improvement from the ```replace()``` method, which could only replace a single character or string.
- **Alternatives:** Other programming languages have similar methods for deleting characters, such as ```replace()``` in Python and ```str_replace()``` in PHP. Regex (regular expressions) can also be used for more complex pattern matching.
- **Implementation Details:** The ```replaceAll()``` method uses regular expressions to find and replace the matching characters. It follows the standard regex syntax, which may differ slightly in other programming languages.

## See Also:
- Oracle's documentation on [replaceAll() method](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#replaceAll(java.lang.String,%20java.lang.String))
- [Regular-Expressions.info](https://www.regular-expressions.info/) for learning more about regex syntax
- [W3Schools Java Strings](https://www.w3schools.com/java/java_strings.asp) for more string manipulation techniques in Java.