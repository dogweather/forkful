---
title:                "Using regular expressions"
html_title:           "Kotlin recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why
Regular expressions, commonly known as regex, are powerful tools for searching and manipulating text in programming languages. They allow for more complex and flexible pattern matching, making tasks like data validation, parsing, and text manipulation much easier and more efficient.

## How To
To use regular expressions in Kotlin, you first need to import the `kotlin.text.Regex` class. Then, you can create a regex pattern by enclosing it in forward slashes (`/`), similar to other languages like Javascript. Here's an example of a simple regex pattern that matches words starting with "kotlin":

```Kotlin
val pattern = Regex("/kotlin\\w+/")
val text = "Kotlin rocks!"
val matchResult = pattern.find(text)
println(matchResult?.value) // Output: "Kotlin"
```
In the above code, we create a regex pattern using the `Regex` class, passing in the desired pattern enclosed in forward slashes. Then, we use the `find()` function to search for the pattern in the given text and store the result in a variable. Finally, we use the `value` property to get the actual matched text and print it out.

But regex in Kotlin is not just limited to simple patterns. You can use special characters to create more complex and specific patterns. For example, `\d` matches any digit, `+` matches one or more instances, `*` matches zero or more instances, and `?` makes the preceding character optional. Combining these characters, we can create a pattern to match a phone number:

```Kotlin
val pattern = Regex("/^\\d{3}-\\d{3}-\\d{4}\$/")
val text = "123-456-7890"
val isMatch = pattern.matches(text)
println(isMatch) // Output: true
```

## Deep Dive
Regular expressions can also be used for replacing text. The `replace()` function can be used to replace all instances of a given pattern in a text with a specified string. Here's an example:

```Kotlin
val text = "I love Kotlin!"
val pattern = Regex("/Kotlin/")
val replacedText = pattern.replace(text, "Java")
println(replacedText) // Output: "I love Java!"
```

But if we want to replace only the first instance, we can use the `replaceFirst()` function instead.

Regex in Kotlin also supports grouping and capturing specific parts of a text. This can be done by enclosing the desired part of the pattern in parentheses `(` and `)`. Here's an example:

```Kotlin
val text = "My birthday is on 15-08-1995."
val pattern = Regex("/(\\d{2})-(\\d{2})-(\\d{4})/")
val matchResult = pattern.find(text)
println(matchResult?.groups?.get(3)?.value) // Output: "1995"
```

In the above code, we use parentheses to create three capture groups for the day, month, and year in a date format. Then, we access the groups using the `groups` property and print out the value of the third group, which is the year.

## See Also
- [Kotlin Regex documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
- [Regular Expressions Quick Start Guide](https://www.regular-expressions.info/quickstart.html)
- [Regex101 tool for testing and learning regular expressions](https://regex101.com/)