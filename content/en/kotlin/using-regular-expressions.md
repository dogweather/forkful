---
title:                "Kotlin recipe: Using regular expressions"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

Regular expressions, also known as regex, are powerful tools that allow programmers to efficiently search, manipulate, and validate text. They are commonly used in tasks such as data cleaning, text parsing, and form validation.

## How To

Using regex in Kotlin is simple and straightforward. First, we need to create a Regex object by passing in a string pattern to search for. For example, if we want to find all words that contain "cat", our regex pattern would be "\w*cat\w*". Next, we can use the matches() function to check if a given string matches our regex pattern.

```
Kotlin 
val regex = Regex("\w*cat\w*")
val stringToCheck = "I love my cat, she's the best!"
println(stringToCheck.matches(regex))
```

**Output:** true

We can also use regex to replace parts of a string with a desired value. This is especially useful for data cleaning and formatting. For example, if we want to replace all numbers in a string with "X", we can use the replace() function.

```
Kotlin
val regex = Regex("\\d+")
val stringToClean = "My phone number is 123-456-7890."
println(stringToClean.replace(regex, "X"))
```

**Output:** My phone number is X-X-X.

## Deep Dive

Regular expressions have a rich set of symbols and operators that allow for complex pattern matching. Some commonly used symbols include:

- **\w** : Matches any word character (letters, numbers, and underscores)
- **\s** : Matches any whitespace character (spaces, tabs, and line breaks)
- **\d** : Matches any digit character
- **[abc]** : Matches any single character within the brackets
- **^** : Matches the beginning of a string
- **$** : Matches the end of a string

Regular expressions also have quantifiers that allow for repetition. Some examples are:

- **+** : Matches one or more occurrences of the preceding expression
- **\*** : Matches zero or more occurrences of the preceding expression
- **?** : Matches zero or one occurrence of the preceding expression
- **{n}** : Matches exactly n occurrences of the preceding expression

It is important to note that regex patterns are case sensitive by default. However, we can use the ignoreCase flag to make our patterns case-insensitive.

## See Also

- [Kotlin Regex Documentation](https://kotlinlang.org/docs/regex.html)
- [Regular Expressions Tutorial - How to Get Started](https://www.regular-expressions.info/tutorial.html)
- [Mastering Regular Expressions Book by Jeffrey E.F. Friedl](https://www.oreilly.com/library/view/mastering-regular-expressions/0596528124/)