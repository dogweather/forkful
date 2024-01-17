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

## What & Why?

Regular expressions are a way for programmers to express patterns and manipulate text. Using regular expressions allows for efficient searching, matching, and replacing of text within a larger body of text. Programmers use regular expressions to quickly and accurately perform tasks such as data validation, string manipulation, and text processing.

## How to:

To use regular expressions in Kotlin, first import the Regex class:

```Kotlin
import kotlin.text.Regex
```

Then, you can create a Regex object with the pattern you want to search for:

```Kotlin
val regex = Regex("[0-9]+")
```

To match this pattern in a string, use the `find()` method and store the result in a `MatchResult?` variable:

```Kotlin
val result: MatchResult? = regex.find("I am 25 years old.")

// result?.value will return "25"
```

You can also use the `matchEntire()` method to match the entire string:

```Kotlin
val result: MatchResult? = regex.matchEntire("12345")

// result?.value will return "12345"
```

To replace text using a regular expression, use the `replace()` method:

```Kotlin
val newString = "Hello, World!".replace(Regex("[Hh]ello"), "Hi")

// newString will return "Hi, World!"
```

## Deep Dive:

The origins of regular expressions can be traced back to the 1950s, when mathematician Stephen Cole Kleene developed a notation for describing patterns in formal languages. This notation was later adapted by computer scientists and became known as regular expressions.

In Kotlin, regular expressions are represented by the `Regex` class, which is based on the Java `java.util.regex` package. This class provides various methods for performing operations on regular expressions, such as `find()`, `matchEntire()`, `matches()`, and `replace()`.

Although regular expressions are a powerful tool for text manipulation, it is important to keep in mind their performance implications. Complex regular expressions can be computationally expensive and may slow down your code. It is also worth exploring other string manipulation methods in Kotlin, such as the `replace()` and `split()` functions.

## See Also:

For more information on regular expressions in Kotlin, check out the official Kotlin documentation on [regular expressions](https://kotlinlang.org/docs/regular-expressions.html).

You can also find further resources on regular expressions in other programming languages, such as [JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions) and [Python](https://docs.python.org/3/library/re.html).

For a fun and interactive way to learn and practice regular expressions, try out [Regex Crossword](https://regexcrossword.com/).