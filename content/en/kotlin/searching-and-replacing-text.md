---
title:                "Kotlin recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why
Searching and replacing text is a common task in programming that allows us to quickly and efficiently change specific words or characters within a larger body of text. This can save us time and effort, especially when dealing with large amounts of data.

## How To
To replace text in Kotlin, we can use the `replace()` function which takes in a regular expression as the first parameter and the replacement string as the second parameter. Below is an example of replacing the word "dog" with the word "cat" in the string "I love my dog":

```Kotlin
val str = "I love my dog"
val newStr = str.replace(Regex("dog"), "cat")
println(newStr) // Outputs: "I love my cat"
```

We can also use named groups in our regular expression for more precise replacement. For example, if we want to replace "dog" with "cat" only if it is preceded by the word "my", we can use the following code:

```Kotlin
val str = "I love my dog"
val newStr = str.replace(Regex("(?<=my )dog"), "cat")
println(newStr) // Outputs: "I love my cat"
```

The `replace()` function can also take in a lambda as the second parameter, allowing us to perform custom replacements. For example, if we want to capitalize every word in a string, we can use the following code:

```Kotlin
var str = "hello world"
val newStr = str.replace(Regex("[a-zA-Z]+")) {
    it.value.capitalize()
}
println(newStr) // Outputs: "Hello World"
```

## Deep Dive
Regular expressions, or regex, are patterns used to match and manipulate text. They are very powerful tools for searching and replacing text, but they can also be complex. Understanding the basics of regex is essential for effective text manipulation.

In Kotlin, we use the `Regex` class to create a pattern to match against. The `replace()` function takes in this regex as its first parameter and performs the replacement based on the given pattern.

There are many different symbols and rules in regex that allow for precise search and replace operations. Some useful symbols to know include `\w` for matching any word character, `+` for matching one or more instances, and `?` for making a pattern optional.

It's always important to test and validate your regex before using it in your code. You can use online regex testers or Kotlin's `Matcher` class to see what parts of a string your pattern will match.

## See Also
- Official Kotlin `Regex` documentation: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/
- Online regex tester: https://regex101.com/
- Tutorial on basic regex: https://www.regular-expressions.info/tutorial.html