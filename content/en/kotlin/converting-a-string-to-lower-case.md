---
title:                "Converting a string to lower case"
html_title:           "Kotlin recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why
Converting a string to lower case is a common task in programming. It is useful for normalizing text for comparison, sorting, and other operations that require consistent casing. In this article, I will explain how to easily convert a string to lower case using Kotlin.

## How To

To convert a string to lower case in Kotlin, we will use the `toLowerCase()` function. Let's take a look at some examples.

```Kotlin
val name = "John Doe"
val lowerCase = name.toLowerCase()

println(lowerCase) // output: john doe
```

In the code above, we declare a string variable `name` with the value "John Doe". Then, we call the `toLowerCase()` function on the string and assign the result to a new variable `lowerCase`. Finally, we print out the converted string to the console using `println()`.

We can also use the `toLowerCase()` function directly on a string literal.

```Kotlin
val greeting = "Hello".toLowerCase()

println(greeting) // output: hello
```

It is important to note that the `toLowerCase()` function does not modify the original string, but instead returns a new string with all characters converted to lower case.

## Deep Dive

Behind the scenes, the `toLowerCase()` function uses the `Locale` class to determine the rules for converting characters to lower case. This ensures that the conversion is consistent with language-specific rules and standards.

Additionally, the `toLowerCase()` function can be used on strings in any language, not just English. It will take into account the specific characters and rules of that language, making it a versatile and reliable tool.

## See Also
- [Kotlin Strings](https://kotlinlang.org/docs/strings.html)
- [Locale Class](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-locale/)
- [String Manipulation in Kotlin](https://www.baeldung.com/kotlin-string-manipulation)