---
title:                "Capitalizing a string"
html_title:           "Kotlin recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

String capitalization is making the first letter of a string uppercase. It's useful for formatting data, especially when formal correctness matters, like names, places, and to start sentences correctly.

## How to:

In Kotlin, there are in-built functions: `capitalize()` and `replaceFirstChar()`. Here's how to do it:

```Kotlin
// Using capitalize function
val str = "hello there!"
val capitalizedStr = str.capitalize()
println(capitalizedStr)  // Output: Hello there!

// Using replaceFirstChar function on Kotlin 1.5 and above
val newCapitalizedStr = str.replaceFirstChar { if (it.isLowerCase()) it.titlecase(Locale.getDefault()) else it.toString() }
println(newCapitalizedStr)  // Output: Hello there!
```

## Deep Dive

The `capitalize()` function was introduced in Kotlin 1.0. It does not follow Unicode titlecase rules, instead, it just converts ASCII characters. As of Kotlin 1.5, `capitalize()` is deprecated in favor of `replaceFirstChar()` function which now respects the Unicode rules for titlecasing characters.

An alternative way for string capitalization in earlier versions of Kotlin could be using the `substring()` function. Here's how:

```Kotlin
val str = "hello there!"
val capitalizedStr = str[0].toUpperCase() + str.substring(1)
println(capitalizedStr)  // Output: Hello there!
```

Keep in mind that these string functions make a new string and do not modify the original string because strings in Kotlin are immutable.

## See Also

For more information, it would be beneficial to check out:

- [Unicode Standard](https://unicode.org/standard/standard.html)
- [Kotlin capitalize() function](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/capitalize.html)
- [Kotlin replaceFirstChar() function](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace-first-char.html)