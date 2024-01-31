---
title:                "Removing quotes from a string"
date:                  2024-01-25T20:50:29.706335-07:00
model:                 gpt-4-1106-preview
simple_title:         "Removing quotes from a string"

category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Removing quotes from a string means stripping out any instances of quote characters, either single (' ') or double (" "), from the text data you're working with. Programmers often need to do this for data cleaning, to prepare for further processing, or when the quotes themselves are not relevant to the data's meaning.

## How to:

Here's a simple way to remove both types of quotes from a string in Kotlin:

```kotlin
fun removeQuotes(input: String): String {
    return input.replace("\"", "").replace("'", "")
}

fun main() {
    val stringWithQuotes = "Kotlin \"rocks\" it's 'cool'"
    val stringWithoutQuotes = removeQuotes(stringWithQuotes)
    println(stringWithoutQuotes) // Output: Kotlin rocks its cool
}
```

And if you want to remove only one type of quote, just skip the other replace call.

```kotlin
fun removeDoubleQuotes(input: String): String {
    return input.replace("\"", "")
}

fun removeSingleQuotes(input: String): String {
    return input.replace("'", "")
}

fun main() {
    val stringWithQuotes = "Kotlin \"rocks\" it's 'cool'"
    println(removeDoubleQuotes(stringWithQuotes)) // Output: Kotlin rocks it's 'cool'
    println(removeSingleQuotes(stringWithQuotes)) // Output: Kotlin "rocks" its cool
}
```

## Deep Dive

Historically, handling strings and escaping characters has been a core part of programming, as text is a fundamental way we interface with data. Quotes within strings sometimes need to be escaped. This is indicated by a preceding backslash (e.g., `"She said, \"Hi!\""`). When processing such strings, you might need to remove the escape characters, or the quotes themselves for cleaner or more usable text.

Alternatives to the `replace` method include regex-based removal or manually parsing the string, character by character. However, regex can be overkill for simple operations and manual parsing is less efficient than using built-in string functions. Kotlin's `replace` function leverages the underlying Java's `String` `replace` method, which is well-optimized for performance.

Implementation-wise, it's worth mentioning that Kotlin is interoperable with Java, so, in effect, any operations you perform on strings are as performant as they would be in Java. It's crucial when removing quotes to be aware of edge cases, like nested quotes, which could require a more sophisticated approach, possibly utilizing regular expressions or a parser library.

## See Also

For more context on handling strings in Kotlin, you can check out the official documentation:

- [Kotlin's String documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)

For deeper dives into regular expressions and parsing in Kotlin:

- [Kotlin Regex documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/)
