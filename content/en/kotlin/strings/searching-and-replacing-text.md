---
date: 2024-01-20 17:58:15.042666-07:00
description: "Searching and replacing text is like playing hide and seek with strings,\
  \ then swapping the hider with someone else. It's a common programming task, vital\u2026"
lastmod: '2024-03-13T22:45:00.035316-06:00'
model: gpt-4-1106-preview
summary: "Searching and replacing text is like playing hide and seek with strings,\
  \ then swapping the hider with someone else. It's a common programming task, vital\u2026"
title: Searching and replacing text
weight: 10
---

## What & Why?
Searching and replacing text is like playing hide and seek with strings, then swapping the hider with someone else. It's a common programming task, vital for tasks like bulk editing, data sanitization, and automating the boring stuff.

## How to:
Kotlin simplifies text manipulation through its standard library. Below, see how you use `replace` to swap out words.

```kotlin
fun main() {
    val originalText = "Kotlin is fun, Kotlin is pragmatic!"
    val newText = originalText.replace("pragmatic", "cool")

    println(newText) // Output: Kotlin is fun, Kotlin is cool!
}
```

For regex patterns:

```kotlin
fun main() {
    val regex = "Kotlin".toRegex()
    val originalText = "Kotlin is fun, Kotlin is pragmatic!"
    val newText = regex.replace(originalText, "Java")

    println(newText) // Output: Java is fun, Java is pragmatic!
}
```

## Deep Dive
Rewriting text is old as print, but in programming, it surged with early text processors. Alternatives? Sure – find & replace functions in editors, command-line tools like `sed`. In Kotlin specifically, you have regex and plain string methods at your disposal.

`replace` is straightforward for simple text; `Regex` gives you a Swiss Army knife for patterns. Regexes are powerful but trickier – they use special syntax to pattern match. Think about regex as playing Where’s Waldo, but you're crafting the rules on what Waldo wears.

Implementation gotchas? Remember, Kotlin's `String` is immutable. Methods that alter text return new strings; they don’t change the original.

## See Also
- Kotlin Documentation on `replace`: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/replace.html
- Regex in Kotlin: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-regex/
- Good old `sed`: https://www.gnu.org/software/sed/manual/sed.html
