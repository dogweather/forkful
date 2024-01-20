---
title:                "Searching and replacing text"
html_title:           "Arduino recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?

Searching and replacing text is a task where you identify certain strings in your code, and swap them out with others. Programmers do this to refine, refactor, and maintain their codebase efficiently.

## How to:

In Kotlin, we have methods like `replace()` and `replaceFirst()`. Here's how to use these:

```Kotlin
fun main() {
    var text = "I love apples. Apples are sweet."
    text = text.replace("apples", "oranges")
    println(text) // Prints "I love oranges. Oranges are sweet."
    
    text = "I love apples. Apples are sweet."
    text = text.replaceFirst("apples", "oranges")
    println(text) // Prints "I love oranges. Apples are sweet."
}
```

The first example replaces all instances of "apples" with "oranges". The second example replaces only the first occurrence.

## Deep Dive 

Historically, this concept traces roots to Unix `grep` and `sed`, created for extracting and manipulating strings. 

An alternative in Kotlin can be using powerful regular expressions (regex). You can find the pattern and replace text like this:

```Kotlin
fun main() {
    val regex = "apples".toRegex()
    val text = "I love apples. Apples are sweet."
    val newText = regex.replace(text, "oranges")
    println(newText) // Prints "I love oranges. Oranges are sweet."
}
```

Kotlin’s `replace()` and `replaceFirst()` are wrappers around Java’s `java.lang.String.replace()` and `java.util.regex.Matcher.replaceFirst()`. Meaning they are platform specific and might work differently on different platforms like JVM or JS.

## See Also
- Official Kotlin docs on strings: [Strings - Kotlin Programming](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/)
- Guide on Kotlin regex: [Working with regular expressions in Kotlin](https://www.baeldung.com/kotlin-regex)