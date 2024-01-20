---
title:                "Concatenating strings"
html_title:           "PHP recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?

Concatenation is the process of combining two or more strings into one. Programmers do this to manipulate and format text data in a readable and meaningful way.

## How To:

Here's how to concatenate strings in Kotlin:

```Kotlin
fun main() {
    val str1 = "Hello"
    val str2 = ", World"
    val result = str1 + str2 
    println(result) // Outputs: Hello, World
}
```

In Kotlin, you can also use the `plus` method or string templates:

```Kotlin
fun main() {
    val str1 = "Hello"
    val str2 = ", World"
    println(str1.plus(str2)) // Outputs: Hello, World

    val name = "John"
    println("Hello, $name") // Outputs: Hello, John
}
```

## Deep Dive

Historically, strings were concatenated using the `+` or `plus` operators. These methods are simple but not very efficient for large numbers of strings because they create new strings in each operation, putting pressure on memory.

For more efficient concatenation, "String Builder" is used. It doesn't create a new string object.

```Kotlin
fun main() {
    val stringBuilder = StringBuilder()
    stringBuilder.append("Hello")
    stringBuilder.append(", World")

    println(stringBuilder.toString()) // Outputs: Hello, World
}
```

Another alternative is using `joinToString` method which is especially helpful when you are working with collections:

```Kotlin
fun main() {
    val words = listOf("Hello", "World")
    val result = words.joinToString(", ") // Join words with a ', '

    println(result) // Outputs: Hello, World
}
```
  
This method doesn't create new string instances for each operation and maintains a reasonable speed even when dealing with large data.

## See Also

- [Kotlin String class documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/index.html)
- [joinToString function](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.collections/join-to-string.html)