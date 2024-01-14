---
title:    "Kotlin recipe: Converting a string to lower case"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Why

Have you ever come across a situation where you needed to compare strings in a case-insensitive manner? Converting a string to lower case can help make the comparison process simpler and more accurate.

## How To

To convert a string to lower case in Kotlin, we can use the `toLowerCase()` function. Here's an example:

```Kotlin
val name = "John Doe"
val lowerCaseName = name.toLowerCase()
println(lowerCaseName)
```

The output of this code would be: `john doe`

We can also use the `toLowerCase()` function on a specific range of characters in a string. Here's an example:

```Kotlin
val message = "Hello World"
val lowerCaseMessage = message.substring(0, 5).toLowerCase() + message.substring(5)
println(lowerCaseMessage)
```

The output of this code would be: `hello World`

## Deep Dive

The `toLowerCase()` function in Kotlin uses the default locale to convert the string to lower case. This means that it will take into consideration the rules of the language in which the code is running. For example, in the English language, the letter "I" should always be capitalized, even when used in the middle of a word. With the default locale, the `toLowerCase()` function will still preserve this rule.

It's important to note that the `toLowerCase()` function does not modify the original string. Instead, it returns a new string with the converted characters. This is important in situations where you need to preserve the original string while also having a lower case version for comparison purposes.

## See Also

- [Kotlin String API documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [Java String toLowerCase() method documentation](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#toLowerCase())
- [Kotlin String operations guide](https://kotlinlang.org/docs/reference/basic-types.html#string-raw-strings-and-escaping)