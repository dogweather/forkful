---
title:                "Kotlin recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why
String concatenation is a fundamental concept in programming, especially when it comes to working with text data. It involves combining multiple strings together to create a longer string. This can be useful in situations such as creating a full name from separate first and last name variables or creating a URL from a base URL and query parameters. In this blog post, we will explore how to concatenate strings in Kotlin and the various methods available to do so.

## How To
Concatenating strings in Kotlin can be done in several ways. Let's take a look at some examples using the ```+``` operator and the ```plus()``` function.

```Kotlin
val firstName = "John"
val lastName = "Smith"
val fullName = firstName + " " + lastName
println(fullName) // Output: John Smith

val base = "https://example.com/"
val query = "search?q=Kotlin"
val url = base.plus(query)
println(url) // Output: https://example.com/search?q=Kotlin
```

As seen in the examples, we can use the ```+``` operator to combine strings or use the ```plus()``` function to achieve the same result. Another approach is using string templates, which allows us to embed variables directly into a string.

```Kotlin
val firstName = "John"
val lastName = "Smith"
val fullName = "$firstName $lastName"
println(fullName) // Output: John Smith
```

We can also concatenate strings using the ```StringBuilder``` class, which provides a more efficient way of manipulating strings. This is especially useful when dealing with large amounts of text data.

```Kotlin
val builder = StringBuilder()
builder.append("Kotlin")
builder.append(" is")
builder.append(" fun")
println(builder.toString()) // Output: Kotlin is fun
```

## Deep Dive
When concatenating strings, it is important to understand the difference between using the ```+``` operator and the ```plus()``` function. While the ```+``` operator is concise and easy to read, it can be less efficient when working with large amounts of data as it creates a new string object every time it is used. On the other hand, the ```plus()``` function can be more efficient as it operates on the existing string object.

Additionally, when using string templates, it is worth noting that they only work when the variables are simple names and not expressions. For example, ```"${2 + 2}"``` will not work as a string template, but ```"$variableName"``` will.

## See Also
- [Official Kotlin documentation on string concatenation](https://kotlinlang.org/docs/basic-syntax.html#string-templates)
- [Kotlin Strings and Characters - Concatenation](https://www.tutorialspoint.com/kotlin/kotlin_strings.htm)

In conclusion, understanding how to concatenate strings in Kotlin is essential for any programmer who works with text data. There are various methods available, so it is important to choose the most efficient one based on the specific situation.