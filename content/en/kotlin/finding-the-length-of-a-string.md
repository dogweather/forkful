---
title:                "Finding the length of a string"
html_title:           "Kotlin recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

Finding the length of a string is a common task in programming, especially when dealing with user input or manipulating text data. By knowing the length of a string, we can perform various operations and create more efficient and accurate programs.

## How To

Finding the length of a string in Kotlin is a simple task that can be accomplished in various ways. Here are three different approaches to finding the length of a string:

```kotlin
// First, we can use the length property of a string
val str = "Hello World"
println(str.length) // Output: 11
```

```kotlin
// Second, we can use the count() function on a string
val str = "Hello World"
println(str.count()) // Output: 11
```

```kotlin
// Lastly, we can convert the string to a character array and use the size property
val str = "Hello World"
println(str.toCharArray().size) // Output: 11
```

All three approaches will give the same result, so choose whichever one you are most comfortable with.

## Deep Dive

Behind the scenes, Kotlin uses the Java String class, which has a method called `length()` that returns the length of a string. The `length` property in Kotlin is just a shorthand for this method. The `count()` function in Kotlin is also similar to the `length()` method, but it also takes into account any special characters or emoji in the string.

Additionally, when using the character array approach, Kotlin automatically converts the string to an array of characters, which has a `size` property that returns the length of the array. This is a more direct approach that may be useful in certain situations.

Another important thing to note is that the `length` property, as well as the `count()` function, are both constants. This means that once the string is assigned to a variable, the length will remain the same regardless of any modifications to the string.

## See Also

- [Kotlin Strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Java String Class](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html)