---
title:                "Kotlin recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

Finding the length of a string is a fundamental skill in programming. It allows us to manipulate strings and perform operations based on their length, such as validating user input or creating dynamic text-based applications.

## How To

To find the length of a string in Kotlin, we can use the built-in `length` property. This property returns the number of characters in a string. Let's see an example:

```Kotlin
val name = "John"
val length = name.length

println("The length of the name is $length") // Output: The length of the name is 4
```

We can also use the `count()` function to get the length of a string. This function takes an optional parameter which represents the predicate for counting elements. In the case of a string, we can simply omit the parameter as it will count all the characters in the string. Here's an example:

```Kotlin
val message = "Hello world"
val length = message.count()

println("The length of the message is $length") // Output: The length of the message is 11
```

However, it's important to note that the `length` property and `count()` function may produce different results if the string contains special characters or emojis. This is because the `length` property counts the number of Java characters, while the `count()` function counts the number of Unicode code points.

## Deep Dive

Java characters are a subset of Unicode code points and are typically used in older programming languages. They represent a single character or special symbol. On the other hand, Unicode code points are used to represent a broader range of characters, including emojis and other special characters.

In Kotlin, a string is represented as a sequence of Unicode code points. This means that the `length` property will return the number of Unicode code points, while the `count()` function will return a more accurate count of characters, regardless of their representation as a Java character or Unicode code point.

## See Also

- [Kotlin Strings](https://kotlinlang.org/docs/strings.html)
- [Java Characters and Unicode](https://www.baeldung.com/java-characters-unicode) 
- [Understanding Kotlin Strings and Their Manipulations](https://medium.com/@deepakkumardasreddy/understanding-kotlin-strings-and-their-manipulation-e48d9fbd5498)