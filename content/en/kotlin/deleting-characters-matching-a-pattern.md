---
title:    "Kotlin recipe: Deleting characters matching a pattern"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

# Why 

Deleting characters that match a certain pattern can be incredibly useful in various programming scenarios. It allows us to easily manipulate strings and remove unwanted characters, making our code more efficient.

# How To

In Kotlin, we can use the `filter()` function to remove characters that match a given predicate. Let's say we have a string `hello123` and we want to remove all the numbers from it. Here's how we would do it:

```
Kotlin 
val string = "hello123"
val filteredString = string.filter { !it.isDigit() }
println(filteredString)
```

The output of this code would be `hello`, as all the numbers have been removed from the original string `hello123`.

We can also use regular expressions (RegEx) to delete characters matching a certain pattern. Let's say we have a string that contains both letters and numbers, and we only want to keep the numbers. Here's how we can achieve that using RegEx:

```
Kotlin
val string = "abc123def"
val filteredString = string.replace("[^0-9]".toRegex(), "")
println(filteredString)
```

The output here would be `123`, as all the non-numeric characters have been removed from the original string.

# Deep Dive

The `filter()` function in Kotlin is an extension function for `String` class, which means it is available on all `String` objects. It takes in a predicate as a parameter, which is a function that returns a Boolean value. The predicate is applied to each character in the string, and only those characters that return `true` are included in the filtered string.

Regular expressions, on the other hand, provide a more powerful and flexible way to manipulate strings. They allow us to specify patterns using a combination of characters, making it easier to match and remove specific characters from a string.

# See Also
- Official Kotlin documentation on `filter()` function: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/filter.html
- Regex tutorial for Kotlin: https://www.tutorialspoint.com/kotlin/kotlin_regular_expressions.htm