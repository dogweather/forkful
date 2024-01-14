---
title:    "Kotlin recipe: Extracting substrings"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Why

Substrings are an essential part of string manipulation in programming. They allow us to extract smaller portions of a larger string, making it easier to work with and modify specific parts of a string. In this blog post, we will discuss the importance of extracting substrings and how to do it in Kotlin.

## How To

To extract substrings in Kotlin, we use the `substring()` function. This function takes in two parameters – the starting index and the ending index of the desired substring. The starting index is inclusive, while the ending index is exclusive.

Here is an example of extracting a substring from a string using Kotlin:

```Kotlin
// Creating a string
val str = "Hello World"

// Extracting substring from index 6 to 11
val subStr = str.substring(6, 11)

// Prints "World"
println(subStr)
```

The output of the above code would be `World`, which is the desired substring. 

We can also use the `substring()` function to extract a substring starting from a specific index to the end of the string without providing an ending index:

```Kotlin
// Creating a string
val str = "Hello World"

// Extracting substring from index 6 to end
val subStr = str.substring(6)

// Prints "World"
println(subStr)
```

In this case, the output would still be `World` as the ending index is not specified.

## Deep Dive

In Kotlin, substrings are extracted using the `substring()` function, which is a member function of the `String` class. This function takes the starting and ending indexes as parameters and returns a new `String` object that contains the desired substring. 

It is important to note that the `substring()` function does not modify the original string, but instead, it creates and returns a new string. This is useful when working with immutable strings, as it allows us to create and manipulate substrings without changing the original string.

Another important aspect to keep in mind is that the `substring()` function throws an `IndexOutOfBoundsException` if the specified indexes are out of range.

Also, the starting index must always be less than the ending index. Otherwise, the function will not return any substring.

## See Also

- [Kotlin String class documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [Official Kotlin documentation on substring extraction](https://kotlinlang.org/docs/basic-types.html#strings)
- [A guide to working with strings in Kotlin](https://www.baeldung.com/kotlin/working-with-strings)