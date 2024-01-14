---
title:                "Kotlin recipe: Finding the length of a string"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why
Finding the length of a string is a fundamental operation in programming, regardless of the language being used. It allows us to manipulate and analyze text, and is useful for a wide range of applications.

## How To
To find the length of a string in Kotlin, we can use the built-in function `length()`.

```Kotlin
// Example string
val text = "Hello, World!"

// Finding the length using length() function
val length = text.length()

// Output
println(length)
// 13
```

In the above example, we declare a string variable named `text` and assign it the value of "Hello, World!". Then, we use the `length()` function to find the length of the string and store it in a variable named `length`. Finally, we print the value of `length` and it outputs `13`, which is the length of the string.

We can also use the `length` property of a string, which is essentially the same as using the `length()` function.

```Kotlin
// Example string
val text = "Hello, World!"

// Finding the length using length property
val length = text.length

// Output
println(length)
// 13
```

## Deep Dive
The `length()` function and `length` property count the number of characters in a string, including spaces and punctuation marks. It returns an integer value representing the length of the string.

In Kotlin, strings are treated as arrays of characters. This means that the length of a string is equal to the number of elements in that array. For example, in the string "Hello", there are five characters, and therefore, the length of the string is 5.

It is also important to note that the index of the last character in a string is always one less than the length of the string. For example, in the string "Hello", the last character "o" is at index 4, while the length of the string is 5.

## See Also
- [Kotlin Strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Kotlin Standard Functions](https://kotlinlang.org/docs/reference/lambdas.html#function-literals-with-receiver)

By understanding how to find the length of a string in Kotlin, we can now effectively use this operation in our programs. So, go ahead and explore the various ways you can manipulate and analyze strings in Kotlin with this knowledge!