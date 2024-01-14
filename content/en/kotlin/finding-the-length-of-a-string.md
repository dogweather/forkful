---
title:    "Kotlin recipe: Finding the length of a string"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Why 

Finding the length of a string is a fundamental task in any programming language. It allows us to determine the number of characters in a string, which is essential for many tasks such as data validation, string manipulation, and more.

## How To

To find the length of a string in Kotlin, we can use the `length` property. Let's say we have a string variable `myString` that contains the value "Hello, world!". We can find its length by using the following code:

```Kotlin
val myString = "Hello, world!"
val length = myString.length 
println(length) // Output: 13
```

We can also find the length of a string using the `count()` method. This method takes in a lambda expression as a parameter and counts the number of elements that satisfy the given condition. In our case, we can use it to count the number of characters in a string, like this:

```Kotlin
val myString = "Hello, world!"
val length = myString.count { it.isLetter() } 
println(length) // Output: 10
```

## Deep Dive

Both the `length` property and `count()` method return an integer value representing the length of the string. However, there are some key differences between them. 

Firstly, the `length` property is a constant-time operation, meaning that it will always take the same amount of time to find the length of a string, regardless of the string's length. On the other hand, the `count()` method is a linear-time operation, meaning that the time taken to find the length of a string will depend on the string's length.

Secondly, the `length` property only counts the number of characters in a string, including spaces and punctuation marks. However, the `count()` method allows us to define a condition and counts only the characters in the string that satisfy that condition. 

## See Also

- [Kotlin Strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Kotlin Standard Library](https://kotlinlang.org/api/latest/jvm/stdlib/)