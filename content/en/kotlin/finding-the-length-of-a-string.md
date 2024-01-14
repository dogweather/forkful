---
title:    "Kotlin recipe: Finding the length of a string"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why
Finding the length of a string is a common and essential task in programming. It allows us to manipulate and analyze text in our programs, making them more dynamic and useful.

## How To
To find the length of a string in Kotlin, we can use the `length` property. Let's take a look at an example:

```Kotlin
val string = "Hello World"
val length = string.length
println(length) // Output: 11 
```

In the above code, we first create a string variable called `string` and assign it the value "Hello World". Then, we use the `length` property to find the length of the string, which is 11 in this case. Finally, we print out the length and see the result in the console.

We can also use the `length` property on user input. Let's see how that works:

```Kotlin
print("Enter a word: ")
val input = readLine()
val length = input?.length // Use safe call operator in case of null input
println(length) // Output: Length of the input word
```

In the above code, we ask the user for a word and store it in the `input` variable using the `readLine()` function. Then, we use the `length` property to find the length of the input word. It is important to note that the `length` property only works on nullable strings, which is why we use the safe call operator `?.` to handle null input.

## Deep Dive
Behind the scenes, the `length` property in Kotlin uses the `length` property in Java's `String` class. This property returns an integer value representing the number of characters in the string. In some programming languages, the length of a string is determined by the number of bytes it takes up. However, in Kotlin, the `length` property counts the number of Unicode code points in the string, making it more accurate.

It is also worth noting that the `length` property is a read-only property, which means we cannot modify it or change the length of a string directly. However, we can manipulate strings and change their length using other methods and functions.

## See Also 
- [Kotlin Strings Documentation](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Java String length() Method](https://www.w3schools.com/java/ref_string_length.asp)

Finding the length of a string is a fundamental skill in programming, and it is important to understand how it works in Kotlin. With the knowledge gained from this article, you can confidently manipulate and use strings in your Kotlin programs. Happy coding!