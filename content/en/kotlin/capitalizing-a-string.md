---
title:    "Kotlin recipe: Capitalizing a string"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Why

Capitalizing a string may seem like a simple task in programming, but it can have a big impact on the overall functionality and readability of your code. By capitalizing a string, you can ensure consistent formatting and make the text stand out, making it easier to read and understand.

## How To

To capitalize a string in Kotlin, we can use the `capitalize()` method. Let's take a look at a simple example:

```
val name = "john"
println(name.capitalize())
```

In this example, we are creating a string called `name` and assigning it the value of "john". Then, we are calling the `capitalize()` method on the `name` string. The output of this code will be "John", with the first letter capitalized.

If we want to capitalize all the words in a string, we can use the `split()` and `joinToString()` methods. Let's see how this works:

```
val sentence = "programming is fun"
val capitalizedSentence = sentence.split(" ").joinToString(" ") { it.capitalize() }
println(capitalizedSentence)
```

In this example, we are first creating a string called `sentence` with the value of "programming is fun". Then, we are using the `split()` method to split the string at each whitespace, creating a list of words. Next, we use the `joinToString()` method to join the words back together, using a space as the separator. Within the `joinToString()` method, we are also using the `it` keyword to apply the `capitalize()` method to each word in the list. The output of this code will be "Programming Is Fun", with each word capitalized.

## Deep Dive

In Kotlin, the `capitalize()` method uses the rules of the Unicode standard to capitalize the first letter of a string. This means that it cannot only handle English characters, but also characters from other languages. For example, if we had a string with the value of "über", the `capitalize()` method would capitalize the "ü" character as well, resulting in "Über" as the output.

Additionally, the `capitalize()` method only capitalizes the first letter of a string. This can be useful in situations where you want to maintain the original formatting of a string. For example, if we had a string with the value of "iPhone", and we applied the `capitalize()` method, the output would still be "iPhone", as only the "i" would be capitalized.

## See Also

- [Kotlin String API](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/)
- [Unicode Standard](https://unicode-table.com/en/)

By using the `capitalize()` method, you can easily and consistently capitalize strings in your Kotlin code. Remember to explore the Kotlin String API for other useful methods and to always consider the Unicode standard for internationalization purposes. Happy coding!