---
title:                "Kotlin recipe: Converting a string to lower case"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

Converting a string to lower case is a common task in programming, especially when dealing with user input. By converting a string to lower case, we can ensure consistency in our data and make it easier to compare and manipulate strings.

## How To

To convert a string to lower case in Kotlin, we can use the `toLowerCase()` function. Let's take a look at an example:

```Kotlin
val name = "John Doe"
val lowercaseName = name.toLowerCase()
println(lowercaseName) // Output: john doe
```

In the code above, we first declare a string variable `name` with the value "John Doe". Then, we use the `toLowerCase()` function to convert the string to lower case and assign it to a new variable `lowercaseName`. Lastly, we use `println()` to print the output, which is "john doe" in this case.

But what if we want to convert an entire sentence or paragraph to lower case? We can do that by using the `toLowerCase()` function on a larger string:

```Kotlin
val sentence = "I LOVE TO CODE IN KOTLIN"
val lowercaseSentence = sentence.toLowerCase()
println(lowercaseSentence) // Output: i love to code in kotlin
```

As you can see, the `toLowerCase()` function converts all characters in the string to lower case.

## Deep Dive

Behind the scenes, the `toLowerCase()` function uses the `CharSequence` interface to iterate through each character in the string and convert it to lower case. The `CharSequence` interface allows us to access a sequence of characters in a string without modifying the string itself.

It's also worth noting that the `toLowerCase()` function uses the default locale for the conversion. This means that if you're working with different languages or characters, the conversion may not be what you expect. In these cases, we can use the overloaded version of the function that takes in a `Locale` parameter to specify the desired locale for the conversion.

## See Also

- [Kotlin Strings](https://kotlinlang.org/docs/strings.html)
- [CharSequence interface](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-char-sequence/index.html)
- [toLowerCase() function documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/to-lower-case.html)