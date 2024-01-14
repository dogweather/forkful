---
title:                "Kotlin recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why
In programming, we often encounter situations where we need to manipulate strings. One common task is to convert a string to lowercase. By doing so, we can ensure consistency in our data and avoid any discrepancies caused by casing differences.

## How To
Converting a string to lowercase in Kotlin is quite simple. We can use the `toLowerCase()` function on a string variable to achieve this. Let's take a look at an example:

```Kotlin
val name = "JESSICA" 
println(name.toLowerCase())
```

The above code will output `"jessica"`, with the string being converted to all lowercase letters.

We can also convert a string to lowercase directly in a print statement:

```Kotlin
val name = "STEVEN" 
println("Hello, ${name.toLowerCase()}!")
```

The output of this code will be `"Hello, steven!"`.

## Deep Dive
Behind the scenes, Kotlin's `toLowerCase()` function uses the `Locale` class to determine which language and region-specific rules to apply when converting the string. If no locale is provided, the default locale of the device will be used. 

Additionally, the `toLowerCase()` function takes into account different types of characters, including accented characters and symbols. This ensures that the conversion is accurate and no characters are left unchanged.

It's also worth noting that the `toLowerCase()` function is locale-sensitive, meaning it may produce different results for different locales. This can be useful for scenarios where we need to handle multiple languages or regions.

## See Also
- [Kotlin Strings](https://kotlinlang.org/docs/basic-types.html#strings)
- [Kotlin String Functions](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/-string/#functions)

Converting a string to lowercase may seem like a small task, but it is an essential skill in programming. By understanding the inner workings of the `toLowerCase()` function, we can confidently manipulate strings and ensure consistent data in our code.