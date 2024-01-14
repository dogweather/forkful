---
title:    "Kotlin recipe: Capitalizing a string"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

Capitalizing a string may seem like a simple task in programming, but it can make a big difference in the readability and presentation of your code. By capitalizing certain words or phrases, you can easily distinguish them from the rest of your code, making it easier to understand and modify in the future.

## How To

To capitalize a string in Kotlin, you can use the `capitalize()` function. This function will capitalize just the first letter of a string, leaving the rest unchanged. Here's an example:

```Kotlin
val str = "hello kotlin"
println(str.capitalize())
```

The output of this code will be "Hello kotlin".

If you want to capitalize every word in a string, you can use the `split()` function to split the string into a list of words, then use the `capitalize()` function on each word in the list. Then, you can join the list back into a string using the `joinToString()` function. Here's an example:

```Kotlin
val str = "hello kotlin"
val words = str.split(" ")
val capitalized = words.map { it.capitalize() }
println(capitalized.joinToString(" "))
```

The output of this code will be "Hello Kotlin".

## Deep Dive

Behind the scenes, the `capitalize()` function uses `toUpperCase()` and `toLowerCase()` functions to capitalize the first letter and then convert the rest of the letters to lowercase. This ensures that the original casing of the letters is retained.

However, it's important to note that the `capitalize()` function only works for ASCII characters. If your string contains any non-ASCII characters, they will not be affected by this function. In these cases, you may need to use a more specialized function depending on your specific needs.

## See Also

For more information on working with strings in Kotlin, check out these resources:

- [Kotlin Strings](https://kotlinlang.org/docs/strings.html)
- [Kotlin String Functions](https://www.geeksforgeeks.org/kotlin-string-functions/)
- [Kotlin String Formatting](https://www.baeldung.com/kotlin/string-formatting)