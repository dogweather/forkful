---
title:                "Kotlin recipe: Searching and replacing text"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why
Searching and replacing text is a common task in programming. It allows us to quickly make changes to large amounts of text without having to manually go through every line. In Kotlin, this can be done easily with built-in functions and regular expressions.

## How To
To start, we will create a simple string to work with. In Kotlin, we can do this by using double quotes (`"`) around our text. For example: 

```Kotlin
val str = "Hello world! This is a sample string."
```

To search for a specific word or phrase in this string, we can use the `contains()` function. This function takes in a string as a parameter and returns a boolean value. For example, if we want to check if the string contains the word "sample", we can use:

```Kotlin
println(str.contains("sample"))
```

The output of this code would be `true`, as our string does contain the word "sample". 

To replace text in a string, we can use the `replace()` function. This function takes in two parameters: the old text and the new text. For example, if we want to replace "Hello" with "Hi" in our string, we can use:

```Kotlin
val newStr = str.replace("Hello", "Hi")
```

The value of `newStr` would now be "Hi world! This is a sample string." The original string remains unchanged.

We can also use regular expressions to search for patterns in our string. For example, if we want to replace all numbers in our string with an "X", we can use:

```Kotlin
val regexPattern = "\\d+".toRegex()
val newStr = str.replace(regexPattern, "X")
```

The output of this code would be "Hello world! This is a sample string." Regular expressions allow for more advanced and specific searching and replacing in strings.

## Deep Dive
In Kotlin, the `replace()` function actually uses regular expressions behind the scenes. This means that we can use regular expressions directly in the function without having to convert them. 

Additionally, the `replace()` function also has an optional third parameter called "ignoreCase". Setting this to true will make the function ignore upper and lower case letters when searching for the old text. This can be useful in situations where we want to replace "hello" and "Hello" with the same new text.

## See Also
- [Kotlin String documentation](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [Regular expressions in Kotlin](https://kotlinlang.org/docs/regexp.html)
- [Kotlin Playgrounds to practice searching and replacing text](https://play.kotlinlang.org/koans/overview)