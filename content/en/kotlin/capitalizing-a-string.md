---
title:                "Capitalizing a string"
html_title:           "Kotlin recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

Capitalizing a string in a program is a common task, especially when dealing with user input or data manipulation. By capitalizing a string, you can ensure consistency in your output and make it more readable for users.

## How To

Capitalizing a string in Kotlin is a simple process. Here's an example of how you can do it:

```
val name = "jessica"
println(name.capitalize()) // Output: Jessica
```

In the above code, the `capitalize()` function is used to capitalize the first letter of the string. This is a built-in function in Kotlin that can be applied to any string variable.

If you want to capitalize every word in a string, you can use the `capitalizeWords()` function, like this:

```
val sentence = "my favorite color is blue"
println(sentence.capitalizeWords()) // Output: My Favorite Color Is Blue
```

The `capitalizeWords()` function is not a built-in function, so you'll need to create your own extension function for it. You can do this by adding the following code to your program:

```
fun String.capitalizeWords(): String = split(" ").map { it.capitalize() }.joinToString(" ")
```

You can also use the `toUpperCase()` function to capitalize the entire string, regardless of the original casing, like this:

```
val word = "hello"
println(word.toUpperCase()) // Output: HELLO
```

## Deep Dive

Kotlin has several useful built-in string functions that can help with capitalization. Here are some of the most commonly used ones:

- `capitalize()` - Capitalizes the first letter of the string
- `decapitalize()` - Converts the first letter of the string to lowercase
- `toUpperCase()` - Converts all characters in the string to uppercase
- `toLowerCase()` - Converts all characters in the string to lowercase
- `capitalizeWords()` - Capitalizes the first letter of every word in the string

Additionally, as shown in the `capitalizeWords()` example earlier, you can create your own extension functions to make your code more concise and reusable.

When dealing with strings, it's important to keep in mind the difference between `capitalize()` and `toUpperCase()` functions. The `capitalize()` function only capitalizes the first letter while the `toUpperCase()` function capitalizes all letters, even if they were originally uppercase.

## See Also

- [Kotlin Strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Kotlin Standard Library](https://kotlinlang.org/api/latest/jvm/stdlib/index.html)
- [Kotlin Official Website](https://kotlinlang.org/)