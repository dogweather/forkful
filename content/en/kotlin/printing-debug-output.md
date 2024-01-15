---
title:                "Printing debug output"
html_title:           "Kotlin recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why

Debugging is an essential part of the software development process, and being able to print out debug output provides valuable information for understanding and fixing errors in our code. Even in the era of advanced debugging tools, the humble print statement remains a powerful and often-used tool by developers.

## How To

Printing debug output in Kotlin is straightforward. We can use the `println()` function to print a string to the console. For example:

```Kotlin
println("Debugging is important")
```

This will print the string "Debugging is important" to the console. We can also print out the value of variables by using string interpolation:

```Kotlin
val num = 10
println("The value of num is: $num")
```

This will print out "The value of num is: 10". Additionally, we can also use `print()` instead of `println()` if we don't want to add a new line after the output.

## Deep Dive

In Kotlin, we can use the `debug()` function to print out debug output instead of using `println()`. This function takes in a lambda expression as a parameter and only executes it in debug mode. This means that the code inside the lambda will only run if the app is built in debug mode. This can help improve performance in production builds by removing unnecessary debug output statements.

```Kotlin
debug {
    val message = "This will only be printed in debug mode"
    println(message)
}
```

Apart from printing to the console, there are other ways to view debug output in Kotlin. We can use logging libraries like Timber or SLF4J to log debug messages to a file for later analysis. These libraries offer more advanced features for filtering and managing the logged output.

## See Also

- [Official Kotlin Documentation on Debugging](https://kotlinlang.org/docs/reference/debugging.html)
- [Debugging tutorial for Kotlin](https://www.raywenderlich.com/5138519-kotlin-debugging-tutorial-for-android) 
- [Debugging best practices for Android developers](https://medium.com/androiddevelopers/best-practices-for-remote-debugging-70e498e40f7b)