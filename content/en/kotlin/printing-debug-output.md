---
title:    "Kotlin recipe: Printing debug output"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why

As developers, we've all been in situations where our code just doesn't seem to be working. We spend hours trying to figure out the issue, only to realize that a simple `println()` statement could have saved us so much time and frustration. That's where printing debug output comes in - it's a simple yet effective way to track the flow of your program and identify any errors or bugs.

## How To

Printing debug output in Kotlin is quite straightforward. You can use the built-in `println()` function to display messages in the console. Let's take a look at an example:

```Kotlin
fun main() {
    val name = "John"
    println("Hello $name!") // prints "Hello John!"
}
```

In the code above, we used `println()` to print a simple greeting message. We can also use it to display the values of variables or perform basic calculations. For example:

```Kotlin
fun main() {
    val num1 = 5
    val num2 = 10
    println("$num1 + $num2 = ${num1 + num2}") // prints "5 + 10 = 15"
}
```

Another useful function for printing debug output is `print()` which works the same as `println()` but does not include a line break after the message. This can be handy if you want to display multiple messages on the same line.

```Kotlin
fun main() {
    print("Hello ")
    print("World!") // prints "Hello World!"
}
```

Besides these basic functions, Kotlin also provides more advanced options for printing debug output. You can use the `Logger` class from the `java.util.logging` package to log messages to a file or customize the level of logging. This is useful for larger projects where you need more control over the debugging process.

## Deep Dive

While printing debug output may seem like a simple concept, there are some best practices and insights to keep in mind. One important thing to remember is to remove any print statements once you've solved the issue. Leaving them in your code can impact performance and lead to cluttered code.

Additionally, consider using conditional statements or logging levels to control when and where your print statements are displayed. This can help to limit the amount of output and make debugging more efficient.

Another useful tip is to use meaningful messages in your print statements. Instead of just printing variable names, include relevant information about what that variable represents or the result of a calculation. This will make it easier to understand the output and debug any issues.

## See Also

- [Official Kotlin Documentation on Basic Input and Output](https://kotlinlang.org/docs/tutorials/kotlin-for-py/basic-input-output.html)
- [Java Logging Tutorial](https://www.vogella.com/tutorials/Logging/article.html)
- [Debugging Tips and Tricks for Kotlin Developers](https://medium.com/@vladimir_milenkovic/debugging-tips-and-tricks-for-kotlin-developers-e3052fa0eb93)