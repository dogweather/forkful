---
title:    "Kotlin recipe: Printing debug output"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Why

Debugging is an essential part of the software development process. It helps us identify and fix errors in our code, ensuring that our programs run smoothly. However, sometimes it can be challenging to understand why our code is not working as expected. This is where printing debug output comes in handy. By printing out the values of variables or specific statements, we can get a better understanding of what's happening in our code and pinpoint any errors.

## How To

To print debug output in Kotlin, we can use the `println()` function. This function takes in a string as a parameter and prints it to the console. Let's take a look at an example:

```Kotlin
fun main() {
    val name = "John"
    println("Hello $name")
}
```

In this code, we have a variable `name` with the value "John". We then pass the string "Hello $name" to the `println()` function, where `$name` is replaced with the value of our variable. When we run this code, the output will be "Hello John". This helps us confirm that our variable has the correct value and is being used correctly in our code.

We can also print out the values of multiple variables or statements by using the `println()` function multiple times:

```Kotlin
fun main() {
    val num1 = 5
    val num2 = 10
    println(num1 + num2)
    println("$num1 + $num2 = ${num1 + num2}")
}
```

In this example, we first print out the sum of `num1` and `num2`, which will be 15. Then, we use string interpolation to print out the full equation along with the result, which will be "5 + 10 = 15". This can help us verify that our calculations are correct and identify any errors.

## Deep Dive

While printing out values with `println()` is useful for basic debugging, there are other techniques we can use to get more detailed information. For instance, we can use the `print()` function to print a string without adding a new line at the end. This can be helpful when we want to print out multiple values on the same line. Additionally, the `debug()` function allows us to print out more complex data types, such as arrays or objects.

Another powerful tool for debugging in Kotlin is the `Log` class. This class provides methods for printing log messages with different levels of severity (e.g., debug, info, error). We can use these methods to print out specific messages when certain conditions are met, allowing us to track the flow of our code and identify any issues.

## See Also

- [Kotlin Documentation: Printing to the Console](https://kotlinlang.org/docs/tutorials/kotlin-for-py/printing-console-output.html)
- [Debugging Tips and Tricks in Kotlin](https://blog.kotlin-academy.com/debugging-tips-and-tricks-in-kotlin-9fc209d7d404)
- [Kotlin Logging: A Viable Alternative to println](https://blog.kotlin-academy.com/5-minute-tutorial-on-kotlin-logging-a-viable-alternative-to-println-9cd9c846da84)