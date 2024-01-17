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

## What & Why?
Debugging is a crucial part of programming, where developers track down and fix errors in their code. Printing debug output is a technique used by programmers to display information in the console during runtime, helping them understand the state of their program and locate any potential issues.

## How to:
To print debug output in Kotlin, we use the ```println()``` function, which stands for "print line". It takes a parameter or multiple parameters enclosed in parentheses and displays their values. Here's an example code:

```Kotlin
fun main() {
    val name = "John"
    val age = 27
    println("Name: $name, Age: $age")
}
```

Output:
```
Name: John, Age: 27
```

## Deep Dive:
Printing debug output has been a common practice among programmers since the early days of software development. It allows developers to inspect the inner workings of their code, see the values of variables, and trace the flow of execution.

An alternative to printing debug output is using a debugger tool, which provides a more interactive and in-depth analysis of code. However, printing debug output is still preferred by many developers due to its simplicity and ease of use.

In Kotlin, the ```println()``` function is implemented as an extension function on the ```Any``` type. This means that every type in Kotlin can access the ```println()``` function, making it a versatile tool for printing debug output.

## See Also:
To learn more about debugging in Kotlin, check out the official documentation on [debugging and testing](https://kotlinlang.org/docs/tutorials/debugging.html).

For more tips on how to effectively debug your code, take a look at this article on [debugging techniques](https://medium.com/@marcopegolotti/8-debugging-techniques-for-efficient-developers-138739e60f7f).

Happy debugging! 🚀