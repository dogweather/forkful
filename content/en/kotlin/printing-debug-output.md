---
title:                "Printing debug output"
html_title:           "Arduino recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?

Printing debug output represents the process of displaying intermediate code results, primarily for code debugging. Programmers do it to track, understand and fix the behavior of their code during runtime.

## How To:

In Kotlin, we use the `println()` function to print debug output. See below:

```kotlin
fun main() {
    val numbers = listOf(1, 2, 3, 4, 5)

    for (number in numbers) {
        println("Number: $number")
    }
}
```

The above example generates the following output:

```
Number: 1
Number: 2
Number: 3
Number: 4
Number: 5
```

## Deep Dive

Printing debug output has a long history, originating from the earliest days of programming. Even as debugging tools have evolved, direct output remains a staple debugging technique.
 
In Kotlin, you can toggle debug output with conditional compilation flags or at runtime using `if(DEBUG)`. Additionally, remember that `println()` can impact performance in intensive loops, and to remove debug outputs in production code to avoid leaking sensitive data.

If you need advanced logging, consider using a dedicated logging library like `Log4j2` or `SLF4j`. These provide options for levels of logging (DEBUG, INFO, WARN, etc.), and output to various locations (console, file, remote).

Also, using `toString()` on objects/classes you've defined can help with debugging. Normally, it will just output the class name and its hashcode. However, you can override this to display helpful properties such as this:

```kotlin
class User (val name: String, val age: Int) {
    override fun toString() = "User(name=$name, age=$age)"
}
```

## See Also

- Debugging in IntelliJ idea: https://www.jetbrains.com/help/idea/debugging-your-first-java-application.html 
- Kotlin documentation: https://kotlinlang.org/docs/home.html 
- Log4j2: https://logging.apache.org/log4j/2.x/