---
title:                "Handling errors"
date:                  2024-01-21T21:19:08.961743-07:00
model:                 gpt-4-1106-preview
simple_title:         "Handling errors"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/handling-errors.md"
---

{{< edit_this_page >}}

## What & Why?
Handling errors is how your code deals with problems that pop up during execution—like handling a curveball without dropping it. Programmers do it to prevent crashes and give users a smooth experience.

## How to:
Kotlin provides `try`, `catch`, `finally`, and `throw` to manage errors. Here's how you use them:

```Kotlin
fun main() {
    val numerator = 10
    val denominator = 0

    try {
        val result = numerator / denominator
        println("Result: $result")
    } catch (e: ArithmeticException) {
        println("Can't divide by zero, buddy.")
    } finally {
        println("This happens no matter what.")
    }
}
```

Output:
```
Can't divide by zero, buddy.
This happens no matter what.
```

If something goes wrong in the `try` block, execution zips to the `catch`. It catches the specific error thrown (`ArithmeticException` in this case). The `finally` block runs after—no matter the outcome.

## Deep Dive
The `try-catch` block has been a thing since early programming days—it's like a safety net. Kotlin also offers `throw` for manually tossing an exception into the ring, and there's `finally` for code that's gotta run—clean-up work, often.

Alternatives include the `Result` type and Kotlin's `try` as an expression.

```Kotlin
val result: Result<Int> = try {
    Result.success(numerator / denominator)
} catch (e: ArithmeticException) {
    Result.failure(e)
}
```
This approach returns a `Result` object—you get either a success or a failure without the drama of an unhandled exception.

Implementation in Kotlin is neat because you can use `try` like an expression, meaning it returns a value. Choices like these make error handling in Kotlin pretty versatile. It's about choosing the right tool for the job, just like you would in a workshop.

## See Also
- Kotlin docs on Exceptions: [Kotlin Exception Handling](https://kotlinlang.org/docs/exception-handling.html)
- Kotlin `Result` type docs: [Kotlin Result](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-result/)
- Effective Java, 3rd Edition, by Joshua Bloch—great insights on exceptions, though it's Java-specific.
