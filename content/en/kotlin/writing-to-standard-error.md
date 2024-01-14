---
title:    "Kotlin recipe: Writing to standard error"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

## Why
When writing a Kotlin program, it is important to understand how to handle errors and exceptions. This includes knowing how to write to standard error, which is a useful tool for debugging and troubleshooting. By writing to standard error, we can get more detailed information about the error and pinpoint the exact issue in our code.

## How To
To write to standard error in Kotlin, we can use the `System.err` object. This object represents the standard error stream, where error messages are printed. To use it, we can simply use the `write` method and pass in the message we want to print. Let's look at an example:

```Kotlin
fun main() {
    try {
        val result = 10 / 0 // division by zero
        println("Result: $result")
    } catch (e: ArithmeticException) {
        System.err.write("ERROR: Cannot divide by zero!")
    }
}
```

In this example, we have a try-catch block where we attempt to divide 10 by 0, which will result in an `ArithmeticException`. Inside the catch block, we use the `System.err` object to write the error message to standard error. When we run this program, the output will look like this:

```
ERROR: Cannot divide by zero!
```

We can also use the `println` method to write to standard error, but using `System.err` is considered a better practice as it explicitly shows that we are writing to the error stream.

## Deep Dive
It's important to note that writing to standard error only works for command-line programs. If we are developing a web application, the error messages will be displayed in the console and not in standard error. We can also use `System.err` to write to a file by using a `FileOutputStream` as follows:

```Kotlin
System.setErr(FileOutputStream("error.log"))
System.err.write("ERROR: Cannot divide by zero!")
```

This will write the error message to a file called "error.log" instead of the console. It's also worth mentioning that we can change the default error output by using `System.setErr`.

## See Also
- [Kotlin Documentation on Writing to Standard Streams](https://kotlinlang.org/docs/reference/standard-input-output.html)
- [Java Documentation on Standard Streams](https://docs.oracle.com/javase/tutorial/essential/io/sysstreams.html)
- [Tutorial on Exception Handling in Kotlin](https://www.baeldung.com/kotlin/exception-handling)