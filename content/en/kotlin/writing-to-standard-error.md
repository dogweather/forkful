---
title:    "Kotlin recipe: Writing to standard error"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

In the world of programming, there are numerous reasons why someone may choose to write to standard error. One main reason is for debugging purposes. When errors occur in code, they are often sent to standard error, allowing developers to identify and fix them more efficiently. Additionally, writing to standard error can be useful for logging important information or displaying warnings to the user.

## How To

To write to standard error in Kotlin, you can use the `System.err` object and the `println()` function. For example:

```Kotlin
System.err.println("An error has occurred.")
```

This code will print the specified message to standard error, which can be viewed in the console. Another option is to use the `eprint()` function from the Kotlin standard library. This function takes in any object and prints it to standard error. For example:

```Kotlin
eprint("Warning: Low battery level.")
```

This code will print the warning message to standard error. It's important to note that standard error is typically used for text-based output, so it's best to avoid formatting or using any special characters.

## Deep Dive

Behind the scenes, writing to standard error is actually writing to a separate stream than standard output. In Kotlin, standard error is known as the `stderr` stream, while standard output is known as the `stdout` stream. These streams are important for differentiating between regular console output and errors or warnings.

When using the `println()` function, the default behavior is to write to standard output. However, by using the `System` object and the `err` property, we are able to write to standard error instead. In most cases, this is all that is needed to successfully write to standard error in Kotlin.

## See Also

For more information on writing to standard error in Kotlin, you may find the following resources helpful:

- [Kotlin documentation on I/O](https://kotlinlang.org/docs/reference/keyword-reference.html#io)
- [Kotlin standard library - eprint function](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.io/eprint.html)
- [Understanding Standard Streams in Java and Kotlin](https://www.baeldung.com/java-standard-streams)

Now that you have a better understanding of writing to standard error in Kotlin, you can use this feature to improve your debugging and error handling processes in your code. Happy coding!