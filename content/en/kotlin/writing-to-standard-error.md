---
title:                "Writing to standard error"
html_title:           "Kotlin recipe: Writing to standard error"
simple_title:         "Writing to standard error"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## What & Why?

Writing to standard error in Kotlin allows programmers to output error messages or log information in their code. This is especially useful for debugging purposes, as it allows developers to identify and fix errors in their code.

## How to:

To write to standard error in Kotlin, use the writeText() function and specify the file "System.err". Here's an example:

```Kotlin
System.err.writeText("Error: File not found")
```

This will print the error message "Error: File not found" to the standard error stream.

## Deep Dive:

Writing to standard error is a practice that has been around since the early days of programming. Initially, programmers would use it as a way to communicate with the user during runtime. However, as development tools and debugging techniques evolved, writing to standard error became primarily used for debugging purposes.

Although writing to standard error is a common practice, there are alternatives that can also be used, such as writing to the standard output stream or using logging libraries. However, writing to standard error is still preferred for debugging due to its convenience and simplicity.

Internally, writing to standard error in Kotlin uses the standard error stream, which is used to output error messages or log information in the console. This stream is separate from the standard output stream, which is used for regular program output.