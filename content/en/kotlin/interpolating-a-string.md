---
title:                "Interpolating a string"
html_title:           "Arduino recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

String interpolation in Kotlin is a smooth way to insert variables directly into strings. This technique improves readability of the code and prevents mess-ups in concatenation.

## How to:

Let's jump straight in to the code to see string interpolation in action:

```Kotlin
fun main() {
    val name = "John"
    val greeting = "Hello, $name!"
    println(greeting) // Outputs: Hello, John!
}
```

Easy right? We can also use expressions within the interpolated strings. Surround the expression by curly braces: 

```Kotlin
fun main() {
    val apples = 5
    val oranges = 3
    println("I have ${apples + oranges} fruits.") // Outputs: I have 8 fruits.
}
```

## Deep Dive

String interpolation is not unique to Kotlin. It has long been in languages like Perl, Ruby, and more recently, JavaScript and Python.

While it seems simple, it has important implementation details. Variables are not converted directly to strings. The compiler transforms the code, wrapping variables or expressions inside a `toString()` call. This ensures correct display of the value, no matter its type.

One may wonder, "Could I just concatenate the strings instead?". Sure, you can, but interpolation is easier to read and less error-prone. 

## See Also

Read more about string templates in Kotlin [here](https://kotlinlang.org/docs/basic-syntax.html#string-templates). For a more comprehensive look at different approaches to string concatenation and their performance differences in Java (and by extension, Kotlin), [this article](http://www.pellegrino.link/2015/08/22/string-concatenation-with-java-8.html) does a great job.