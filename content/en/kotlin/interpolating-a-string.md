---
title:                "Interpolating a string"
html_title:           "Kotlin recipe: Interpolating a string"
simple_title:         "Interpolating a string"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Interpolating a string is a way to dynamically insert variables or expressions into a string. This allows for more flexible and concise code, making it a popular choice among programmers.

## How to:

To use string interpolation in Kotlin, we can use the "$" symbol followed by the variable or expression we want to insert into the string. Here's an example:

```Kotlin
val name = "John"
val greeting = "Hello $name, how are you?"
println(greeting)
```

This will print out the string "Hello John, how are you?" in the console.

String interpolation can also be used for more complex expressions. Let's say we want to display the result of adding two numbers in a string. We can do it like this:

```Kotlin
val num1 = 6
val num2 = 8
val sum = "The sum of $num1 and $num2 is ${num1 + num2}"
println(sum)
```

This will print out "The sum of 6 and 8 is 14" in the console.

## Deep Dive

String interpolation has been around for a long time, with origins in programming languages like Perl and PHP. It has now become a standard feature in most modern programming languages, including Kotlin.

An alternative to string interpolation is string concatenation, which involves using the "+" operator to combine strings and variables. However, string interpolation offers a more readable and concise way to achieve the same result.

Internally, string interpolation in Kotlin uses the StringBuilder class to combine the string and the inserted variables or expressions. This means that string interpolation is more efficient than string concatenation, especially when dealing with long strings or multiple variables.

## See Also

To learn more about string interpolation in Kotlin, you can check out the official documentation here: https://kotlinlang.org/docs/reference/basic-types.html#string-templates