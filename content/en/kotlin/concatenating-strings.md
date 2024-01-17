---
title:                "Concatenating strings"
html_title:           "Kotlin recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?

Concatenating strings is the process of combining multiple strings into one. This is often done by using the "+" operator, or by calling the "plus()" method. Programmers use this technique to create dynamic strings that can include variables or user input.

## How to:

### Using the "+" operator:

```Kotlin
val name = "John"
val greeting = "Hello " + name + "!"
print(greeting)

//Output: Hello John!
```

### Using the "plus()" method:

```Kotlin
var age = 25
val message = "I am " plus age.toString() plus " years old."
print(message)

//Output: I am 25 years old.
```

## Deep Dive:

Concatenation has been a fundamental operation in computer programming since the early days of computing. Alternatives to the "+"" operator include the "format()" method, which allows for more flexibility in formatting strings. In Kotlin, strings are immutable, meaning they cannot be changed. So, when concatenating strings, a new string object is created each time, which can impact performance for large strings.

## See Also:

- [Kotlin Strings and String Templates](https://kotlinlang.org/docs/reference/basic-types.html#strings-and-string-templates)
- [String Concatenation in Java](https://www.baeldung.com/java-string-concatenation)