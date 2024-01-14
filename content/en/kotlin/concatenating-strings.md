---
title:    "Kotlin recipe: Concatenating strings"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why 

In many programming languages, including Kotlin, strings are one of the most commonly used data types. They allow us to store and manipulate text in our code, making it more readable and interactive for users. One way to manipulate strings is by concatenating them, which simply means combining two or more strings into one.

## How To

To concatenate strings in Kotlin, we can use the plus operator (+) or the "plus" function. Let's take a look at some examples:

```
// Using the plus operator
val firstName = "John"
val lastName = "Doe"
val fullName = firstName + " " + lastName
println(fullName) // Output: John Doe

// Using the "plus" function
val str1 = "Hello"
val str2 = "World"
val str3 = str1.plus(" ").plus(str2)
println(str3) // Output: Hello World
```

In addition to using the plus operator and "plus" function, we can also use the "format" function to concatenate strings in a more structured way. Here's an example:

```
// Using the "format" function
val age = 25
val name = "Alice"
val message = "Hello, my name is %s and I am %d years old".format(name, age)
println(message) // Output: Hello, my name is Alice and I am 25 years old
```

## Deep Dive 

When concatenating strings, it is important to keep in mind that both operands must be strings. If one of the operands is not a string, the compiler will automatically convert it to one. This allows us to concatenate not only strings, but also other data types such as integers and booleans.

In addition, Kotlin also provides the "toString" function which converts any data type into a string. This can be useful when concatenating an Integer or Boolean with a string. Here's an example:

```
// Using the "toString" function
val num = 10
val message = "The number is " + num.toString()
println(message) // Output: The number is 10
```

It is also possible to omit the plus operator or "plus" function and simply use string interpolation, denoted by the $ symbol, to concatenate strings in Kotlin. Here's an example:

```
val firstName = "Lisa"
val age = 30
val message = "Hi, my name is $firstName and I am $age years old"
println(message) // Output: Hi, my name is Lisa and I am 30 years old
```

## See Also 

For more information about strings and concatenation in Kotlin, check out these resources:

- [Kotlin Strings and Characters](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Kotlin in Action](https://www.manning.com/books/kotlin-in-action)
- [Official Kotlin Documentation](https://kotlinlang.org/docs/home.html)