---
title:    "Kotlin recipe: Concatenating strings"
keywords: ["Kotlin"]
---

{{< edit_this_page >}}

# Why 

String concatenation is an essential skill in any programming language, including Kotlin. It allows us to combine different strings to create a new one, which is a common task when developing any application. Whether you are building a simple text editor or a complex mobile app, understanding how to concatenate strings is crucial.

# How To

Concatenating strings in Kotlin is straightforward, thanks to its built-in functions and operators. Let's look at some examples:

```Kotlin
// Using the plus operator
val str1 = "Hello"
val str2 = "World"
val combinedString = str1 + " " + str2
println(combinedString) // Output: Hello World

// Using the plusAssign operator
val str3 = "Good"
val str4 = "bye"
str3 += " " + str4
println(str3) // Output: Good bye

// Using the concatenation operator with variables and strings
val name = "John"
val age = 25
val message = "My name is $name and I am $age years old."
println(message) // Output: My name is John and I am 25 years old.
```

As you can see, there are a few ways to concatenate strings in Kotlin. You can use the plus operator (+) or the plusAssign operator (+=) to combine two strings. Additionally, the concatenation operator (${}) allows us to use variables and strings in the same statement. 

# Deep Dive

In Kotlin, strings are immutable, meaning they cannot be changed once created. When we concatenate two strings using the plus operator, a new string is allocated in memory, which can affect performance, especially when dealing with large strings. To avoid this, we can use the StringBuilder class, which allows us to modify strings without creating new objects.

```Kotlin
val str1 = "Hello"
val str2 = "World"
val builder = StringBuilder()
builder.append(str1)
builder.append(" ")
builder.append(str2)
val combinedString = builder.toString()
println(combinedString) // Output: Hello World
```

Another important thing to note is that when we use the plus operator, the compiler will convert it to a call to the StringBuilder class, making it the preferred method for concatenation. However, for simpler scenarios, the plus operator can be more readable and concise.

# See Also
- [Kotlin Official Documentation](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Difference Between plus and plusAssign Operators](https://kotlinlang.org/docs/reference/operator-overloading.html)
- [Java vs Kotlin String Concatenation](https://www.baeldung.com/kotlin-string-concatenation-vs-string-builder)