---
title:    "Kotlin recipe: Converting a string to lower case"
keywords: ["Kotlin"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why
Converting a string to lower case is a common task in many programming projects. It allows for easier comparison of strings and can improve the performance of some algorithms that are case sensitive.

## How To
To convert a string to lower case in Kotlin, there are two main ways to accomplish this. The first method involves using the `toLowerCase()` function, which is a member function of the `String` class. Let's take a look at an example:

```
Kotlin val str = "HELLO" 
val lowerCaseStr = str.toLowerCase()
println(lowerCaseStr)
```

The output of this code would be "hello". As you can see, the `toLowerCase()` function converts all of the characters in the string to lowercase.

The second method involves using the `toLowerCase()` function from the `java.lang.String` class. This method works the same way as the previous one, but it can also be used on null strings without throwing an exception. Here's an example:

```
Kotlin val str: String? = null 
val lowerCaseStr = str?.toLowerCase()
println(lowerCaseStr) 
```

In this case, the output would be null. This can be useful when dealing with data that may have missing or null values.

## Deep Dive
When converting a string to lower case, it's important to understand how this function works behind the scenes. In Kotlin, strings are immutable, meaning they cannot be changed once created. So when we use the `toLowerCase()` function, a new string object is created with the converted characters.

It's also worth noting that this function uses the default locale for conversion. If you want to convert to a specific locale, you can use the `toLowerCase(Locale)` function and specify the locale you want to use.

Lastly, it's important to remember that the `toLowerCase()` function only converts alphabetic characters. Any non-alphabetic characters, such as numbers or symbols, will remain unchanged.

## See Also
- [Kotlin String class](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin/-string/)
- [Java String class](https://docs.oracle.com/javase/8/docs/api/java/lang/String.html#toLowerCase--)