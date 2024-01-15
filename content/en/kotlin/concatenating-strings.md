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

# Why 

If you're a developer working with strings, you've probably encountered situations where you need to combine multiple smaller strings into one larger string. This process is known as **concatenation**, and it can be a powerful tool in your coding arsenal to create dynamic and flexible strings.

# How To 

To concatenate strings in Kotlin, we can use the `+` operator or the `plus()` function. Let's take a look at both options:

```
val str1 = "Hello"
val str2 = "World"

val result = str1 + str2
println(result) //Output: HelloWorld

val result2 = str1.plus(str2)
println(result2) //Output: HelloWorld
```

In the code block above, we first created two string variables `str1` and `str2` containing "Hello" and "World" respectively. Then, we used the `+` operator and `plus()` function to concatenate the two strings into a new variable `result` and `result2`. Lastly, we used `println()` to display the concatenated strings, resulting in "HelloWorld" being printed to the console.

We can also use string templates to concatenate strings in Kotlin. String templates allow us to insert variables directly into strings. Simply add a `$` sign before the variable name enclosed in curly braces `{}` to use string templates.

```
val firstName = "John"
val lastName = "Doe"
val fullName = "$firstName $lastName"
println(fullName) //Output: John Doe
```

In the example above, we used string templates to concatenate `firstName` and `lastName` into `fullName`, resulting in "John Doe" being printed to the console.

# Deep Dive 

In Kotlin, strings are immutable, which means they cannot be changed after they are created. When we concatenate strings, a new string is created with the combined value. This can impact performance if done frequently as it requires the garbage collector to deal with the unused strings.

To avoid this performance impact, we can use the `StringBuilder` class in Kotlin. `StringBuilder` allows us to modify strings without creating new objects, making it more efficient for string concatenation.

```
val str1 = "This is "
val str2 = "a sentence."
val stringBuilder = StringBuilder(str1)
stringBuilder.append(str2)
println(stringBuilder.toString()) //Output: This is a sentence.
```

In the example above, we first created a `StringBuilder` object using the first string `str1`. Then, we used the `append()` method to add the second string `str2` to the `StringBuilder` object. Lastly, we used `toString()` to convert the `StringBuilder` object to a string and printed it to the console.

# See Also 

- [Official Kotlin Documentation on Strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Kotlin Academy - Concatenating Strings](https://blog.kotlin-academy.com/strings-concatenation-patterns-in-kotlin-d058a1b49158)
- [Codecademy - String Concatenation in Kotlin](https://www.codecademy.com/learn/learn-kotlin/modules/learn-kotlin-introduction-to-variables/cheatsheet)