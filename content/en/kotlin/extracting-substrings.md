---
title:                "Kotlin recipe: Extracting substrings"
programming_language: "Kotlin"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why 
Substrings, or smaller pieces of a larger string, are commonly used in programming to manipulate and extract specific information from text. Extracting substrings can be extremely useful for tasks such as data parsing, form validation, and text processing. In Kotlin, there are several built-in methods and functions that make extracting substrings a breeze.

## How To
To extract a substring in Kotlin, we can use the `substring()` method. Let's take a look at a simple example:

```Kotlin
val str = "Hello World!"
val substring = str.substring(0, 5)
println(substring)
```

In this code, we first create a string containing the phrase "Hello World!". Then, using the `substring()` method, we extract the first 5 characters, which gives us the output of "Hello". The `substring()` method takes in two parameters - the starting index and the ending index. In this case, we specify the starting index as 0 and the ending index as 5, which gives us the first 5 characters of the string.

We can also use the `subSequence()` method to achieve the same result:

```Kotlin
val str = "Hello World!"
val substring = str.subSequence(0, 5)
println(substring)
```

Both `substring()` and `subSequence()` return a `String` type as the output.

We can also use the `lastIndexOf()` method to extract a substring from the end of a string:

```Kotlin
val str = "Hello World!"
val substring = str.substring(str.lastIndexOf("o"))
println(substring)
```

In this example, we use the `lastIndexOf()` method to find the index of the last occurrence of the letter "o" in the string. We then pass this index as the starting index in the `substring()` method, which gives us the output of "o World!".

## Deep Dive
Under the hood, the `substring()` method uses the `get()` method of the `String` class to extract the substring. This method takes in an index and returns a `Char` at that index. It then loops through the string, adding each `Char` to a `StringBuilder` until it reaches the specified ending index. This `StringBuilder` is then converted to a `String` and returned as the output.

Additionally, both `substring()` and `subSequence()` have overloaded versions that take in only one parameter - the starting index. In this case, the substring will be extracted from the starting index to the end of the string.

## See Also
- [Kotlin String Manipulation](https://www.geeksforgeeks.org/kotlin-string-manipulation/)
- [Kotlin Strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Kotlin Standard Functions](https://kotlinlang.org/docs/reference/lambdas.html#scope-functions)