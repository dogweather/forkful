---
title:                "Kotlin recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Substring extraction is a common task in programming, whether you are working with strings, arrays, or lists. It allows you to retrieve a specific portion of a larger data structure, which can be useful for a variety of purposes such as data processing, text manipulation, or data analysis. In this blog post, we will explore how to efficiently extract substrings in Kotlin.

## How To

To extract substrings in Kotlin, we can use the `substring()` function. Let's take a look at a simple example:

```Kotlin
val string = "This is a test string"
val substring = string.substring(5, 8)
println(substring)
```

The `substring()` function takes two parameters - the starting index and the ending index of the desired substring. In this example, we are extracting the characters from index 5 to index 8, which gives us the word "is". The output of this code will be:

```
is
```

We can also use the `substring()` function with variables, as shown in the following example:

```Kotlin
val string = "Kotlin is a powerful language"
val start = 8
val end = 18
val substring = string.substring(start, end)
println(substring)
```

Here, we are extracting the characters between the 8th and 18th index of the string variable, giving us the word "powerful". The output will be:

```
powerful
```

It is also possible to extract substrings starting from a specific index until the end of the string, by omitting the second parameter:

```Kotlin
val string = "I love Kotlin"
val substring = string.substring(7)
println(substring)
```

This code will extract all the characters from index 7 (including) until the end of the string, giving us "Kotlin" as the output.

## Deep Dive

Under the hood, the `substring()` function in Kotlin uses the `subSequence()` method, which returns a `CharSequence` object. This means that we can use the `substring()` function not only on strings, but also on other data structures that implement the `CharSequence` interface, such as arrays, lists, or custom classes.

We can also use negative indices in the `substring()` function, which counts backwards from the end of the string. So, a starting index of -1 would refer to the last character of the string.

It is important to note that the `substring()` function returns a new string each time it is called, as strings in Kotlin are immutable. If you need to make frequent substring extractions from the same string, it might be more efficient to convert it into a `StringBuilder` and use the `substring()` function on it.

## See Also

- [Kotlin Strings](https://kotlinlang.org/docs/strings.html)
- [Kotlin Standard Library - Substrings](https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.text/substring.html)
- [JavaDoc - CharSequence Interface](https://docs.oracle.com/en/java/javase/11/docs/api/java.base/java/lang/CharSequence.html)