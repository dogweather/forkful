---
title:                "Extracting substrings"
html_title:           "Kotlin recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why?
Extracting substrings is the process of retrieving a specific part of a string from a given string. This is often used by programmers to manipulate and work with data more efficiently. By extracting substrings, programmers can easily access and modify particular sections of larger strings without having to modify the entire string.

## How to:
To extract substrings in Kotlin, we can use the `substring()` function which takes in two parameters - the starting index and the ending index. These indices specify the range of characters we want to extract from the original string. Let's look at an example:

```Kotlin
val original = "Hello, world!"
val substring = original.substring(7, 12)
println(substring)
```
Output: `world`

In the above example, the `substring()` function extracted the characters from index 7 to index 12 (not including index 12) from the original string and assigned it to the `substring` variable.

We can also pass in a single argument to the `substring()` function, which specifies the starting index and extracts all characters from that index until the end of the string. Let's see this in action:

```Kotlin
val original = "Kotlin is awesome!"
val substring = original.substring(7)
println(substring)
```
Output: `is awesome!`

## Deep Dive:
Extracting substrings is a common operation in programming languages and has been around since the early days of string manipulation. In Kotlin, there are a few alternatives to the `substring()` function such as the `subSequence()` function which returns a `CharSequence` instead of a `String`.

When implementing the `substring()` function, it's important to consider the starting and ending indices carefully. If the starting index is larger than the ending index, the function will throw an `IndexOutOfBoundsException`. Additionally, if the indices are out of range, an `IndexOutOfBoundsException` will also be thrown.

## See Also:
- [Kotlin Strings](https://kotlinlang.org/docs/reference/basic-types.html#strings)
- [Java Substring](https://docs.oracle.com/javase/7/docs/api/java/lang/String.html#substring(int,%20int)) (Kotlin is interoperable with Java, so this can also be used)
- [Subsequence vs substring in Kotlin](https://stackoverflow.com/questions/47279792/subsequence-vs-substring-in-kotlin)