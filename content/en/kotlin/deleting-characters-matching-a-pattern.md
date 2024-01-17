---
title:                "Deleting characters matching a pattern"
html_title:           "Kotlin recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?
Deleting characters matching a pattern is the process of removing specific characters from a string based on a set of criteria. Programmers often do this to clean up or manipulate data, as well as for data validation and formatting purposes.

## How to:
Here are some examples of how to delete characters matching a pattern in Kotlin:

```Kotlin
// Using Regex
val string = "Hello, world!"
val regex = Regex("[,!]") // match comma and exclamation mark
val result = string.replace(regex, "") // removes ',' and '!'
println(result) // prints "Hello world"

// Using replace function
val string = "This is a test."
val result = string.replace("is", "") // remove all instances of "is"
println(result) // prints "Th  a tet."

// Using filter
val string = "Hello, world!"
val result = string.filter { it != 'o' } // remove all "o"
println(result) // prints "Hell, wrld"
```
## Deep Dive:
There are multiple ways to delete characters matching a pattern in Kotlin. The first and most common method is to use Regex, short for regular expressions. This allows for more complex pattern matching, such as using wildcards and character groups, making it useful for extracting and manipulating data.

Another alternative is using the replace function, which replaces specific occurrences of a character or substring with another value. This is useful for simpler scenarios where only specific characters need to be removed.

Lastly, the filter function can be used to remove characters matching a pattern based on a condition. This allows for more control over which characters are removed, such as only removing vowels or consonants.

In terms of implementation, Kotlin provides built-in functions for all of these methods, making it easier for programmers to manipulate strings. Additionally, Kotlin also has support for Java's String class, which provides even more options for deleting characters matching a pattern.

## See Also:
To learn more about Regex in Kotlin, check out the official documentation here: https://kotlinlang.org/docs/regex.html
For a detailed explanation of how the replace function works, visit this page: https://kotlinlang.org/api/latest/jvm/stdlib/kotlin.collections/replace.html
For more information on the filter function and its applications, check out this tutorial: https://kotlinlang.org/docs/tutorials/kotlin-for-py/filter-map.html