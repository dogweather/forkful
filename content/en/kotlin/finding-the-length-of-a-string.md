---
title:                "Finding the length of a string"
html_title:           "Kotlin recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/kotlin/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Finding the length of a string simply means determining the number of characters in a string. Programmers often do this to validate user input, count the number of characters in a text, or manipulate strings to perform certain operations.

## How to:

```
Kotlin val myString = "Hello World!"
println(myString.length)
```

Output: 12

```
Kotlin fun findLength(str: String): Int{
    return str.length
}

println(findLength("Goodbye")) 
```

Output: 7

## Deep Dive

The concept of finding the length of a string has been around since the early days of programming languages and is still a crucial practice in modern programming. In Kotlin, the ```length``` property is available on all string variables and returns the number of characters in the string. 

However, in some cases, finding the length of a string may not be the optimal solution. For example, if you only need to check if a string is empty, using the ```isEmpty()``` function would be a more efficient approach.

It is also important to note that when using the ```length``` property, spaces and special characters are also counted as characters. This may affect your results if you are expecting a certain number of characters without considering these.

## See Also

- Kotlin Strings documentation: https://kotlinlang.org/docs/reference/basic-types.html#strings
- Alternative ways to find string length: https://www.geeksforgeeks.org/ways-to-find-length-of-strings-in-java/
- String manipulation techniques in Kotlin: https://www.baeldung.com/string-manipulation-in-kotlin