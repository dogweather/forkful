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

# Why
Substring extraction is a common task in programming, especially when dealing with text data. It allows developers to access and manipulate specific parts of a string, making it a powerful tool for data processing and manipulation.

# How To

```Kotlin
// Example string
val carModel = "Tesla Model S"

// Extracting a substring starting at index 6
val model = carModel.substring(6)

// Output: Model S

// Extracting a substring from index 0 to 5
val brand = carModel.substring(0, 5)

// Output: Tesla

// Extracting a substring using ranges
val number = carModel.substring(6..-1)

// Output: Model S

```

The `substring()` function in Kotlin takes two parameters: the starting index and the ending index. If the ending index is not specified, the function will return a substring starting from the specified index until the end of the string. It can also be used with ranges, as shown in the last example.

# Deep Dive

There are a few important things to note when working with substring extraction. First, the indexing in Kotlin starts at 0, so the first character in a string has index 0. Additionally, the ending index is non-inclusive, meaning the substring will end at the index before the specified one.

It's also worth noting that substring extraction returns a new string and does not modify the original string. This makes it a safe and non-destructive operation.

# See Also

- Kotlin Strings: https://kotlinlang.org/docs/reference/basic-types.html#strings
- Official Kotlin Documentation: https://kotlinlang.org/docs/home.html