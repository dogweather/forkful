---
title:                "Concatenating strings"
html_title:           "PHP recipe: Concatenating strings"
simple_title:         "Concatenating strings"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why?
In C++, concatenating strings simply refers to gluing two or more strings together in order to generate a new string. Programmers do this to form dynamic messages, build text content, or format data for output.

## How to:
Here is a basic example of how to concatenate strings in C++. We'll take two strings and glue them together using the `+` operator.

```C++
#include <iostream>
#include <string>

int main() {
    std::string str1 = "Hello, ";
    std::string str2 = "World!";
    std::string str3 = str1 + str2;
    std::cout << str3;
    return 0;
}
```

This code will output:

```C++
Hello, World!
```

## Deep Dive
## Historical Context:
Historically, C++ is derived from the C language which did not have a native string type. In C, strings were arrays of characters, and concatenation was a little bit more complex. But with the advent of C++, string manipulation got simpler with the `std::string` class and the `+` operator for concatenation.

## Alternatives:
If you're more comfortable with C-style char arrays, or dealing with something where you can't use `std::string`, you can use `strcat()` function from cstring header.

```C++
#include <cstring>
#include <iostream>

int main() {
    char str1[50] = "Hello, ";
    char str2[] = "World!";
    strcat(str1, str2);
    std::cout << str1;
    return 0;
}
```
Bear in mind using `strcat` can lead to security issues if not handled properly, as it does not perform bounds checking.

## Implementation Details:
The C++ `std::string` class overloads the `+` operator for string concatenation. When you use `+` to concatenate two strings, C++ creates a new `std::string` object to hold the result. This involves a dynamic memory allocation, which can be a bit of a performance concern in a hot loop.

## See Also
Want to dive deeper into C++ string manipulation? Here are some resources that might help. 

- [Cplusplus.com String Library Documentation](http://www.cplusplus.com/reference/string/string/)
- [C++ Super-FAQ String Operations](https://isocpp.org/wiki/faq/strings)
- [Modifying strings in C++ - Stack Overflow](https://stackoverflow.com/questions/3899900/c-modifying-string-in-function)