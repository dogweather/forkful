---
title:                "Finding the length of a string"
html_title:           "C++ recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Finding the length of a string in programming means determining the number of characters present in a given string. This is a common task for programmers as it allows them to manipulate and work with strings in their code. It can also help with validation and error handling, making it an essential skill for any programmer.

## How to:
To find the length of a string in C++, we can use the `size()` or `length()` function from the `<string>` library. Here is an example:

```C++
#include <iostream>
#include <string>

int main() {
    std::string str = "Hello World!";
    std::cout << "Length of string: " << str.size() << std::endl;
    return 0;
}
```

Output:
```
Length of string: 12
```

Alternatively, we can also use a loop to count each character in the string until we reach the end. Here is an example of a for loop:

```C++
#include <iostream>
#include <string>

int main() {
    std::string str = "Hello World!";
    int count = 0;

    for(int i=0; i<str.length(); i++) {
        count++;
    }

    std::cout << "Length of string: " << count << std::endl;
    return 0;
}
```

Output:
```
Length of string: 12
```

## Deep Dive:
The function `size()` is preferred over `length()` as it provides a more intuitive name for the task at hand. However, they both essentially do the same thing.

Before the introduction of the `<string>` library in C++, the `strlen()` function from the `<cstring>` library was used to find the length of a string. This function only works with null-terminated strings, which means strings that end with a null character (\0). With the `<string>` library, we can work with strings of any length without the need for a null character at the end.

There are also other programming languages that have built-in functions for finding the length of a string, such as `len()` in Python and `length()` in JavaScript.

## See Also:
- [Explanation of string length in C++](https://www.learncpp.com/cpp-tutorial/6-6a-an-introduction-to-stdstring/)
- [Alternative methods for finding string length](https://www.geeksforgeeks.org/how-to-find-length-of-a-string-in-c/)
- [Official documentation for the string library in C++](https://en.cppreference.com/w/cpp/string/basic_string)