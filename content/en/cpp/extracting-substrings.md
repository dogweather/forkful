---
title:                "Extracting substrings"
html_title:           "Arduino recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

# Extracting Substrings in C++: What & Why?

Substring extraction is a common operation in C++ where part of a string is selected, often defined by starting index and length. Programmers extract substrings to manipulate specific segments of larger strings - crucial for tasks like parsing data files, coding algorithms, or validating user input.

## How to: Extract Substrings in C++

Here's a simple example to demonstrate substring extraction in C++:

```C++
#include <iostream>
#include <string>

int main() {
    std::string str = "Hello, world!";
    std::string subStr = str.substr(7, 5);
    std::cout << subStr;
    return 0;
}
```

Running this code gives the output: 

```C++
world
```

Here, we've extracted a substring starting from 7th index till 5 characters, which is "world".

## Deep Dive: Substring Extraction

Historically, C++ has provided robust substring extraction functionality. String manipulation became more straightforward since the introduction of std::string class in C++98 and its `substr()` function. 

Although `substr` is commonly used, other alternatives exist for more specific cases. For example, you can use `std::find_first_of()` or `std::find_last_of()` to extract a substring up to or from a specific character. 

As for `substr` implementation, it returns a new string object with a copy of the substring. This is significant to note as creating new string objects in a loop can significantly impact your program performance due to memory allocation and deallocation.

## See Also 

To broaden your understanding of C++, check out these related resources:

1. [More about the `std::string` class](http://www.cplusplus.com/reference/string/string/)
2. [Understanding `std::find_first_of()` & `std::find_last_of()`](http://www.cplusplus.com/reference/string/string/find_first_of/)
3. [A guide to C++ Strings](https://www.geeksforgeeks.org/c-string-class-and-its-applications/)
4. [More on C++ performance](https://isocpp.github.io/CppCoreGuidelines/CppCoreGuidelines#S-performance)