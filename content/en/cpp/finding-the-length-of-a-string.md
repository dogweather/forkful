---
title:                "Finding the length of a string"
aliases:
- en/cpp/finding-the-length-of-a-string.md
date:                  2024-01-20T17:47:04.612161-07:00
model:                 gpt-4-1106-preview
simple_title:         "Finding the length of a string"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Finding the length of a string means determining how many characters it contains. Programmers do this to validate input, set up loops, or prepare data for certain API functions that require string size.

## How to:

C++ provides a straightforward way to find a string's length using the `length()` method of the `std::string` class. But if you're old school, you can still go with C-style strings and `strlen()`. Here's both in action:

```C++
#include <iostream>
#include <string>
#include <cstring>

int main() {
    // Using std::string
    std::string greeting = "Hello, World!";
    std::cout << "Length of string (std::string): " << greeting.length() << std::endl;

    // Using C-style string
    const char *c_greeting = "Hello, World!";
    std::cout << "Length of string (C-style): " << strlen(c_greeting) << std::endl;

    return 0;
}
```

Sample output:
```
Length of string (std::string): 13
Length of string (C-style): 13
```

## Deep Dive:

Originally, C++ inherited C-style character arrays and the accompanying `strlen()` function from C. `strlen()` calculates the length by marching through the array until it hits the null character, `'\0'`. This is a simple yet effective strategy but it can't beat the efficiency of `std::string.length()`, which typically keeps track of the length for quick retrieval.

Alternatives? Sure thing:
- You can also use `size()` method, identical to `length()` for `std::string`.
- For wide character strings, `std::wstring` and its `length()` method are your friends.
- Spicier choices include custom functions or using algorithms like `std::distance` with iterators.

Beware though, `std::string::length()` returns a `size_t` type, an unsigned integer, which can trip you up with unexpected behaviors if you mix it with signed types in expressions.

## See Also:

- C++ reference for `std::string::length()`: https://en.cppreference.com/w/cpp/string/basic_string/length
- C++ reference for `strlen()`: https://en.cppreference.com/w/cpp/string/byte/strlen
- More about `std::string` vs. C-style strings: https://www.learncpp.com/cpp-tutorial/4-4a-c-style-strings/
- For the enthusiasts wanting to go deeper into the `std::string` class: https://en.cppreference.com/w/cpp/string/basic_string
