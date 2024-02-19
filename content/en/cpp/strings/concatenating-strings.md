---
aliases:
- /en/cpp/concatenating-strings/
date: 2024-01-20 17:34:09.900750-07:00
description: "String concatenation is stitching together two or more strings end-to-end.\
  \ Programmers do it to build sentences, create messages, or combine input data\u2026"
lastmod: 2024-02-18 23:09:11.347623
model: gpt-4-1106-preview
summary: "String concatenation is stitching together two or more strings end-to-end.\
  \ Programmers do it to build sentences, create messages, or combine input data\u2026"
title: Concatenating strings
---

{{< edit_this_page >}}

## What & Why?
String concatenation is stitching together two or more strings end-to-end. Programmers do it to build sentences, create messages, or combine input data for processing or display.

## How to:
In C++, we’ve got a few ways to concatenate strings. Here's a taste using `std::string` and the plus (`+`) operator:

```cpp
#include <iostream>
#include <string>

int main() {
    std::string hello = "Hello, ";
    std::string world = "World!";
    
    std::string greeting = hello + world;
    
    std::cout << greeting << std::endl; // Outputs: Hello, World!
    return 0;
}
```

Quick and simple, yeah? But, we can also use `append()`:

```cpp
#include <iostream>
#include <string>

int main() {
    std::string hello = "Hello, ";
    hello.append("World!");
    
    std::cout << hello << std::endl; // Outputs: Hello, World!
    return 0;
}
```

Or even `operator+=` if you feel like it:

```cpp
#include <iostream>
#include <string>

int main() {
    std::string hello = "Hello, ";
    hello += "World!";
    
    std::cout << hello << std::endl; // Outputs: Hello, World!
    return 0;
}
```

## Deep Dive
Historically, C++ took over from C, which used character arrays and functions like `strcat()` for string work. It was messier and more error-prone.

Modern C++ improved the scene with `std::string`. It's safer, easier to read, and gives you options. If `std::string` isn't your jam, there’s `std::stringstream` or even `std::format` (from C++20) for the formatting fans.

Under the hood, concatenating strings involves memory allocation and copying. Done carelessly, it can hit your program’s performance like a brick. Smart pointers and move semantics alleviate some pain here.

Let's not forget about the alternatives - libraries like Boost, or handling UTF-8 with `std::string_view` for zero-copy operations on modern C++.

## See Also
- C++ reference for `std::string`: https://cplusplus.com/reference/string/string/
- C++ Working Draft, Standard for Programming Language C++: http://www.open-std.org/JTC1/SC22/WG21/docs/papers/2020/n4861.pdf
- Learn more about `std::format`: https://en.cppreference.com/w/cpp/utility/format
- Boost library documentation: https://www.boost.org/doc/libs/1_75_0/libs/string_algo/doc/html/index.html
