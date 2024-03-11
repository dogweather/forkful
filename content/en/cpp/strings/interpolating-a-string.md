---
date: 2024-01-20 17:50:25.560402-07:00
description: String interpolation is about inserting variables into strings. We do
  it to construct messages on the fly, personalize output, or build dynamic queries.
lastmod: '2024-03-11T00:14:34.214270-06:00'
model: gpt-4-1106-preview
summary: String interpolation is about inserting variables into strings. We do it
  to construct messages on the fly, personalize output, or build dynamic queries.
title: Interpolating a string
---

{{< edit_this_page >}}

## What & Why?
String interpolation is about inserting variables into strings. We do it to construct messages on the fly, personalize output, or build dynamic queries.

## How to:
C++ doesn't have built-in string interpolation like some other languages. You often use `std::ostringstream`, `std::format` (from C++20), or printf-style formatting.

With `std::ostringstream`:
```cpp
#include <sstream>
#include <iostream>

int main() {
    std::ostringstream message;
    int age = 30;
    message << "Hello, I am " << age << " years old.";
    std::cout << message.str() << std::endl; // "Hello, I am 30 years old."
}
```

With `std::format` (C++20):
```cpp
#include <format>
#include <iostream>

int main() {
    int age = 30;
    std::string message = std::format("Hello, I am {} years old.", age);
    std::cout << message << std::endl; // "Hello, I am 30 years old."
}
```

## Deep Dive
Before C++20, we concatenated strings with streams or sprintf, which was clunky. With the advent of `std::format`, we're catching up to modern languages like Python with their f-strings.

`std::ostringstream`: This gives us a stream-like way to build up strings. It's versatile but not the most concise. It's been the go-to for years because it's safe and easy to use.

`std::format`: Introduced in C++20, it offers Python-like formatting. It’s more readable and efficient than stream concatenation but requires newer compilers.

Alternatives exist like Boost.Format or using string concatenation, but they aren't as clean or may incur overhead.

String interpolation is sugar, but it’s sweet. It simplifies code and avoids the performance hit of repeatedly appending strings.

## See Also
- [cppreference on std::format](https://en.cppreference.com/w/cpp/utility/format)
- [cppreference on std::ostringstream](https://en.cppreference.com/w/cpp/io/basic_ostringstream)
- [Boost.Format Library](https://www.boost.org/doc/libs/release/libs/format/)
