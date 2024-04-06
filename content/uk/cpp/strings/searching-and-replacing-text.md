---
date: 2024-01-20 17:57:27.989815-07:00
description: "How to: (\u042F\u043A \u0440\u043E\u0431\u0438\u0442\u0438:) Long before\
  \ C++ got its <algorithm> library, devs would manually loop through strings for\
  \ search-and-replace. Now, we have\u2026"
lastmod: '2024-04-05T22:51:02.762747-06:00'
model: gpt-4-1106-preview
summary: "(\u042F\u043A \u0440\u043E\u0431\u0438\u0442\u0438:) Long before C++ got\
  \ its <algorithm> library, devs would manually loop through strings for search-and-replace."
title: "\u041F\u043E\u0448\u0443\u043A \u0442\u0430 \u0437\u0430\u043C\u0456\u043D\
  \u0430 \u0442\u0435\u043A\u0441\u0442\u0443"
weight: 10
---

## How to: (Як робити:)
```C++
#include <iostream>
#include <string>
#include <algorithm>

int main() {
    std::string text = "Hello, World! Let's replace 'World' with 'Ukraine'.";
    std::string from = "World";
    std::string to = "Ukraine";

    // Find starting point of the sequence you want to replace
    size_t start_pos = text.find(from);
    if(start_pos != std::string::npos) {
        // Replace it
        text.replace(start_pos, from.length(), to);
    }

    std::cout << text << std::endl; // Output: Hello, Ukraine! Let's replace 'Ukraine' with 'Ukraine'.
    return 0;
}
```

## Deep Dive (Поглиблений аналіз):
Long before C++ got its <algorithm> library, devs would manually loop through strings for search-and-replace. Now, we have `std::string::find` and `std::string::replace` for a cleaner approach.

Alternatives like regular expressions (`std::regex`) exist but are heavier and slower for simple tasks. Still, they're king for complex patterns.

On implementation: searching typically runs O(n) complexity, where n is string length. For replacing, another O(m) is added, with m being the replacement's length. Efficient for short texts or infrequent changes but can bog down with large-scale edits. Libraries like Boost can offer performance enhancements.

## See Also (Дивіться також):
- [cplusplus.com reference for std::string](http://www.cplusplus.com/reference/string/string/)
- [Regular expressions in C++](https://en.cppreference.com/w/cpp/regex)
- [Boost String Algorithms Library](https://www.boost.org/doc/libs/release/libs/algorithm/string/)
