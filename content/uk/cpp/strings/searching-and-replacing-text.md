---
title:                "Пошук та заміна тексту"
date:                  2024-01-20T17:57:27.989815-07:00
model:                 gpt-4-1106-preview
simple_title:         "Пошук та заміна тексту"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why? (Що і чому?)
Searching and replacing text is about finding specific sequences of characters in a string and swapping them out for something else. Programmers do it to manipulate data, automate editing tasks, or preprocess text for other operations. It’s a bread-and-butter skill because text processing happens in almost every app.

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
