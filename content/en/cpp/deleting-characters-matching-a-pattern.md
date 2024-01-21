---
title:                "Deleting characters matching a pattern"
date:                  2024-01-20T17:41:34.742692-07:00
model:                 gpt-4-1106-preview
simple_title:         "Deleting characters matching a pattern"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why?
Deleting characters matching a pattern means stripping out specific sequences from a string. Programmers do this for cleanup, data formatting, or to meet application rules.

## How to:
Let's rip out characters using `erase` and `remove_if` alongside lambda expressions. Here's a quick example:

```cpp
#include <iostream>
#include <algorithm>
#include <string>

int main() {
    std::string data = "B4n4n4!";

    // Remove all numeric characters
    data.erase(std::remove_if(data.begin(), data.end(), ::isdigit), data.end());
    
    std::cout << data << std::endl; // Outputs: Bnn!
    
    return 0;
}
```
Sample output:
```
Bnn!
```

## Deep Dive
The `std::remove_if` algorithm from `<algorithm>` header doesn't actually shrink the string; it reorders elements and returns a pointer to the new logical end. The `erase` method of the `std::string` class then removes the "dead wood" from the end. This combo arrived with C++98 and remains efficient and popular.

Alternatives? For complex patterns, regex (`<regex>`) is your Swiss Army knife. But, it's overkill for simple chores.

Details? `std::remove_if` and algorithms alike lean on iterators, which C++ adopted from the Standard Template Library (STL) in the mid-90s. They empower generic programming, ensuring your chop-and-change code works on strings, lists, you name it.

## See Also
- C++ reference for `std::remove_if`: https://en.cppreference.com/w/cpp/algorithm/remove
- C++ reference for `std::string::erase`: https://en.cppreference.com/w/cpp/string/basic_string/erase
- More about iterators in C++: https://en.cppreference.com/w/cpp/iterator
- When to use `std::regex` for pattern matching: https://en.cppreference.com/w/cpp/regex