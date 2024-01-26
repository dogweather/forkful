---
title:                "Capitalizing a string"
html_title:           "C recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizing a string means turning all the characters in the text to uppercase. Programmers do it for uniformity, emphasis, or sometimes to meet certain data standards.

## How to:
C++ offers various ways to capitalize a string, but here's a straightforward example:

```cpp
#include <iostream>
#include <algorithm>
#include <string>

std::string capitalizeString(std::string str) {
    std::transform(str.begin(), str.end(), str.begin(), ::toupper);
    return str;
}

int main() {
    std::string text = "Hello, World!";
    std::string capitalizedText = capitalizeString(text);
    std::cout << capitalizedText << std::endl;
    return 0;
}
```

Sample Output:
```
HELLO, WORLD!
```

## Deep Dive
To capitalize strings in C++, we historically relied on a loop to iterate through each character, applying the `toupper` function from `<cctype>`. 

As C++ evolved, the Standard Template Library (STL) provided algorithms like `std::transform` which can apply a function over a sequence. This style promotes cleaner code and potentially better performance due to algorithmic optimizations.

Beyond `std::transform`, there's also the option to use ranges (from C++20) which makes the code even more concise and expressive. But that's a topic for another day.

Alternatives for capitalizing strings include writing your own function or using external libraries like Boost. It really comes down to how much control you need and what dependencies you're willing to take on.

When using `std::transform`, be mindful that it directly modifies the string. If maintaining the original string's case matters, always work on a copy.

## See Also
- C++ Reference for `std::transform`: https://en.cppreference.com/w/cpp/algorithm/transform
- C++ Reference for `toupper`: https://en.cppreference.com/w/cpp/string/byte/toupper
- An overview of C++20 Ranges: https://en.cppreference.com/w/cpp/ranges
