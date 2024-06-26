---
date: 2024-02-03 19:02:53.042562-07:00
description: "How to: In C++, you can capitalize a string using the standard library\
  \ without the need for third-party libraries. However, for more complex or specific\u2026"
lastmod: '2024-03-13T22:45:00.342676-06:00'
model: gpt-4-0125-preview
summary: In C++, you can capitalize a string using the standard library without the
  need for third-party libraries.
title: Capitalizing a string
weight: 2
---

## How to:
In C++, you can capitalize a string using the standard library without the need for third-party libraries. However, for more complex or specific capitalization behaviors, libraries like Boost can be quite helpful. Below are examples illustrating both approaches.

### Using Standard C++ Library:
```cpp
#include <iostream>
#include <cctype> // for std::tolower and std::toupper
#include <string>

std::string capitalizeString(const std::string& input) {
    std::string result;
    bool capitalizeNext = true;

    for (char ch : input) {
        if (std::isspace(ch)) {
            capitalizeNext = true;
        } else if (capitalizeNext) {
            ch = std::toupper(ch);
            capitalizeNext = false;
        }
        result += ch;
    }

    return result;
}

int main() {
    std::string text = "hello world from c++";
    std::string capitalizedText = capitalizeString(text);
    std::cout << capitalizedText << std::endl; // Output: "Hello World From C++"
}
```

### Using Boost Library:
For more advanced string manipulation, including locale-aware capitalization, you might want to use the Boost String Algo library.

First, ensure you have the Boost library installed and configured in your project. Then you can include the necessary headers and use its features as shown below.

```cpp
#include <boost/algorithm/string.hpp>
#include <iostream>
#include <string>

int main() {
    std::string text = "hello world from c++";
    std::string capitalizedText = text;

    // capitalize first letter of each word
    boost::algorithm::to_lower(capitalizedText); // ensuring the string is in lowercase
    capitalizedText[0] = std::toupper(capitalizedText[0]); // capitalize the first character

    for (std::size_t i = 1; i < capitalizedText.length(); ++i) {
        if (isspace(capitalizedText[i - 1])) { // capitalize after a space
            capitalizedText[i] = std::toupper(capitalizedText[i]);
        }
    }

    std::cout << capitalizedText << std::endl; // Output: "Hello World From C++"
}
```

In this case, Boost simplifies some of the string manipulation tasks but still requires a custom approach for true capitalization since it mainly offers transformation and case conversion utilities.
