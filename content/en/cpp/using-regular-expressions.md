---
title:                "Using regular expressions"
date:                  2024-01-19
simple_title:         "Using regular expressions"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?

Regular expressions are patterns used for matching character combinations in text. Programmers use them for tasks like validation, search, and text manipulation due to their power and flexibility.

## How to:

To use regular expressions in C++, you'll need to include the `<regex>` library. Here’s how you match, search, and replace text:

```cpp
#include <iostream>
#include <regex>

int main() {
    std::string target("Hello World. This is a regex test.");
    
    // Match
    std::regex match_pattern("Hello World");
    bool is_match = std::regex_match(target, match_pattern);
    std::cout << (is_match ? "Matched" : "Not matched") << "\n";
    
    // Search
    std::regex search_pattern("\\bis\\b");
    std::smatch matches;
    if (std::regex_search(target, matches, search_pattern)) {
        std::cout << "Found: " << matches[0] << "\n";
    }

    // Replace
    std::regex replace_pattern("World");
    std::string result = std::regex_replace(target, replace_pattern, "Universe");
    std::cout << "After replace: " << result << "\n";
    
    return 0;
}
```

Sample output:

```
Matched
Found: is
After replace: Hello Universe. This is a regex test.
```

## Deep Dive

Regular expressions have been a part of computer science since the 1950s, popularized by utilities like grep in Unix. C++ adopted them much later, with std::regex in C++11. Native support varies by compiler; some may lag in full regex feature support.

Alternatives to `std::regex` include libraries like Boost.Regex or PCRE (Perl Compatible Regular Expressions). Boost.Regex, for instance, often outperforms `std::regex` and has a richer feature set.

Implementation-wise, `std::regex` can be slower than some custom parsing algorithms, especially for simple patterns. Understanding the trade-off between regex convenience and potential performance issues is key.

## See Also

- C++ reference on `<regex>`: https://en.cppreference.com/w/cpp/regex
- Boost.Regex documentation: https://www.boost.org/doc/libs/release/libs/regex/
- PCRE's official site: https://www.pcre.org/

Further reading and tools to improve your regex skills:

- Regular-Expressions.info Tutorial: https://www.regular-expressions.info/tutorial.html
- Regex101 (online tester): https://regex101.com/
