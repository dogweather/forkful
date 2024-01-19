---
title:                "Using regular expressions"
html_title:           "Bash recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

# Article: Using Regular Expressions in C++ 

## What & Why?

Regular expressions (regex) are a quick way to match patterns in text. Programmers use regex to efficiently search, modify, or validate text against multiple conditions.

## How to:

Here's how to use regex with C++:

```C++
#include <regex>
#include <iostream>

int main() {
    std::string myString = "I have 2 cats but I want 3 dogs.";
    std::regex myRegex ("\\d+");

    std::sregex_iterator it(myString.begin(), myString.end(), myRegex);
    std::sregex_iterator end;

    while (it != end) {
        std::cout << it->str() << "\n";
        ++it;
    }

    return 0;
}
```

This code will output:

```C++
2
3
```

The code uses the `\\d+` regular expression to find all occurrences of one or more digits in the input string.

## Deep Dive

- **Historical context**: Regular expressions originated in the 1950s, grew in popularity in Unix-based systems, and today, they are found in virtually all programming languages.

- **Alternatives**: For very complex text manipulation, a full-fledged parser might be more appropriate. Lighter tasks can be done with basic string functions.

- **Implementation details**: In C++, regex functionality is provided by the `<regex>` library. Keep in mind regex can be slow. Use wisely to not impact performance.

## See Also:

- Learn more about regex in C++ at [cplusplus.com](http://www.cplusplus.com/reference/regex/)
- Test and learn regex interactively [Regex101.com](https://regex101.com/)
- Understand C++ performance implications at [cppreference.com](https://en.cppreference.com/w/cpp/regex)