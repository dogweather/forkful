---
title:                "Using regular expressions"
html_title:           "C++ recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why?
Regular expressions are a tool used by programmers to search for patterns in text. They allow for efficient, flexible text processing and can be used for tasks such as finding and replacing words, extracting data, and validating input. Programmers use regular expressions to save time and write more concise code.

## How to:
To start using regular expressions in your C++ code, include the `<regex>` header and use the `std::regex` class. Here is an example of matching a three digit number in a string:
```C++
#include <iostream>
#include <regex>

int main() {
    std::string str = "Hello 123 world!";
    std::regex reg("\\d{3}");
    std::smatch match;
    
    if (std::regex_search(str, match, reg)) {
        std::cout << match[0] << std::endl;
    }
    
    return 0;
}
```
Output:
```
123
```
In this example, we use the `regex_search()` function to search for a match in the string. The `match` object stores the results of the search, and we can access the first match using `match[0]`. Regular expressions can also be used for more complex tasks such as replacing text and validating input. 

## Deep Dive:
Regular expressions have been around since the 1950s when they were first created by mathematician Stephen Kleene. They have since evolved into a standard tool used in many programming languages. Some alternatives to regular expressions include string manipulation functions and parsers, but regular expressions offer a more concise and versatile solution. 

Under the hood, regular expressions are converted into a state machine and used to match patterns in the text. The syntax for regular expressions can vary between languages, so it's important to refer to the specific documentation for your chosen language.

## See Also:
- [C++ Regular Expressions documentation](https://en.cppreference.com/w/cpp/regex)
- [Regex tutorial for beginners](https://www.regular-expressions.info/tutorial.html)
- [Regular Expressions cheat sheet](https://cheatography.com/davechild/cheat-sheets/regular-expressions/)