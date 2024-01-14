---
title:                "C++ recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why
Searching and replacing text is a common task in programming, especially when working with large amounts of data. It allows for quick and efficient editing of text, making it a valuable tool for developers.

## How To
To perform a simple search and replace task in C++, we can use the `replace()` function from the `<algorithm>` library. This function takes in three parameters: a beginning iterator, an ending iterator, and a value to replace.

````C++
#include <iostream>
#include <algorithm>

int main() {
    std::string text = "Hello world!";
    std::replace(text.begin(), text.end(), 'o', 'e');
    std::cout << text << std::endl;
    return 0;
}
````

Output: `Helle werld!`

We can also use regular expressions with the `regex_replace()` function from the `<regex>` library to perform more complex search and replace tasks. Regular expressions allow for pattern matching in strings, making it a powerful tool for text manipulation.

````C++
#include <iostream>
#include <regex>

int main() {
    std::string text = "Hello world!";
    std::regex reg("o");
    std::string new_text = std::regex_replace(text, reg, "e");
    std::cout << new_text << std::endl;
    return 0;
}
````

Output: `Helle werld!`

## Deep Dive
When using regular expressions, there are various modifiers and special characters that can be used to refine the search criteria. Some commonly used ones include `*` for zero or more occurrences of a character, `+` for one or more occurrences, `.` for any character, `[]` for a list of characters, and `()` for grouping. Regular expressions also have modifiers such as `*?` for non-greedy matching and `?` for optional matching.

It is important to be aware of the potential performance impact of using regular expressions for large amounts of data. In some cases, it may be more efficient to use simple string methods for search and replace tasks.

## See Also
- [C++ replace() function documentation](https://www.cplusplus.com/reference/algorithm/replace/)
- [C++ regex_replace() function documentation](https://www.cplusplus.com/reference/regex/regex_replace/)
- [Regular expressions in C++](https://www.geeksforgeeks.org/regular-expressions-in-c-c/)
- [Performance impact of using regular expressions](https://softwareengineering.stackexchange.com/questions/223514/performance-impact-of-using-regular-expressions)