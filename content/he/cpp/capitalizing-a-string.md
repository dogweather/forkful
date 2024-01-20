---
title:                "הפיכת מחרוזת לאותיות רישיות"
html_title:           "Bash: הפיכת מחרוזת לאותיות רישיות"
simple_title:         "הפיכת מחרוזת לאותיות רישיות"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (מה ולמה?)
Capitalizing a string means converting the first character of each word to uppercase. Programmers do this to format text for consistency, readability, or to meet specific data standards.

## How to: (איך לעשות:)
```C++
#include <iostream>
#include <cctype>
#include <string>

std::string capitalize(const std::string& input) {
    std::string result;
    bool newWord = true;
    
    for (char ch : input) {
        if (newWord && std::isalpha(ch)) {
            result += std::toupper(ch);
            newWord = false;
        } else {
            result += ch;
        }
        if (std::isspace(ch)) {
            newWord = true;
        }
    }
    
    return result;
}

int main() {
    std::string text = "hello, world! here's some text.";
    std::string capitalizedText = capitalize(text);
    std::cout << capitalizedText << std::endl;
    // Output: Hello, World! Here's Some Text.
}
```

## Deep Dive (עומק השקעה)
Historically, capitalizing words in programming can be traced to early computing when systems were case-insensitive. Different languages and libraries have their own methods to capitalize strings. For instance, in Python, there's a title() method. 

In C++, there's no built-in method for string capitalization but we usually use a mix of character checking and manipulation functions like `isalpha()`, `toupper()`, and `isspace()`.

One alternative way to capitalize is to use locale-specific functions if dealing with non-English texts. Implementing your own function, as shown above, gives you control over the capitalization process, especially for words with apostrophes or special characters.

## See Also (ראה גם)
- C++ reference for `<cctype>` functions: https://en.cppreference.com/w/cpp/header/cctype
- C++ reference for string class: https://en.cppreference.com/w/cpp/string/basic_string
- Discussion on string manipulation in C++: https://stackoverflow.com/questions/tagged/c%2b%2b+string+manipulation