---
title:                "Searching and replacing text"
html_title:           "Arduino recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# Searching & Replacing Text in C++

## What & Why?

Searching and replacing text is the process of locating a specific string and swapping it with another. Programmers do this to manipulate data, correct errors, customize content, or simplify user interactions.

## How to:

```C++
#include <string>
#include <iostream>

int main() {
    std::string str = "Hello, Dude!";
    std::cout << "Original String: " << str << std::endl;
    
    size_t pos = str.find("Dude");
    if (pos != std::string::npos)
        str.replace(pos, 4, "World");
    
    std::cout << "Modified String: " << str << std::endl;
    
    return 0;
}
```

This C++ program will search the string "Hello, Dude!" for the substring "Dude", and replace it with "World". The output will be:

```C++
Original String: Hello, Dude!
Modified String: Hello, World!
```

## Deep Dive

Historically, text searching and replacing have been essential for text processing. Originating in early command-line interfaces, they're now standard features in most programming languages. 

One alternative to manual search-and-replace is using regular expressions, which allows for pattern-based searching. However, these can be complex and are generally less efficient. 

Under the hood, `std::string::find` scans the string linearly until it finds a match. If a match is found, `std::string::replace` alters the necessary characters. These operations are both O(n), where n is the length of the string.

## See Also

Discover more with these links: 

- `std::string::find`: http://www.cplusplus.com/reference/string/string/find/
- `std::string::replace`: http://www.cplusplus.com/reference/string/string/replace/
- Text Processing in C++: https://en.cppreference.com/w/cpp/string
- Regular Expressions in C++: https://www.cplusplus.com/reference/regex/