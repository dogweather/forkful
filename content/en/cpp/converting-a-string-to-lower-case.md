---
title:                "Converting a string to lower case"
html_title:           "Clojure recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?
Converting a string to lower case in C++ refers to the process of changing each uppercase character in a string to its lowercase equivalent. This is done for several reasons, such as for ensuring data uniformity, performing case-insensitive comparisons, and preprocessing text data in areas like natural language processing and data mining.

## How To:
To convert a string to lower case in C++, you can use the `tolower(int c)` function from the <cctype> library in a for loop:

```C++
#include <cctype>   // For tolower()
#include <iostream> // For cout
#include <string>   // For string

int main() {
    std::string str = "Hello, World!";
    for (char &c : str) {
        c = std::tolower(c);
    }
    std::cout << str << std::endl; // "hello, world!" is printed
    return 0;
}
```
This program changes every character of the string into the lower case and prints it. 

## Deep Dive
The `tolower(int c)` function actually has a long history. It comes from the C standard library and was incorporated into C++ in its earliest versions. This function is known for its simplicity yet profound utility.

Still, `tolower(int c)` isn't the only way to convert a string to lower case in C++. An alternative is to use `std::transform` from the <algorithm> library with `::tolower`:

```C++
#include <algorithm> // For transform
#include <cctype>    // For tolower()
#include <iostream>  // For cout
#include <string>    // For string

int main() {
    std::string str = "Hello, World!";
    std::transform(str.begin(), str.end(), str.begin(), 
        [](unsigned char c) -> unsigned char { return std::tolower(c); });
    std::cout << str << std::endl; // "hello, world!" is printed
    return 0;
}
```
In this example, `std::transform` applies `::tolower` to every character in the string in a more declarative, arguably more "C++"-ish style.

## See Also
To get a broader perspective, here are some useful links:

1. ASCII table, which explains why `tolower()` works: http://www.asciitable.com/
2. Documentation for `tolower()`: https://en.cppreference.com/w/cpp/string/byte/tolower
3. Documentation for `std::transform`: https://en.cppreference.com/w/cpp/algorithm/transform
4. Case manipulation alternatives in C++: https://stackoverflow.com/questions/313970/how-to-convert-stdstring-to-lower-case