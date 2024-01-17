---
title:                "Capitalizing a string"
html_title:           "C++ recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Capitalizing a string means converting all the letters in the string to uppercase. Programmers often do this because it can make the string easier to read and compare with other strings. It can also be necessary for certain functions that require the string to be in uppercase.

## How to:
```C++
// Include the necessary header file
#include <iostream>

using namespace std;

int main() {
    // Declare a string variable
    string str = "hello world";
    
    // Convert the string to uppercase
    for (int i = 0; i < str.length(); i++) {
        str[i] = toupper(str[i]);
    }
    
    // Output the capitalized string
    cout << str << endl;
    
    return 0;
}
```
Output: HELLO WORLD

## Deep Dive:
Historically, capitalizing strings was necessary because computer systems used to only recognize uppercase letters. However, as technology has evolved, most systems can now recognize lowercase letters as well. Alternatives to converting the entire string to uppercase include using functions that only capitalize the first letter of each word (title case) or only the first letter of the string (sentence case). The implementation details of capitalizing a string involve iterating through each character in the string and using the built-in function `toupper()` to convert it to uppercase. 

## See Also:
- [tolower() function reference](https://www.cplusplus.com/reference/cctype/tolower/)
- [Title case and sentence case in C++](https://www.geeksforgeeks.org/title-case-sentence-case-string/)
- [ASCII vs Unicode](https://www.guru99.com/difference-ascii-vs-unicode.html)