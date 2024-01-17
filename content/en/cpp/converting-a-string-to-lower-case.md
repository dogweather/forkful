---
title:                "Converting a string to lower case"
html_title:           "C++ recipe: Converting a string to lower case"
simple_title:         "Converting a string to lower case"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why?

Converting a string to lower case is the process of changing all the letters in a string to their lower case equivalents. This is often done by programmers to standardize text input and make it easier to compare strings for various operations.

## How to:

```C++
//Example coding using standard library functions

#include <iostream>
#include <algorithm> //for using std::transform function
#include <string> //for using string data type

int main() {
    
    //declare and initialize a string variable
    std::string myString = "HeLlO wOrLd!";
    
    //print original string
    std::cout << "Original string: " << myString << "\n";
    
    //use std::transform function to convert string to lower case
    std::transform(myString.begin(), myString.end(), myString.begin(), ::tolower); 
    
    //print converted string
    std::cout << "Converted string: " << myString << "\n";
    
    return 0;
}

```

Output:
```
Original string: HeLlO wOrLd!
Converted string: hello world!
```

## Deep Dive:

Converting strings to lower case has been a common practice in programming since the early days of computing. In older programming languages, such as C, developers had to use complex functions or algorithms to convert strings to lower case. However, with the evolution of newer languages like C++, this conversion can be easily achieved using built-in functions and methods.

There are also alternative methods for converting strings to lower case, such as using the ASCII table to manually change each letter to its lower case equivalent. However, this can be time-consuming and error-prone, making it less efficient compared to using standard library functions.

In terms of implementation, the std::transform function in C++ is commonly used to convert strings to lower case. This function takes in three parameters: the beginning and end iterators of the string, and a function to specify the conversion. The `::tolower` parameter is used to specify that all characters should be converted to their lower case equivalents.

## See Also:

- [C++ Standard Library: transform()](https://www.cplusplus.com/reference/algorithm/transform/)
- [ASCII Table](https://www.ascii-code.com/)