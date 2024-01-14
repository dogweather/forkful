---
title:    "C++ recipe: Converting a string to lower case"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Why
Converting a string to lower case is a common task in many programs, especially when working with user inputs. By converting a string to lower case, we can ensure that our program is not case sensitive and can handle user inputs in a consistent manner.

## How To
To convert a string to lower case in C++, we can use the `std::transform` function along with the `std::tolower` function. Here is an example code snippet:

```C++
#include <iostream>
#include <algorithm> // for std::transform
#include <string> // for std::string
#include <cctype> // for std::tolower

int main() {
    // Initialize a string with mixed case alphabets
    std::string str = "HeLlo WoRlD";
    
    // Use std::transform to convert each character to lower case
    std::transform(str.begin(), str.end(), str.begin(), [](unsigned char c) {
        return std::tolower(c);
    });
    
    // Output the converted string
    std::cout << str << std::endl;
    
    // Output: hello world
    
    return 0;
}
```

## Deep Dive
Behind the scenes, the `std::transform` function uses the `std::tolower` function to convert each character to lower case. `std::tolower` is a standard library function that takes in a single character and returns its lower case equivalent. The `std::transform` function makes use of this by passing in a lambda function as a parameter, which ensures that each character in the string is converted to lower case.

There are also other ways to convert a string to lower case in C++, such as using a `for` loop and checking each character's ASCII value. However, using `std::transform` is a more efficient and concise method.

## See Also
- [std::transform documentation](https://en.cppreference.com/w/cpp/algorithm/transform)
- [std::tolower documentation](https://en.cppreference.com/w/cpp/string/byte/tolower)