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

## Why
Converting a string to lower case is a common task in programming, especially when working with user input. It allows for consistency in data manipulation and comparisons, making it easier to handle and process strings.

## How To

To convert a string to lower case in C++, you can use the ```transform``` function from the ```algorithm``` library. This function takes in three parameters: the string to be converted, the beginning iterator of the string, and the ending iterator of the string. It also takes in a lambda function that specifies the transformation to be done on each character.

```C++
#include <algorithm> 
#include <string>

std::string myString = "HeLlO wOrld!";
std::transform(myString.begin(), myString.end(), myString.begin(),
               [](unsigned char c){ return std::tolower(c); });
std::cout << myString; // Output: hello world!
```

Another way to convert a string to lower case is by using the ```tolower``` function from the ```ctype.h``` library. This function takes in a single character as a parameter and returns the corresponding lower case if it is an uppercase character. This method is useful for converting a single character or a specific character within a string.

```C++
#include <ctype.h> 

char myChar = 'A';
myChar = tolower(myChar);
std::cout << myChar; // Output: a
```

## Deep Dive
In C++, strings are treated as an array of characters. Therefore, converting a string to lower case involves iterating through each character and performing the necessary modifications. The ```transform``` function simplifies this process by handling the iteration and transformation for us.

It's worth noting that the ```transform``` function modifies the string in-place, meaning it changes the original string instead of creating a new one. If you want to preserve the original string, you can use the ```tolower``` function and create a new string with the modified character.

Furthermore, the transformation done on each character can be customized according to your needs. For example, if you only want to convert a specific range of characters to lower case, you can modify the lambda function to check for those characters and convert them accordingly.

## See Also
- [Cplusplus.com](http://www.cplusplus.com/reference/algorithm/transform/)
- [Geeksforgeeks.org](https://www.geeksforgeeks.org/converting-string-lower-case-using-stl-c/)
- [Stackoverflow.com](https://stackoverflow.com/questions/313970/how-to-convert-stdstring-to-lower-case)