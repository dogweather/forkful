---
title:                "Finding the length of a string"
html_title:           "C++ recipe: Finding the length of a string"
simple_title:         "Finding the length of a string"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why
Finding the length of a string is a common task in programming, as it allows us to manipulate and process strings effectively. By knowing the length of a string, we can perform various operations such as searching, sorting, and comparing strings.

## How To
To find the length of a string in C++, we can use the `std::string::length()` function. This function returns the number of characters in a string, including white spaces and special characters. Let's take a look at an example:

```C++
#include <iostream> 
using namespace std; 

int main() 
{ 
  string str = "Hello World"; 
  // using the length() function to find the length of the string 
  int length = str.length(); 
  // printing the length 
  cout << "The length of the string is: " << length; 
  return 0; 
} 
```

This code will output: `The length of the string is: 11`. In this example, we have declared a string `str` and used the `length()` function to find its length. Then, we have printed the result using the `cout` statement.

Apart from the `length()` function, we can also find the length of a string using the `size()` function. Both these functions are members of the `std::string` class and have the same functionality.

## Deep Dive
In C++, strings are an array of characters, and thus, we can find the length of a string using array-related methods as well. We can use a loop to traverse the string and count the number of characters. Alternatively, we can use the `strlen()` function from the C standard library.

One important thing to note is that the size of a string in C++ is not limited by a data type. It can dynamically grow or shrink according to the program's needs. Therefore, the `length()` and `size()` functions are essential to keep track of the string's current size.

## See Also
- [C++ Documentation on `std::string`](https://en.cppreference.com/w/cpp/string/basic_string)
- [C++ string functions](https://www.javatpoint.com/cpp-string-functions)