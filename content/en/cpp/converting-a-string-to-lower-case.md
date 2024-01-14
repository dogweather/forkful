---
title:                "C++ recipe: Converting a string to lower case"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Why

When working with strings in C++, it's important to keep in mind the input and output formats. Converting a string to lower case can be useful in a variety of situations, such as when comparing user input to a specific value or when performing case-insensitive searches.

## How To

To convert a string to lower case in C++, we can use the `tolower()` function from the `<cctype>` header. Here's an example:

```C++
#include <iostream>
#include <string>
#include <cctype> // header for tolower()

using namespace std;

int main() {
  string myString = "HELLO WORLD";
  for (char &c : myString) { // loop through each character in the string
    c = tolower(c); // convert to lower case
  }
  cout << myString; // outputs "hello world"
  return 0;
}
```

In this example, we use a `for` loop to iterate through each character in the string and convert it to lower case using the `tolower()` function. Then, we simply print out the updated string to see the result.

## Deep Dive

Behind the scenes, the `tolower()` function uses the ASCII values of characters to convert them to lower case. In the ASCII character set, upper case letters have a higher value than lower case letters, making it easy to convert them using simple arithmetic.

It's important to note that the `tolower()` function only works with single characters, so we need to use a loop to convert an entire string. Additionally, this function only works with ASCII characters, so it may not be suitable for languages with special characters or non-ASCII characters.

## See Also

- [ASCII Table](https://www.asciitable.com/) - A helpful reference for ASCII values of characters.
- [C++ String Functions](https://www.geeksforgeeks.org/string-class-in-cpp/) - A comprehensive guide to working with strings in C++.