---
title:                "Extracting substrings"
html_title:           "C++ recipe: Extracting substrings"
simple_title:         "Extracting substrings"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/extracting-substrings.md"
---

{{< edit_this_page >}}

##Why
So, you're writing a C++ program and you need to extract a substring from a larger string. Maybe you need to separate a date into its individual components or get a specific word from a sentence. Whatever your reason may be, extracting substrings is a common and useful task in programming.

##How To
To start off, let's define what a substring is. A substring is a sequence of characters within a larger string. In C++, substrings are represented as a new string object that contains the specified characters from the original string.

To extract a substring in C++, we use the `substr()` function from the `<string>` library. The function takes in two parameters: the starting index and the length of the substring. For example:

```C++
#include <iostream>
#include <string>

using namespace std;

int main() {
    string s = "Hello World";
    string sub = s.substr(6, 5); //starting index = 6, length = 5
    cout << sub << endl; //Outputs "World"
}
```

You can also use the `substr()` function with strings stored in variables:

```C++
string s1 = "Welcome to";
string s2 = "C++ programming";
string sub = s1.substr(2, 6) + s2.substr(0, 4); //Outputs "lcomC++"
```

It's important to note that the starting index of a string in C++ is 0, not 1. So, the first character of a string has an index of 0.

##Deep Dive
Here are some other things to keep in mind when extracting substrings in C++:

- The `substr()` function can also take in a single parameter, the starting index, which will return a substring starting from the specified index until the end of the string.
- Negative values for the length parameter will not work, and an empty string will be returned.
- If the starting index is greater than or equal to the length of the string, an empty string will be returned.

And that's all there is to it! Now you can confidently extract substrings in your C++ programs.

##See Also
To learn more about the `substr()` function and other string operations in C++, check out these resources:

- [C++ string documentation](https://www.cplusplus.com/reference/string/string/)
- [Tutorialspoint - C++ String Operations](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)
- [GeeksforGeeks - C++ Strings](https://www.geeksforgeeks.org/cpp-strings/)

Happy coding!