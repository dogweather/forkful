---
title:    "C++ recipe: Using regular expressions"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Why Regular Expressions are Essential in C++ Programming

Regular expressions are a powerful tool in C++ programming that allow developers to search and manipulate text in a flexible and efficient manner. They provide a concise and powerful syntax for pattern matching, making it easier to handle complex text processing tasks. Whether you're working with data parsing, input validation, or string manipulation, regular expressions are a must-have in your programming arsenal.

## How To Use Regular Expressions in C++

To use regular expressions in your C++ code, you first need to include the \<regex> header file. This provides the necessary functions and types to work with regular expressions. Let's take a look at an example:

```C++
// Code block 1
#include <iostream>
#include <regex>

using namespace std;

int main() {
  // Create a regular expression object that matches a 10-digit phone number
  regex phone_regex("\\d{3}-\\d{3}-\\d{4}");

  // A sample string to check against
  string phone_number = "123-456-7890";

  // Use regex_match() to check if the string matches the given pattern
  if (regex_match(phone_number, phone_regex)) {
    cout << "Valid phone number" << endl;
  }
  else {
    cout << "Invalid phone number" << endl;
  }
  return 0;
}
```
Output:
```
Valid phone number
```

In the code above, we create a regular expression object that matches a phone number of the format "###-###-####". Then, using the regex_match() function, we check if a given string matches the pattern. Regular expressions in C++ support a wide range of syntax, from basic characters and character classes to more advanced features like lookaheads and backreferences.

## Deep Dive into Regular Expressions

To better understand regular expressions, let's look at some essential components and syntax:

- **Character classes:** These are sets of characters that can match any single character in a string. For example, [abc] will match any character that is either 'a', 'b', or 'c'.

- **Quantifiers:** These are used to specify the occurrence of a pattern. For instance, "a{2,4}" will match "aa", "aaa", or "aaaa".

- **Anchors:** These are used to specify the location of a pattern within a string. The ^ and $ symbols represent the start and end of a string, respectively.

- **Capturing groups:** These allow us to extract specific portions of a matched string. Groups are represented by parentheses ( ) and numbered from left to right.

You can find a comprehensive list of regular expression syntax in the [C++ regex reference](https://en.cppreference.com/w/cpp/header/regex).

## See Also

Here are some useful links to dive deeper into regular expressions in C++:

- [Mastering Regular Expressions by Jeffrey E.F. Friedl](https://www.oreilly.com/library/view/regular-expressions-cookbook/9781449327453/)
- [C++ Regular Expressions Cheat Sheet](https://www.cheatography.com/davechild/cheat-sheets/regular-expressions/)
- [Regular expressions in C++ tutorial by CppCon](https://www.youtube.com/watch?v=7VD51F5pEQU)