---
title:                "Deleting characters matching a pattern"
html_title:           "C++ recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why

Sometimes, when working with strings or text data, you may want to remove specific characters that match a certain pattern. This could be to clean up the data or to prepare it for further processing. In such cases, knowing how to delete characters matching a pattern can be very useful.

## How To

To delete characters matching a pattern in C++, you can use the `std::regex_replace` function from the `<regex>` library. This function takes in three parameters: the string to be modified, the regular expression pattern, and the replacement string. Here's an example of using this function to remove all digits from a string:

```C++
#include <iostream>
#include <regex>

int main()
{
  std::string text = "I have 3 apples and 4 oranges.";
  std::regex pattern("[0-9]"); // regular expression pattern to match digits
  std::string replacement = ""; // replacement string, in this case an empty string
  std::string result = std::regex_replace(text, pattern, replacement); // call to regex_replace
  std::cout << result << std::endl; // output: "I have apples and oranges."
  return 0;
}
```

In this example, we first define the string `text` that contains both numbers and letters. Then, we define the regular expression `pattern` to match any digit (represented by the character class `[0-9]`). Finally, we specify an empty string as the replacement string, as we want to remove the digits from the text. The resulting string is stored in the `result` variable and is printed to the console.

## Deep Dive

Regular expressions, often referred to as regex, are patterns used to match and manipulate text. They are widely used in programming languages to perform string operations such as search, replace, and delete. In C++, the `<regex>` library provides various functions for working with regular expressions, including the `std::regex_replace` function used in our example.

The regular expression pattern can include not just single characters, but also special characters and operators to define complex patterns. In our example, the `[0-9]` character class matches any single digit from 0 to 9. Other commonly used operators in regular expressions include `*` to match zero or more occurrences, `+` to match one or more occurrences, and `?` to match zero or one occurrence.

In addition to the `std::regex_replace` function, the `<regex>` library also provides other useful functions such as `std::regex_match` for validating a string against a regular expression and `std::smatch` for storing matched substrings. Understanding regular expressions can greatly enhance your string manipulation skills in C++.

## See Also

- [C++ Regular Expressions](https://en.cppreference.com/w/cpp/regex)
- [Regular Expressions 101](https://regex101.com/) (online regex tester and debugger)
- [C++ String Class](https://en.cppreference.com/w/cpp/string/basic_string) (for more string operations)