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

## What & Why?

Deleting characters matching a pattern in programming refers to removing specific characters from a string that follow a certain pattern. This is often useful for filtering out unwanted or irrelevant information from a large dataset. Programmers use this technique to streamline their code and make it more efficient.

## How to:

To delete characters matching a pattern in C++, you can make use of the `std::regex_replace` function from the `<regex>` library. This function takes in three parameters - the string to be modified, the regular expression pattern, and the replacement string.

```
#include <iostream>
#include <regex>

int main() {
  std::string text = "Hello123World";
  std::cout << "Before: " << text << std::endl;
  
  // using regex_replace to remove numbers from string
  text = std::regex_replace(text, std::regex("[0-9]+"), "");
  std::cout << "After: " << text << std::endl;
  return 0;
}
```

Output:
```
Before: Hello123World
After: HelloWorld
```

As you can see, the regular expression `[0-9]+` matches any sequence of numbers in the string and replaces them with an empty string, effectively deleting them.

## Deep Dive:

Deleting characters matching a pattern has been a widely used technique in programming for a long time. However, with the introduction of regular expressions in C++, this task has become much easier and more customizable.

One alternative to using regular expressions for deleting characters is the use of loop functions to iterate through each character in the string and remove any that match the desired pattern. While this is a viable option, it often leads to longer and more complex code.

Regular expressions provide a powerful and concise way of finding and replacing patterns in strings. They offer a wide range of metacharacters and special sequences that can be used for more advanced pattern matching.

## See Also:

For more information on regular expressions and their usage in C++, check out the [C++ documentation for std::regex](https://en.cppreference.com/w/cpp/regex) and the [regular expressions tutorial](https://www.regular-expressions.info/tutorial.html).

Other useful resources include [regexr](https://regexr.com/), a live regular expression testing site, and [regex101](https://regex101.com/), a site for building and debugging regular expressions.