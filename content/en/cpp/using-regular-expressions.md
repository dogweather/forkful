---
title:                "C++ recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why
Regular expressions are a powerful tool for string manipulation and validation in programming. They allow for efficient and precise pattern matching, making them an essential weapon in a developer's arsenal. Whether you're working on a small personal project or a large enterprise application, knowing how to use regular expressions can greatly enhance your coding skills.

## How To
Using regular expressions in C++ is relatively straightforward. First, we need to include the <regex> header in our code. This provides us with the necessary functions and classes to work with regular expressions.

Next, we can define a regular expression pattern using the constructor of the std::regex class. For example, if we want to find all words containing the letter "a" in a given string, we can use the pattern `std::regex("a+\\w*")`.

Once we have our regular expression pattern, we can use the `std::regex_match` function to check if a string matches the pattern. This function returns a boolean value, indicating whether the string matches the pattern or not. We can also use the `std::regex_search` function to find the first occurrence of the pattern in a string.

Here's a simple example of using regular expressions in C++ to check for valid email addresses:

```C++
#include <iostream>
#include <regex>

int main() {
    // define regular expression pattern for email addresses
    std::regex pattern("\\b[A-Z0-9._%+-]+@[A-Z0-9.-]+\\.[A-Z]{2,}\\b", std::regex_constants::icase);
    
    // ask user for input
    std::string email;
    std::cout << "Enter an email address: ";
    std::cin >> email;
    
    // check if input matches pattern
    if(std::regex_match(email, pattern)) {
        std::cout << "Valid email address!" << std::endl;
    } else {
        std::cout << "Invalid email address!" << std::endl;
    }
    
    return 0;
}
```

Sample output:

```
Enter an email address: john.doe@email.com
Valid email address!
```

```
Enter an email address: john.doeemail.com
Invalid email address!
```

For more complex pattern matching, we can use the `std::regex_replace` function to replace parts of a string that match the pattern with a new string. This is useful for tasks such as data cleaning and formatting.

## Deep Dive
Regular expressions can get quite complex, with different metacharacters and modifiers that allow for more precise matching. Some commonly used ones include:

- `.` : matches any single character
- `+` : matches one or more occurrences of the preceding character
- `*` : matches zero or more occurrences of the preceding character
- `?` : matches zero or one occurrence of the preceding character
- `\d` : matches any digit
- `\w` : matches any alphanumeric character
- `\s` : matches any whitespace character

We can also use brackets to create groups of characters and the pipe symbol `|` to specify alternatives. For example, the pattern `(foo|bar)` will match either "foo" or "bar" in a string.

Regular expressions also support a variety of modifiers, such as case sensitivity, greedy vs non-greedy matching, and lookaheads/lookbehinds. It's worth taking some time to delve into these concepts to fully utilize the power of regular expressions.

## See Also
- [C++ Regular Expressions Reference](https://www.cplusplus.com/reference/regex/)
- [Regex Tutorial for Beginners](https://www.regular-expressions.info/tutorial.html)
- [Regex Cheat Sheet](https://www.rexegg.com/regex-quickstart.html)

Regular expressions may seem daunting at first, but with practice and understanding, they can greatly improve your productivity as a programmer. So go ahead and give them a try in your next project!