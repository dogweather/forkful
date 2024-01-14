---
title:    "C++ recipe: Using regular expressions"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why

Regular expressions, also known as regex, are an essential tool for developers when it comes to string manipulation. They provide a concise and powerful way to find, replace, or extract specific patterns from a given string. Using regular expressions can greatly simplify tasks such as data validation, parsing, and text mining.

## How To

To use regular expressions in your C++ code, you will first need to include the header file `regex`. This header provides the necessary classes and functions for working with regular expressions.

Next, we will create a regular expression using the `std::regex` class and pass in the pattern that we want to match. For example, let's say we want to find all email addresses in a string, our regular expression pattern could look like this: `[a-zA-Z0-9_.]+@[a-zA-Z0-9]+.[a-z]+`.

Once we have our regular expression, we can use `std::regex_match` to check if a given string matches the pattern. This function returns a boolean value, which we can use to determine if the string is a valid email address or not. Here's an example of how we can do this in code:

```C++
#include <iostream>
#include <string>
#include <regex>

int main() {
    std::string email = "john@gmail.com";
    std::regex emailRegex("[a-zA-Z0-9_.]+@[a-zA-Z0-9]+.[a-z]+");
    if (std::regex_match(email, emailRegex)) {
        std::cout << "Valid email address" << std::endl;
    } else {
        std::cout << "Invalid email address" << std::endl;
    }
    return 0;
}
```

The above code will output "Valid email address", as the given email string matches our regular expression pattern. Keep in mind that regular expressions can be much more complex, and there are many different ways to write them, so it may take some practice to get the hang of it.

## Deep Dive

Regular expressions have their own syntax and set of rules that can take some time to learn. But once you understand how they work, they can be a powerful tool in your programming arsenal. Here are a few tips to keep in mind when using regular expressions:

- Use escape characters when necessary: Some characters have special meanings in regular expressions, such as `.` which matches any single character. If you want to match the actual `.` character, you will need to escape it with a backslash: `\.`.
- Use character classes: Character classes are a convenient way to specify a group of characters to match. For example, `\d` will match any digit character, and `\w` will match any word character (letters, numbers, and underscore).
- Use quantifiers: Quantifiers allow you to specify the number of times a character or group of characters can be repeated. For example, `a+` will match one or more `a` characters, `a*` will match zero or more `a` characters, and `a{3}` will match exactly three `a` characters.

These are just a few basic concepts, but there is much more to explore when it comes to regular expressions. With practice and experimentation, you will become more comfortable using them in your code.

## See Also

- [C++ regex documentation](https://en.cppreference.com/w/cpp/regex)
- [Regular Expression Tutorial](https://www.regular-expressions.info/tutorial.html)
- [Regex101](https://regex101.com/) - An online tool for testing and building regular expressions.