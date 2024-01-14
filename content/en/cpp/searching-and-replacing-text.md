---
title:    "C++ recipe: Searching and replacing text"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Why

As a programmer, one of the most common tasks is to search and replace text in a codebase. This could be for various reasons such as correcting typos, updating variables, or refactoring code. Manual searching and replacing can be time-consuming and error-prone, which makes it important to know how to do it efficiently using code. 

## How To

There are various ways to search and replace text in C++. One method is by using the `std::string` class and its `find()` and `replace()` functions. Here's a sample code snippet that demonstrates this approach:

```C++
#include <iostream> 
#include <string> 
  
int main() 
{ 
    // Define string 
    std::string str = "Hello World!"; 
  
    // Print original string
    std::cout << "Original String: " << str << std::endl;

    // Find and replace "World" with "Universe" 
    str.replace(str.find("World"), 5, "Universe"); 

    // Print modified string 
    std::cout << "Modified String: " << str << std::endl;
  
    return 0; 
} 
```

*Output:*

```
Original String: Hello World!
Modified String: Hello Universe!
```

In this code, we use the `find()` function to locate the position of the first occurrence of "World" in the string, and then use the `replace()` function to replace it with "Universe". This method is useful for simple text replacements, but may not be ideal for more complex searching patterns.

Another approach is using regular expressions with the `std::regex` class. Regular expressions are powerful tools for searching and manipulating strings. Here's an example of how to use them for searching and replacing:

```C++
#include <iostream>
#include <regex>
#include <string>

int main()
{
    // Define string
    std::string str = "Today is 01/01/2022";

    // Define regex pattern
    std::regex date_pattern("\\d\\d/\\d\\d/\\d\\d\\d\\d");

    // Replace all dates with "TBD"
    std::cout << std::regex_replace(str, date_pattern, "TBD") << std::endl;

    return 0;
}
```

*Output:*

```
Today is TBD
```

In this code, we define a regular expression pattern that matches dates in the format "mm/dd/yyyy". We then use the `regex_replace()` function to replace all occurrences of this pattern with "TBD". Regular expressions are incredibly powerful and have a wide range of use cases, making them a valuable tool for searching and replacing text in your code.

## Deep Dive

While these are just two examples of how to search and replace text in C++, there are many other methods and tools available, such as using libraries like Boost or using third-party text editors with built-in find and replace features. It's important to understand the specific requirements of your task and choose the best method for efficient and accurate results. Additionally, it's always good to familiarize yourself with the specific functions or tools you are using, as this can help optimize your code and avoid any unexpected errors.

## See Also

- [std::string documentation](https://en.cppreference.com/w/cpp/string/basic_string)
- [std::regex documentation](https://en.cppreference.com/w/cpp/regex)
- [Boost library](https://www.boost.org/)
- [Third-party text editors](https://stackoverflow.com/questions/40584978/regex-search-and-replace-in-c-files-using-ultraedit)