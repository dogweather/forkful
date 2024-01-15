---
title:                "सामान्य अभिव्यक्ति का उपयोग करें"
html_title:           "C++: सामान्य अभिव्यक्ति का उपयोग करें"
simple_title:         "सामान्य अभिव्यक्ति का उपयोग करें"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/using-regular-expressions.md"
---

{{< edit_this_page >}}

##Regular Expressions ka upyog kyon karna chahiye?

Agar aap C++ programmer hai, to aapko regular expressions ka upyog karke apne code ko powerful aur efficient bana sakte hai. Ye bahut hi versatile aur flexible hai, jiske madhyam se aap patterns ko search, match aur replace kar sakte hai apne string mein, sath hi sath input validation aur data processing bhi kar sakte hai.

##Regular Expressions kaise upyog karein?

```C++
// Simple Pattern Matching
#include <iostream>
#include <regex>

int main()
{
    // Input string
    std::string str = "Hello World!";

    // Pattern to search
    std::regex pattern("World");

    // Matching string using regex_search function
    if (std::regex_search(str, pattern))
    {
        // Output
        std::cout << "Pattern found!" << std::endl;
    }
    else
    {
        // Output
        std::cout << "Pattern not found!" << std::endl;
    }

    return 0;
}

```

```C++
// Using Regular Expressions for Input Validation
#include <iostream>
#include <regex>

int main()
{
    // Input validation for age
    int age;
    std::cout << "Enter your age: ";
    std::cin >> age;

    // Regex pattern for valid age
    std::regex pattern("[0-9]{1,3}");

    // Matching input with pattern
    if (std::regex_match(std::to_string(age), pattern))
    {
        // Output
        std::cout << "Valid age entered!" << std::endl;
    }
    else
    {
        // Output
        std::cout << "Invalid age entered!" << std::endl;
    }

    return 0;
}
```

##Regular Expressions ka adhyan karna

Regular Expressions ko adhyan karne se pehle, aapko inke basic syntax aur functions ka pata hona chahiye. Inke alawa, aapko inka upyog karte hue apne code ko efficient banane ke techniques bhi sikhna hoga, jaise ki capturing groups, backreferences aur lookaheads/lookbehinds. Iske liye aap online tutorials aur documentation refer kar sakte hai.

##Dekhiye Bhi

- [C++ Regex Tutorial in Hindi](https://www.youtube.com/watch?v=oZgx7f_LIhk)
- [C++ Regex Documentation](https://en.cppreference.com/w/cpp/regex)