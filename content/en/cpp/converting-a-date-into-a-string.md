---
title:                "Converting a date into a string"
html_title:           "C++ recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

Converting a date into a string is a common task in programming, especially when working with user interfaces or data visualization. By converting a date into a string, we can easily display it in a readable format for users to understand.

## How To

To convert a date into a string in C++, we can use the `std::stringstream` class from the `<sstream>` library. This class allows us to easily manipulate strings, including converting other data types into strings.

```
#include <iostream>
#include <sstream>

int main() {
    // Create a date object
    std::tm date = {0, 0, 0, 12, 10, 119}; // Dec 10, 2019

    // Create a string stream
    std::stringstream ss;

    // Convert the date to string format
    ss << std::put_time(&date, "%b %d, %Y"); // Output: Dec 10, 2019

    // Output the resulting string
    std::cout << ss.str();

    return 0;
}
```

In the above code, we first create a `std::tm` object representing the date we want to convert. Then, we create a `std::stringstream` object and use the `<<` operator to insert the date into the stream. Finally, we can use the `str()` method to retrieve the resulting string and output it.

## Deep Dive

The `std::stringstream` class is just one way to convert a date into a string in C++. Another method is to use the `std::strftime()` function from the `<ctime>` library. This function allows us to format a `std::tm` object according to a specified format string.

```
#include <iostream>
#include <ctime>

int main() {
    // Create a date object
    std::tm date = {0, 0, 0, 12, 10, 119}; // Dec 10, 2019

    // Convert the date to string format
    char str[11];
    std::strftime(str, sizeof(str), "%b %d, %Y", &date); // Output: Dec 10, 2019

    // Output the resulting string
    std::cout << str;

    return 0;
}
```

In this code, we use the `std::strftime()` function to format the date according to the given format string. The resulting string is then stored in the `str` variable and can be outputted.

## See Also

- [C++ Dates and Time tutorial](https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm)
- [std::stringstream documentation](https://en.cppreference.com/w/cpp/io/basic_stringstream)
- [std::strftime documentation](https://en.cppreference.com/w/cpp/chrono/c/strftime)