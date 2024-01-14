---
title:                "C++ recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why
In many programming tasks, it is necessary to convert a date into a string in order to properly display it or use it in a specific format. This allows for easier readability and manipulation of dates within a program.

## How To
To convert a date into a string in C++, we can use the `strftime` function from the `<ctime>` library. This function takes in a format string and the date to be converted. Let's take a look at an example:

```C++
#include <iostream>
#include <ctime>

int main(){
    // Current date
    std::time_t now = std::time(0);

    // Format string
    char* format = "%m/%d/%Y";

    // Convert date to string
    char date_str[9];
    std::strftime(date_str, 9, format, std::localtime(&now));

    // Output
    std::cout << "Today's date is: " << date_str << std::endl;
    return 0;
}
```

Running this code will output the current date in the format of `mm/dd/yyyy`. We can change the format string to tailor the string output to our desired format. Some common format specifiers include `%m` for the month, `%d` for the day, and `%Y` for the year.

## Deep Dive
The `strftime` function uses the system's locale to determine how to format the date string. This means that the string output may vary depending on the system's language and region settings. It is also important to note that the `char` array used to store the string must be large enough to hold the formatted date. Failing to allocate enough space can result in unexpected behavior.

Another important detail to consider is how the date is stored before being converted to a string. In our example, we used `std::time_t` which represents the number of seconds since January 1, 1970. Other formats may require different conversions before being used with the `strftime` function.

## See Also
- [strftime reference on cplusplus.com](https://www.cplusplus.com/reference/ctime/strftime/)
- [Date and Time in C++ - Tutorial on tutorialspoint](https://www.tutorialspoint.com/cplusplus/cpp_date_time.htm)
- [Working with Dates and Times in C++ - Article on CodeProject](https://www.codeproject.com/Articles/31907/Working-with-Dates-and-Time-C)