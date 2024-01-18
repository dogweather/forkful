---
title:                "Parsing a date from a string"
html_title:           "C++ recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing a date from a string is the process of extracting date-related information (such as day, month, and year) from a given string. This task is commonly performed by programmers when dealing with user inputs or data from external sources, so that the information can be converted into a more manageable format for further processing.

## How to:

To parse a date from a string in C++, you can use the `strptime` function from the `ctime` library. This function takes in two parameters - the string containing the date, and a formatting string that specifies the date format.

Example code:

```C++
#include <ctime>
#include <iostream>

int main() {
  char date_str[] = "2021-07-01"; // string containing the date
  struct std::tm tm; // define a tm structure to store the parsed date
  std::string format = "%Y-%m-%d"; // the formatting string specifying the date format
  strptime(date_str, format.c_str(), &tm); // parse the date using strptime
  std::cout << "The day is: " << tm.tm_mday << std::endl;
  std::cout << "The month is: " << tm.tm_mon+1 << std::endl;
  std::cout << "The year is: " << tm.tm_year+1900 << std::endl;
  return 0;
}
```

Sample output:

```
The day is: 1
The month is: 7
The year is: 2021
```

## Deep Dive:

Parsing dates from strings has been a common task for programmers since the early days of computing. With the increase in data processing and the use of date-related information, the need for a standardized approach to date parsing led to the creation of various date/time libraries in different programming languages.

Alternative methods for date parsing in C++ include using the `get_time` function from the `iomanip` library or using regular expressions. However, the `strptime` function provides a more simple and efficient method for parsing dates from strings in C++.

It is worth noting that the `strptime` function is a POSIX standard, meaning it is implementation-defined and may vary across different Unix-like systems. Therefore, it is recommended to check the documentation of the specific system you are using for any variations.

## See Also:

- [C++ documentation on strptime](https://www.cplusplus.com/reference/ctime/strptime/)
- [Using time and date in C++ programs](https://www.geeksforgeeks.org/using-time-date-functions-c/)
- [An overview of date/time libraries in different programming languages](https://www.freecodecamp.org/news/everything-you-need-to-know-about-date-and-time-notation-in-programming/)