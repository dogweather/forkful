---
title:                "C++ recipe: Converting a date into a string"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

As a programmer, you may often encounter situations where you need to convert a date into a string. This can be useful for displaying dates in a specific format or for storing dates in a database. In this blog post, we will explore how to do this in C++.

## How To 

Converting a date into a string in C++ involves using the `std::stringstream` class. This class allows us to convert different types of data into strings. Firstly, we will need to include the `<sstream>` header file in our program. Then, we can use the `std::stringstream` object to convert the date into a string as shown in the code below.

```C++
#include <iostream>
#include <sstream>

int main()
{
    // Create a date object
    int day = 12;
    int month = 9;
    int year = 2021;

    // Create a stringstream object
    std::stringstream ss;

    // Insert the date into stringstream object
    ss << day << "/" << month << "/" << year;

    // Convert the stringstream object into a string
    std::string date_str = ss.str();

    // Print the output
    std::cout << "Date in string format: " << date_str << std::endl;

    return 0;
}
```

**Output:**

```
Date in string format: 12/9/2021
```

## Deep Dive

The `std::stringstream` class allows us to perform operations on strings just like we do with streams of characters. This means we can use functions like `<<` (insertion) and `>>` (extraction) to modify the string. In the above code, we inserted the date values into the `ss` object using the `<<` operator. Then, we used the `str()` function to convert the `ss` object into a string.

It is also important to note that the format of the date string will depend on the locale of your system. If you want to convert the date into a specific format, you can use other functions like `std::put_time` or `strftime` from the `<ctime>` library to format the date before converting it into a string.

## See Also

If you want to learn more about converting data types in C++, you may find these resources useful:

- [String streams in C++](https://www.geeksforgeeks.org/string-stream-in-c/)
- [C++ Date and Time](https://www.programiz.com/cpp-programming/library-function/ctime)
- [Converting data types in C++](https://www.cplusplus.com/doc/tutorial/typecasting/)