---
title:    "C++ recipe: Converting a date into a string"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Why

Converting a date into a string may seem like a trivial task, but it has several practical applications. By converting a date into a string, we can easily display it in a user-friendly format, store it in a database, or use it in other string manipulation operations.

## How To

To convert a date into a string in C++, we can use the `std::to_string()` function. This function takes in a date variable and returns a string representation of it. Let's look at an example:

```C++
//include necessary libraries
#include <iostream>
#include <string>

int main() {
    //define date variables
    int day = 10;
    int month = 8; 
    int year = 2021;

    //convert date into string
    std::string date = std::to_string(day) + "/" + std::to_string(month) + "/" + std::to_string(year);

    //print string representation of date
    std::cout << "Today's date is: " << date << std::endl;

    return 0;
}
```

**Output:**

```
Today's date is: 10/8/2021
```

We can see that by using the `std::to_string()` function, we were able to convert our date variables into a string and display it in a user-readable format. This is just one of the many ways we can use this function for converting a date into a string. 

## Deep Dive

In the `std::to_string()` function, the date variable is first converted into a temporary `std::basic_string` object before being returned as a string. This means that it is not a direct conversion, and we need to be careful about the data type of the variable we are trying to convert. 

For example, if we try to convert a `float` or `double` variable, it will be rounded off to its nearest integer before being converted into a string. This might not be the desired output for some applications. Therefore, it is essential to understand the data types and make sure they are compatible with the `std::to_string()` function. 

## See Also

- [std::to_string - C++ Reference](https://www.cplusplus.com/reference/string/to_string/)
- [Converting Between Strings and Numbers in C++](https://www.learncpp.com/cpp-tutorial/numbers-converting-between-bases/)