---
title:    "C++ recipe: Converting a date into a string"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/cpp/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

Converting a date into a string may seem like a trivial task, but it can actually be quite useful in certain scenarios. For example, if you're working with an API that requires a date to be in string format, or if you want to display a date in a specific format for your user interface, then this skill will come in handy.

## How To

To convert a date into a string, we will be using the `strftime()` function from the `<ctime>` header. This function takes in two parameters: a format specifier and a `tm` struct that contains the date and time information.

Let's take a look at an example of converting the current date into a string in the format of `MM/DD/YYYY`.

```C++
#include <iostream>
#include <ctime>

int main() {
    // get current date and time
    time_t now = time(0);

    // convert to tm struct
    tm *today = localtime(&now);

    // create char array to store string
    char date_str[11];

    // use strftime to format date
    strftime(date_str, 11, "%m/%d/%Y", today);

    std::cout << "Today's date is: " << date_str << std::endl;

    return 0;
}
```

Output:

```
Today's date is: 12/09/2021
```

Here, we use the `%m` for month, `%d` for day, and `%Y` for year in our format specifier. It's important to note that the `tm` struct contains other elements such as hours, minutes, and seconds, but we only use these three for our desired format. You can check out the [strftime documentation](https://www.cplusplus.com/reference/ctime/strftime/) for more format specifiers and their meanings.

## Deep Dive

If you're curious about how the `strftime()` function actually works, let's take a deeper look. The function takes in a format string, which is essentially a combination of literal text and conversion specifiers, and converts it into a string representation of the `tm` struct. It does this by iterating through the format string and replacing any conversion specifiers with their corresponding values from the `tm` struct.

For example, in the previous code sample, the `%m` conversion specifier is replaced with the current month, which is December (represented by 12). It then stores this value in the `date_str` array. This process is repeated for every conversion specifier in the format string.

## See Also

- [strftime documentation](https://www.cplusplus.com/reference/ctime/strftime/)
- [tm struct documentation](https://www.cplusplus.com/reference/ctime/tm/)