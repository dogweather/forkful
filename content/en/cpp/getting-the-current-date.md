---
title:                "Getting the current date"
html_title:           "C++ recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?

Getting the current date in a C++ program means getting the date on the system's clock at the exact moment the program is running. This is useful for tasks such as time-stamping logs or scheduling events. Programmers use this function to ensure their programs are using the current date and time for accurate and timely processing.

## How to:

To get the current date in C++, we can use the built-in <ctime> library. Below is an example code using the <ctime> library to get the current date and print it out in the format of "Month/Day/Year":

```C++
#include <ctime>
#include <iostream>

int main() {
  // declare variables to store current date and time
  time_t now = time(0);
  tm *ltm = localtime(&now);

  // print current date and time
  std::cout << ltm->tm_mon + 1 << "/" << ltm->tm_mday << "/" << 1900 + ltm->tm_year << std::endl;

  return 0;
}
```

Below is the sample output for this code:

```
6/7/2021
```

## Deep Dive:

- **Historical context:** In earlier versions of C++, getting the current date required using a combination of different time-related functions, making it a complex and time-consuming process. The addition of the <ctime> library in the current version of C++ has made getting the current date a much simpler and more convenient task.

- **Alternatives:** As an alternative to using the <ctime> library, programmers can also use external libraries or APIs that provide more advanced features for getting the current date and time, such as including time zones or international date formats.

- **Implementation details:** In the C++ standard library, the <ctime> header file provides the struct "tm" which holds information about the current date and time. The "time_t" data type represents time values in seconds, while the "localtime" function converts this value into a readable and local time format.

## See Also:

- C++ reference for the <ctime> library: https://www.cplusplus.com/reference/ctime/

- Alternative libraries for getting the current date in C++: https://www.cplusplus.com/reference/datetime/