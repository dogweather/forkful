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

## Why 

Getting the current date is a common task in programming and it can serve various purposes like tracking the time of an event or generating timestamps for data.

## How To 

To get the current date in C++, we can use the <ctime> library and the function "time(0)". Here's an example:

```C++
#include <iostream> 
#include <ctime> 

int main() 
{ 
    // create a variable to store current time 
    time_t currentTime = time(0); 

    // convert the current time into a string 
    char* currentDateTime = ctime(&currentTime); 

    // print the current date and time 
    std::cout << "The current date and time is: " << currentDateTime << std::endl; 

    return 0; 
} 
```

Output:
```
The current date and time is: Sun Nov 10 17:53:28 2019
```

We can also format the current date and time using the <iomanip> library and the "std::put_time" function. Here's an example:

```C++
#include <iostream> 
#include <iomanip> 
#include <ctime> 

int main() 
{ 
    // create a variable to store current time 
    time_t currentTime = time(0); 

    // convert the current time into a struct 
    struct tm * now = localtime(&currentTime); 

    // format the date and time 
    std::cout << "The current date and time is: "; 
    std::cout << std::put_time(now, "%Y-%m-%d %X") << std::endl; 

    return 0; 
} 
```

Output: 
```
The current date and time is: 2019-11-10 17:55:28
```

## Deep Dive 

The "time(0)" function returns the number of seconds since the Unix epoch (January 1, 1970). This can be useful for calculating time differences between events or generating unique timestamps. 

We can also use the <chrono> library to get the current date and time with higher precision. Here's an example:

```C++
#include <iostream> 
#include <chrono>

int main() 
{ 
    // get current system time 
    auto currentTime = std::chrono::system_clock::now(); 

    // convert time to time_t object 
    std::time_t currentDateTime = std::chrono::system_clock::to_time_t(currentTime); 

    // print current date and time 
    std::cout << "The current date and time is: " << std::ctime(&currentDateTime) << std::endl; 

    return 0; 
} 
```

Output: 
```
The current date and time is: Sun Nov 10 18:00:05 2019
```

## See Also 

- <ctime> library documentation: http://www.cplusplus.com/reference/ctime/ 
- <chrono> library documentation: http://www.cplusplus.com/reference/chrono/