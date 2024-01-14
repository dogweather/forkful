---
title:                "C++ recipe: Getting the current date"
programming_language: "C++"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

As a programmer, it's important to have a good understanding of how to get the current date in your code. This allows you to create time-sensitive applications and keep track of important events.

## How To

Getting the current date in C++ is a simple process. First, we need to include the `<ctime>` library in our code. This library provides functions for manipulating date and time.

```
#include <iostream>
#include <ctime>

using namespace std;
```

Next, we can define a variable of type `time_t` to store the current date and time.

```
time_t now = time(0);
```

We can then use the `ctime` function to convert this `time_t` variable into a human-readable format.

```
// Convert time_t to string
string date = ctime(&now);

// Print out current date
cout << "Today is: " << date << endl;
```

Running this code will output the current date in the format "Day of Week Month Day HH:MM:SS Year". For example, "Mon Feb 01 09:00:00 2021".

## Deep Dive

The `time()` function used to get the current date and time returns the number of seconds elapsed since January 1, 1970. This time is commonly referred to as the Unix Epoch or Unix Time. In order to convert this into a human-readable format, the `ctime()` function uses the system's current time zone.

There are also other useful functions in the `<ctime>` library, such as `localtime()` and `gmtime()`, which allow us to retrieve the current date and time in a specific time zone or in Greenwich Mean Time (GMT) respectively.

Another important aspect to note is that the time retrieved using `time()` is dependent on the system's clock. If the system clock is changed, the date and time retrieved using `time()` will also be affected.

## See Also

- [C++ Reference: <ctime> library](https://www.cplusplus.com/reference/ctime/)
- [Unix time - Wikipedia](https://en.wikipedia.org/wiki/Unix_time)
- [The Unix Epoch](https://www.epochconverter.com/)