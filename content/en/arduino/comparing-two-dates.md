---
title:    "Arduino recipe: Comparing two dates"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Why

Comparing dates is a common task when working with time-based data or creating scheduling applications. By comparing two different dates, you can determine which one is earlier, later, or if they are the same, which can be helpful in decision making for your project. In this blog post, we will explore how to compare two dates using Arduino programming.

## How To

To begin, we will need to define two variables for our dates. These variables should be of the `Date` data type, which represents a specific point in time. We can use `DateTime` library in Arduino to work with dates and times.

```Arduino
#include <DateTime.h>

// Define two Date variables
Date date1, date2;

void setup() {
  // Initialize serial communication
  Serial.begin(9600);

  // Set the first date to October 1st, 2021
  date1 = Date(10, 1, 2021);
  // Set the second date to September 30th, 2021
  date2 = Date(9, 30, 2021);
}

void loop() {
  // Compare the two dates using the `isEarlier()` and `isSame()` functions
  if (date1.isEarlier(date2)) {
    Serial.println("Date 1 is earlier than Date 2");
  }
  else if (date1.isSame(date2)) {
    Serial.println("Date 1 and Date 2 are the same");
  }
  else {
    Serial.println("Date 1 is later than Date 2");
  }
  delay(1000); // Delay for one second
}
```

The `isEarlier()` function compares the two dates and returns `true` if the first date is earlier than the second date, and `false` if it is not. Similarly, the `isSame()` function compares the two dates and returns `true` if they are the same, and `false` if they are not. In our example, the output would be "Date 1 is later than Date 2" since October 1st is after September 30th.

## Deep Dive

Arduino's `Date` data type is based on the `time_t` data type, which represents the number of seconds since January 1st, 1970. This is known as the Unix Epoch and is commonly used in programming for time calculations. The `Date` class in the `DateTime` library has various functions for working with dates, such as adding or subtracting days, months, or years, as well as converting to and from different formats. You can explore these functions in more detail in the [DateTime library documentation](https://github.com/PaulStoffregen/DateTime).

When comparing two dates, it is important to consider time zones and daylight saving time. The `Date` constructor takes an optional time zone parameter, allowing you to specify the time zone for your date. Additionally, the `DateTime` library has functions for converting between local time and UTC time, which can be helpful when working with time zones.

## See Also

- [DateTime library documentation](https://github.com/PaulStoffregen/DateTime)
- [Arduino Date and Time functions](https://www.arduino.cc/en/Reference/RTC)
- [Unix Epoch](https://en.wikipedia.org/wiki/Unix_time)