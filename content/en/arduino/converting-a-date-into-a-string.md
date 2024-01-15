---
title:                "Converting a date into a string"
html_title:           "Arduino recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why

Have you ever wanted to display the current date in a specific format on your Arduino project? Maybe you want to include the date in a data-logging system or display it on an LCD screen. Whatever your reason may be, converting a date into a string is an essential skill for any Arduino programmer.

## How To

Converting a date into a string on Arduino is a straightforward process that can be achieved using the standard `sprintf()` function. Let's take a look at an example of how we can use this function to display the current date in the format *Day, Month Date, Year*.

``` Arduino
#include <TimeLib.h>
#include <Time.h>

void setup() {
  // initialize serial communication
  Serial.begin(9600);

  // wait for Serial Monitor to open
  while (!Serial);

  // set the current time and date
  setTime(11, 0, 0, 15, 1, 2020);

  // create a char array to store the date
  char date[20];

  // use sprintf function to format the date
  sprintf(date, "%s %02d, %s %04d", dayStr(weekday()), day(), monthStr(month()), year());

  // print the date to the Serial Monitor
  Serial.println(date);
}

void loop() {
  // do nothing
}
```

**Output:**

```
Wed 15, Jan 2020
```

This code snippet uses the `setTime()` function from the TimeLib library to set the current time and date. Then, the `sprintf()` function is used to store the formatted date into the `date` char array. Finally, the date is printed to the Serial Monitor using the `println()` function.

Feel free to experiment with different formats and use cases to display the date in your project.

## Deep Dive

Now, let's take a deeper look at how the `sprintf()` function works. This function is used to format and store a series of characters and values into a string, similar to the `printf()` function in languages like C.

The first argument of the `sprintf()` function is the array or pointer where the resulting string will be stored. The remaining arguments are the values that will be formatted and inserted into the string.

In our example, we used the `%s` and `%d` specifiers to format the string with the day, month, and year values. You can find a complete list of specifiers and their corresponding data types on the Arduino Reference page.

## See Also

Check out these resources for more information on converting a date into a string on Arduino:

- [Arduino Reference - sprintf()](https://www.arduino.cc/reference/en/libraries/time/time-lib/sprintf/)
- [C++ Reference - printf()](https://www.cplusplus.com/reference/cstdio/printf/)
- [TimeLib Library](https://github.com/PaulStoffregen/Time)