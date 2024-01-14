---
title:    "Arduino recipe: Converting a date into a string"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Why

Converting dates into strings may seem like a simple task, but it can be incredibly useful when working with Arduino programs. By converting dates into strings, you can easily display and manipulate date information in your projects. This can come in handy for projects that involve tracking time, scheduling tasks, or displaying timestamps.

## How To

To convert a date into a string in Arduino, you can use the `strftime()` function from the `time.h` library. This function takes in a date and a format string, and returns a formatted string representing the date. Let's look at an example where we convert the current date into a string in the format of "mm/dd/yyyy":

```Arduino
#include <TimeLib.h> // include the TimeLib library

void setup() {
    Serial.begin(9600); // initialize serial communication
    setTime(15, 30, 0, 15, 10, 2021); // set the date and time (hour, minute, second, day, month, year)
}

void loop() {
    char dateStr[11]; // create a char array to store the formatted date
    strftime(dateStr, 11, "%m/%d/%Y", &now()); // use strftime() to convert the current date into a string
    Serial.println(dateStr); // print the formatted date to the serial monitor
}
```

The output in the serial monitor would be: `10/15/2021`.

As you can see, the `strftime()` function takes in three arguments: the char array to store the formatted date, the size of the array, and the format string. The format string is used to specify the desired format of the date. You can use different format specifiers, such as `%d` for day, `%m` for month, and `%Y` for year, to customize the format to your liking.

## Deep Dive

Behind the scenes, the `strftime()` function uses the `time.h` library to access the internal timekeeping functions of the Arduino. This library provides access to system time in seconds since January 1, 1970. The `setTime()` function, used in our example, sets the internal clock to a specific date and time. This, in turn, allows us to use the `now()` function in `strftime()` to get the current date and time.

It's also worth noting that the `strftime()` function is not limited to just dates; it can also be used to convert time information, such as hours, minutes, and seconds, into strings. You can explore the different format specifiers and experiment with different formats to suit your project's needs.

## See Also

- [Arduino Documentation: TimeLib Library](https://www.arduino.cc/en/Reference/Time)
- [C++ Reference: strftime() function](https://www.cplusplus.com/reference/ctime/strftime/)