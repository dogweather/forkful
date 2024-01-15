---
title:                "Getting the current date"
html_title:           "Arduino recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why
When working with time-sensitive projects, it is important to keep track of the current date and time. This allows for accurate data logging, scheduling and timing of events, and displaying current information to users.

## How To
To get the current date on an Arduino board, we will be using the built-in Time library. First, we need to include the Time library in our code by adding `#include <Time.h>` at the top. Next, we will initialize the Time library by calling `setSyncProvider(getExternalTime)` in the `setup()` function. Then, we can use the `now()` function to get the current date and time, and use functions like `year()`, `month()`, and `day()` to extract specific information from the date. Let's take a look at a sample code to see this in action:

```
// Including the Time library
#include <Time.h>

void setup() {
  // Initializing the Time library
  setSyncProvider(getExternalTime);
}

void loop() {
  // Getting the current date and time
  time_t current_time = now();

  // Extracting the year, month, and day from the date
  int year = year(current_time);
  int month = month(current_time);
  int day = day(current_time);

  // Printing the current date to the serial monitor
  Serial.print("Today is: ");
  Serial.print(month);
  Serial.print("/");
  Serial.print(day);
  Serial.print("/");
  Serial.println(year);

  // Pausing for 1 second before repeating
  delay(1000);
}
```

Running this code will output the current date in the format of month/day/year to the serial monitor every second. You can modify the code to display the date in any format you want.

## Deep Dive
The `setSyncProvider()` function used in the previous code example has a parameter called `getExternalTime` which is a function that returns the current Unix time. This is the number of seconds that have elapsed since January 1, 1970. The Time library uses this Unix time as a reference to calculate the current date and time. Additionally, the Time library also has functions to set and adjust the system clock, as well as perform time zone calculations.

## See Also
Learn more about the Arduino Time library: https://www.arduino.cc/en/Reference/Time

Get an in-depth understanding of the Unix time: https://www.epochconverter.com/programming/c