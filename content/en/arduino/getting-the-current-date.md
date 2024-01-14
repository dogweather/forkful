---
title:                "Arduino recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why Engage in Getting the Current Date

Have you ever wanted to incorporate the current date into your Arduino programming projects? Whether you want to keep track of time or create timed events, getting the current date can be a useful skill to have. In this blog post, we will discuss how you can easily retrieve the current date on your Arduino board.

## How To Get the Current Date in Arduino

Coding an Arduino board to get the current date is a simple process. First, you need to include the Time library by going to `Sketch > Include Library > Time`. Then, you can use the `now()` function to get the current date and time in a number format. Finally, you can convert the number into a readable format using the `year()`, `month()`, `day()`, `hour()`, `minute()`, and `second()` functions.

```
Arduino Code:

#include <Time.h>

void setup() {
  Serial.begin(9600);
  while(!Serial) {
    // wait for serial connection
  }
  setSyncProvider(RTC.read); // sync the time to the RTC
  if(timeStatus() != timeSet) {
    Serial.println("Unable to sync with the RTC");
  }
}

void loop() {
  time_t now = now(); // retrieve the current date and time
  int year = year(now); // extract the year from the date
  int month = month(now); // extract the month from the date
  int day = day(now); // extract the day from the date
  int hour = hour(now); // extract the hour from the time
  int minute = minute(now); // extract the minute from the time
  int second = second(now); // extract the second from the time
  // print the current date and time
  Serial.print(year); Serial.print("-"); 
  Serial.print(month); Serial.print("-");
  Serial.print(day); Serial.print(" ");
  Serial.print(hour); Serial.print(":");
  Serial.print(minute); Serial.print(":");
  Serial.println(second);
  delay(1000); // wait for a second
}
```

### Sample Output:

```
2021-10-04 12:45:21
```

## Deep Dive: How Does It Work?

Now, let's take a closer look at the code. We are using the Time library to access the `now()` function, which returns the current date and time in a number format called `time_t`. This format counts the seconds since a specific reference date (January 1, 1970). 

To convert the number into a readable format, we use the `year()`, `month()`, `day()`, `hour()`, `minute()`, and `second()` functions. These functions extract the respective values from the `time_t` format.

Additionally, we use the `setSyncProvider()` function to synchronize the time with a real-time clock (RTC) module on the Arduino board. This ensures that the current date and time are accurate.

## See Also

- [Time Library for Arduino](https://www.arduino.cc/reference/en/libraries/time/)
- [Tutorial: Using the Time.h library on Arduino](https://www.arduino.cc/en/Tutorial/libraries/time)