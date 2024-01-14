---
title:                "Arduino recipe: Calculating a date in the future or past"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why

Have you ever wondered how to calculate a date in the future or past using an Arduino board? This skill can come in handy for various projects, such as creating a countdown timer or scheduling tasks. With just a few lines of code, you can easily calculate any date you desire.

## How To

To start, you will need to include the "Time" library in your sketch. This library provides functions for obtaining and manipulating time information. Once you have included the library, you can use the "now()" function to get the current time as a Unix timestamp. Keep in mind that a Unix timestamp represents the number of seconds that have passed since January 1, 1970.

Next, you will need to use the "makeTime()" function to create a new time structure that holds the desired date and time. This function takes in the year, month, day, hour, minute, and second as parameters and returns a Unix timestamp.

Now, to calculate a date in the future, you can simply add the desired number of seconds to the current timestamp using the "addTime()" function. Similarly, to calculate a date in the past, you can subtract the desired number of seconds using the "subTime()" function.

Here is a sample code for calculating a date 10 days in the future:

```Arduino
#include <Time.h>

void setup(){
  Serial.begin(9600);
  setTime(16, 32, 25, 15, 9, 2021); // set current time to September 15, 2021 at 4:32:25 PM
  time_t currentTime = now(); // get current time as Unix timestamp
  time_t futureTime = addTime(currentTime, 10*SECS_PER_DAY); // add 10 days to current time
  printDateAndTime(futureTime); // prints new date and time in "MM/DD/YYYY hh:mm:ss" format
}

void loop(){
  // do nothing
}

void printDateAndTime(time_t timestamp){
  String date = String(month(timestamp)) + "/" + String(day(timestamp)) + "/" + String(year(timestamp));
  String time = String(hour(timestamp)) + ":" + String(minute(timestamp)) + ":" + String(second(timestamp));
  Serial.print("Date: ");
  Serial.println(date);
  Serial.print("Time: ");
  Serial.println(time);
}
```

The above code would output:

```
Date: 9/25/2021
Time: 16:32:25
```

## Deep Dive

While the above method works for calculating simple dates, it may not be accurate for more complex situations, such as leap years or daylight saving time. In these cases, it is better to use the "convertDateToTimeStamp()" function. This function takes in the year, month, day, hour, minute, and second as parameters and returns a Unix timestamp, taking into account any leap years or daylight saving time changes.

It is also important to note that Arduino boards do not have a real-time clock, meaning they lose track of time when turned off. To overcome this issue, you can connect a real-time clock module to your Arduino board.

## See Also

For more information and examples on using the "Time" library, check out the following links:

- [Time Library Reference](https://www.arduino.cc/en/Reference/Time)
- [Arduino Time Library Examples](https://www.pjrc.com/teensy/td_libs_Time.html)