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

## What & Why?

Converting a date into a string in Arduino programs involves transforming a specific instance in time into a human-readable format. Programmers do it to display, log, or process date/time data in a way that people can easily understand.

## How to:

Here's a simple approach on how to convert date and time into a string with Arduino. You can use "sprintf" function, which is similar to "printf" but it prints the output as a string.

```Arduino
#include <TimeLib.h>

void setup() {
  Serial.begin(9600);
  
  setTime(10,30,0,1,1,2022); // set time to 10:30:00am Jan 1 2022
}

void loop() {
  time_t t = now();
  char buffer[20];
  sprintf(buffer, "%02d:%02d:%02d %02d/%02d/%4d", hour(t), minute(t), second(t), day(t), month(t), year(t));
  Serial.println(buffer);
  
  delay(1000); // update every second
}
```
When you run this code, it outputs the current time as a string in the format: `10:30:00 01/01/2022`.

## Deep Dive

The "sprintf" function has been around since the early days of C, providing a way to print formatted data. Though very useful, it's worth noting things can get messy if you're not careful with buffer sizes — resulting in overflow errors.

An alternative to using "sprintf" is to use separate char arrays for each segment of the date and then concatenate them.

```Arduino
char yearString[5];
char dateBuffer[11]; // holds "DD/MM/YYYY\0"

itoa(year(t), yearString, 10);
strcpy(dateBuffer, dayStr(t));
strcat(dateBuffer, "/");
strcat(dateBuffer, monthStr(t));
strcat(dateBuffer, "/");
strcat(dateBuffer, yearString);

Serial.println(dateBuffer);
```

A deeper implementation detail to note is that the Time library used above provides functions like `hour()`, `minute()`, etc., which extract the respective values from a `time_t` value - the number of seconds since the UNIX epoch (1970-01-01 00:00:00). 

## See Also

1. Arduino official reference on `sprintf`: [sprintf - Arduino Reference](https://www.arduino.cc/reference/en/language/functions/characters/strings/sprintf/).

2. A useful introduction to Time library is available at: [Arduino - Time Library](https://playground.arduino.cc/Code/time/).

3. More info on `time_t`: [time_t - C++ Reference](http://www.cplusplus.com/reference/ctime/time_t/).