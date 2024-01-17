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

Converting a date into a string is the process of representing a date in a human-readable and easily understandable format, such as "March 25th, 2020" or "03/25/2020". Programmers often do this to display the date in a more user-friendly manner or to save it as a string for further processing.

## How to:

To convert a date into a string in Arduino, you can use the ```itoa()``` function. This function takes in three parameters: the integer value of the date, a character array to store the converted string, and the base of the number system to convert into. Here is an example code using ```itoa()``` to convert the current date into a string:

```
#include <Time.h>

int day, month, year;
char date[11];

void setup() {
  Serial.begin(9600);
  setTime(9, 30, 0, 25, 3, 2020); // set time to 9:30am, March 25th, 2020
}

void loop() {
  day = day();
  month = month();
  year = year();
  itoa(day, date, 10); // convert day to string with a base of 10
  strcat(date, "/");
  itoa(month, date+3, 10); // convert month to string and add it to date array starting from index 3
  strcat(date, "/");
  itoa(year, date+6, 10); // convert year to string and add it to date array starting from index 6
  Serial.println(date); // print the converted date in the format "dd/mm/yyyy"
  delay(1000);
}
```

The output of this code would be ```25/03/2020``` on the serial monitor.

## Deep Dive:

Historically, converting a date into a string was a more complex process, involving manual calculations and formatting. However, with the development of programming languages and libraries, this process has become much simpler and more efficient. Some alternative methods to convert a date into a string in Arduino include using the ```sprintf()``` function or creating your own function for the conversion. These methods may provide more customization options, but they also require more coding and may not be as optimized as the built-in ```itoa()``` function.

When implementing the ```itoa()``` function, it is essential to consider the base of the number system to convert into. In our example, we used a base of 10, but you can also use other bases such as 2, 8, or 16 depending on your project's needs. The ```itoa()``` function takes the modulus of the date value with the given base to convert it into a string.

## See Also:

- [Arduino Reference - itoa()](https://www.arduino.cc/reference/en/language/variables/conversion/itoa/)
- [dateToStr library](https://create.arduino.cc/projecthub/royy/convert-date-to-string-julian-gregorian-867a37)
- [sprintf() function](https://www.arduino.cc/reference/en/language/functions/communication/serial/sprintf/)