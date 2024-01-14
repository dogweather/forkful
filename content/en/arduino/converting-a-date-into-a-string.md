---
title:    "Arduino recipe: Converting a date into a string"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/arduino/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why 

If you're working on a project that involves keeping track of time, you may come across the need to convert a date into a string. This can be useful for displaying the current date on a digital clock or even recording timestamps for data logging purposes. In this blog post, we will explore how to convert a date into a string with an Arduino board. 

## How To 

To convert a date into a string with an Arduino board, we will be using the built-in `sprintf()` function. This function allows us to format and store a string into a specific variable. Placing the date information in this string will allow us to easily manipulate and display it as needed. 

```Arduino
#include <TimeLib.h>

// Creating a char array to store the date as a string
char dateStr[12];

void setup() {
  // Initializing serial communication for debugging
  Serial.begin(9600);

  // Setting the initial time - replace with current date and time
  setTime(11, 59, 30, 31, 12, 2019);
}

void loop() {
  // Using sprintf to format the date into a string
  sprintf(dateStr, "%02d/%02d/%04d", day(), month(), year());

  // Printing the date string to the serial monitor
  Serial.println(dateStr);

  delay(1000); // Delay for 1 second
}
```

With the above code, our output would be: `31/12/2019`, which is the current date in the format of DD/MM/YYYY. You can change the formatting by altering the string in the `sprintf()` function. 

## Deep Dive 

Now let's take a closer look at how the `sprintf()` function works. The first argument of this function is the variable in which the formatted string will be stored. The second argument is the format string, which specifies the desired format of the output. The rest of the arguments are the variables or values to be inserted into the format string. 

There are many different formatting options that can be used, but here are a few common ones for dates: 

- `%02d` - represents a decimal number with a minimum of 2 digits (e.g. 05)
- `%04d` - represents a decimal number with a minimum of 4 digits (e.g. 2019)
- `%2d` - represents a decimal number with a minimum of 2 digits but no leading zeros (e.g. 5)
- `%b` - represents the abbreviated month name (e.g. Dec)
- `%B` - represents the full month name (e.g. December)
- `%m` - represents the month number with leading zeros (e.g. 12)
- `%d` - represents the day of the month with leading zeros (e.g. 31)
- `%Y` - represents the full year (e.g. 2019)

For a full list of formatting options, you can refer to the [Arduino sprintf() documentation](https://www.arduino.cc/reference/en/language/functions/communication/sprintf/).

## See Also 

- [TimeLib library](https://www.arduino.cc/en/Reference/Time)
- [Arduino sprintf() documentation](https://www.arduino.cc/reference/en/language/functions/communication/sprintf/)