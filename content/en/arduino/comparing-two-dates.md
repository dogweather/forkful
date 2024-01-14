---
title:    "Arduino recipe: Comparing two dates"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

Using Arduino to compare two dates can be a helpful tool when creating projects that involve tracking and organizing time. This feature allows for more precise control over events and can enhance the functionality of your project.

## How To

To compare two dates on Arduino, you will first need to declare two variables with the data type "time_t". This data type is used to store date and time values. For example:

```Arduino
time_t date1 = {year, month, day, hours, minutes, seconds};
time_t date2 = {year, month, day, hours, minutes, seconds};
```
Next, you can use the built-in function "difftime()" to calculate the difference between the two dates. The syntax for this function is:

```Arduino
long difftime(time_t timer2, time_t timer1);
```

Where "timer1" is the starting date and "timer2" is the ending date. This function will return the difference between the two dates in seconds. You can then convert this value into a more readable format, such as days, hours, and minutes, by dividing by the appropriate time unit.

```Arduino
long difference = difftime(date2, date1);
int days = difference / 86400; // 86400 seconds in a day
int hours = (difference % 86400) / 3600; // 3600 seconds in an hour
int minutes = ((difference % 86400) % 3600) / 60; // 60 seconds in a minute

Serial.print("Difference: ");
Serial.print(days);
Serial.print(" days, ");
Serial.print(hours);
Serial.print(" hours, ");
Serial.print(minutes);
Serial.println(" minutes");
```

## Deep Dive

When comparing two dates, it is important to take into account things like leap years and daylight saving time. Fortunately, the "time_t" data type automatically adjusts for these factors, making it a reliable method for tracking time.

Additionally, the "difftime()" function also takes into account time zones. By default, it will use the local time zone of the Arduino board. However, you can adjust this by setting the time zone using the "setTimezone()" function from the "Timezone" library.

## See Also

- [Arduino Reference](https://www.arduino.cc/reference/en/language/functions/time/difftime/)
- [Using Arduinos to Build Timers and Clocks](https://hackaday.com/2012/10/15/using-arduinos-to-build-timers-and-clocks/)
- [Time and Date Library for Arduino](https://www.pjrc.com/teensy/td_libs_Time.html)