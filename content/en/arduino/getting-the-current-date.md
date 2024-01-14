---
title:    "Arduino recipe: Getting the current date"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Why

As an Arduino programmer, you may find yourself wanting to incorporate time and date functionality into your projects. Having access to the current date can be incredibly useful in creating timing-based tasks or displaying real-time data. With a little bit of programming, you can easily get the current date on your Arduino board.

## How To

Obtaining the current date on an Arduino can be done using the built-in `millis()` function, which returns the number of milliseconds since the board started running. This can be used as a starting point to calculate the current date and time. Take a look at the code snippet below to see how this is done:

```Arduino
unsigned long currentTime = millis();  // store current milliseconds in a variable.
unsigned long days = currentTime / (1000 * 60 * 60 * 24);  // convert milliseconds into days.
int currentYear = 1970 + days / 365;  // calculate current year.
int currentDay = days % 365;  // calculate current day of the year.
int currentMonth;  // declare current month variable.

// use conditional statements to determine current month.
if (currentDay <= 31) {
  currentMonth = 1;  // January
} else if (currentDay <= 59) {
  currentMonth = 2;  // February
  currentDay -= 31;
} else if (currentDay <= 90) {
  currentMonth = 3;  // March
  currentDay -= 59;
} else if (currentDay <= 120) {
  currentMonth = 4;  // April
  currentDay -= 90;
} else if (currentDay <= 151) {
  currentMonth = 5;  // May
  currentDay -= 120;
} else if (currentDay <= 181) {
  currentMonth = 6;  // June
  currentDay -= 151;
} else if (currentDay <= 212) {
  currentMonth = 7;  // July
  currentDay -= 181;
} else if (currentDay <= 243) {
  currentMonth = 8;  // August
  currentDay -= 212;
} else if (currentDay <= 273) {
  currentMonth = 9;  // September
  currentDay -= 243;
} else if (currentDay <= 304) {
  currentMonth = 10;  // October
  currentDay -= 273;
} else if (currentDay <= 334) {
  currentMonth = 11;  // November
  currentDay -= 304;
} else if (currentDay <= 365) {
  currentMonth = 12;  // December
  currentDay -= 334;
}

// print current date to the serial monitor.
Serial.print(currentDay);
Serial.print("/");
Serial.print(currentMonth);
Serial.print("/");
Serial.print(currentYear);
```

Running this code will display the current date in the serial monitor in the format of day/month/year. The `currentDay`, `currentMonth`, and `currentYear` variables can also be used in other parts of your code to perform time-sensitive tasks.

## Deep Dive

In the above code, we used the `millis()` function to get the number of milliseconds since the board started running. However, this value will eventually overflow and reset, so it is not a reliable way to keep track of the current date and time indefinitely. To solve this issue, a real-time clock (RTC) can be used.

An RTC is a dedicated chip that keeps track of accurate date and time, even when the Arduino board is powered off. These chips are relatively inexpensive and can easily be connected to an Arduino board via I2C communication. Libraries such as "DS3231" or "DS1307" can be used to interface with the RTC chip and retrieve the current date and time.

Using an RTC chip is a more accurate and reliable method for keeping track of the current date and time on an Arduino. It also allows for more advanced time and date-related functions, such as setting alarms or creating custom date formats.

## See Also

- [Arduino millis() function documentation](https://www.arduino.cc/reference/en/language/functions/time/millis/)
- [DS3231 RTC chip](https://www.mouser.com/datasheet/2/737/DS3231-1131771.pdf)