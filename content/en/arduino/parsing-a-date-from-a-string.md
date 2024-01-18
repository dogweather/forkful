---
title:                "Parsing a date from a string"
html_title:           "Arduino recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?

Parsing a date from a string is the process of extracting date information from a string of characters. This is commonly done in programming when working with date/time data. It allows programmers to easily manipulate and work with dates in their code without having to manually input and format each individual date.

## How to:

```Arduino
// Input string with date in format "MM/DD/YYYY"
String date = "02/14/2020";

// Convert string to array of characters
char dateString[] = date.toCharArray();

// Extract month, day, and year from string
int month = atoi(strtok(dateString, "/"));
int day = atoi(strtok(NULL, "/"));
int year = atoi(strtok(NULL, "/"));

// Print parsed date
Serial.print("Month: ");
Serial.println(month);
Serial.print("Day: ");
Serial.println(day);
Serial.print("Year: ");
Serial.println(year);
```

Sample Output:
```
Month: 02
Day: 14
Year: 2020
```

## Deep Dive:

Historically, parsing dates from strings was a more complex task for programmers. They had to manually read and interpret the characters in the string to determine the date information. However, with the advancement of programming languages and tools, this process has become much simpler.

An alternative method to parsing dates from strings is to use libraries or built-in functions specifically designed for handling date/time data. For example, the ```TimeLib``` library for Arduino includes functions for converting strings to date/time objects.

When parsing a date from a string, it is important to consider the format of the date and how it may vary. For example, the date "02/14/2020" could also be written as "2/14/20". This would require additional logic in the code to account for different formats.

## See Also:

- [TimeLib Library for Arduino](https://playground.arduino.cc/Code/Time/)
- [Parsing Dates and Times in C](https://www.gnu.org/software/libc/manual/html_node/Parsing-Dates-and-Times.html)