---
title:                "Comparing two dates"
html_title:           "Arduino recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?
Comparing two dates is the process of determining whether two given dates are the same, earlier, or later than each other. This is a common task for programmers when working with date and time data. By comparing dates, programmers can create conditional statements and perform actions based on date comparisons, making their code more efficient and dynamic.

## How to:
Coding examples using the Arduino language below:

```Arduino
// Comparing two dates using the if...else statement
int date1 = 20190501;
int date2 = 20190502;

if (date1 == date2) {
  Serial.println("The dates are the same");
} else if (date1 < date2) {
  Serial.println("date1 is earlier than date2");
} else {
  Serial.println("date1 is later than date2");
}

/*
Output:
date1 is earlier than date2
*/
```

```Arduino
// Comparing two dates using the switch statement
int date1 = 20190501;
int date2 = 20190502;

switch(date1 > date2) {
  case true:
    Serial.println("date1 is later than date2");
    break;
  case false:
    switch(date1 == date2) {
      case true:
        Serial.println("The dates are the same");
        break;
      case false:
        Serial.println("date1 is earlier than date2");
    }
    break;
}

/*
Output:
date1 is earlier than date2
*/
```

## Deep Dive:
- **Historical Context**: The concept of comparing two dates has been around since the invention of calendars and has evolved along with the advancements in technology. Before computers, dates were compared manually or with the help of mechanical devices like abacuses and slide rules. However, with the rise of digital computing, the process of comparing dates became more accurate and efficient.
- **Alternatives**: There are other ways to compare dates, such as using external date comparison libraries or creating custom functions. Depending on the project's requirements, different methods may be used to compare dates.
- **Implementation Details**: The examples above show the most basic way of comparing two dates using the if...else and switch statements. However, for more complex comparisons, additional logic and nested conditional statements may be necessary. It is important to consider the format and data type of the dates being compared to ensure accurate results.

## See Also:
- [Arduino Reference - Comparison Operators](https://www.arduino.cc/reference/en/language/structure/comparison-operators/)
- [Date Comparison Libraries for Arduino](https://www.hackster.io/ArduinoTeam/date-comparison-comparing-dates-can-be-a-pain-6197be)