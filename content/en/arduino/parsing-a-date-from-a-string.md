---
title:                "Parsing a date from a string"
html_title:           "C recipe: Parsing a date from a string"
simple_title:         "Parsing a date from a string"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Parsing Date from a String with Arduino: A concise guide

## What & Why?

Parsing a date from a string involves extracting usable date information from a given text. Programmers do it to convert and manipulate date data when handling various user inputs and interactions.

## How to:

Ready to parse some dates? Here's a practical example using Arduino's built-in functions.

```Arduino
String dateStr = "2022-07-01"; // this is your date string.  

int Year = dateStr.substring(0, 4).toInt(); 
int Month = dateStr.substring(5, 7).toInt(); 
int Day = dateStr.substring(8, 10).toInt(); 

Serial.println(Year); // 2022
Serial.println(Month); // 07  
Serial.println(Day); // 01  
```

This code snippet gets the year, month, and day from a string in the "YYYY-MM-DD" format and converts them into integer values.

## Deep Dive

Parsing dates from strings isn't a new trick. Dating back to the early days of computing, it's been a necessity for dealing with time-related data. Yet, the problem of interpreting dates correctly remains. This little issue is related to the different date formats used worldwide.

As an alternative to our example, the Arduino Time Library offers built-in functions for time manipulation, including parsing strings. But be aware: it requires more memory which could be a constraint on Arduino devices with limited resources. 

When using the `substring()` function, remember it doesn't check if the data makes sense as a date. For instance, it wouldn't bat an eyelid at "9999-99-99". So, you might want to include additional error-checking code to handle such scenarios.

## See Also

Want to know more about how string manipulation works in Arduino? Dive into the Official Arduino Reference for String:
[String Object](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject)

For a more detailed approach to time manipulation using Arduino, there's the Official Arduino Time Library Documentation:
[Time Library](https://www.arduino.cc/reference/en/libraries/time)

Ready to explore more about error checking and validations? This community post provides detailed explanations:
[Arduino Community: Error Checking](https://forum.arduino.cc/index.php?topic=396450.0)