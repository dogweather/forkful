---
title:                "Arduino recipe: Comparing two dates"
simple_title:         "Comparing two dates"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to compare two dates in an Arduino program? Whether you're tracking time or scheduling events, being able to compare dates is a fundamental skill in programming. In this blog post, we'll show you how to easily compare dates in Arduino using built-in functions and methods.

## How To

Comparing two dates is a common task in programming, but the process can seem daunting at first. However, with Arduino, it's simple and straightforward. Let's take a look at an example program using the ```millis()``` function to compare two dates:

```
Arduino

unsigned long date1 = millis(); //set first date
delay(500); //wait 0.5 seconds
unsigned long date2 = millis(); //set second date
if(date2 - date1 > 1000) { //if second date is more than 1 second after first date
  Serial.println("Second date is after first date."); //print message
} else { //if second date is equal or less than 1 second after first date
  Serial.println("Second date is not after first date."); //print message
}
```

In this example, we use the ```millis()``` function to get the number of milliseconds since the program started running. This allows us to accurately compare two dates without worrying about time zones or leap years. We then use a simple ```if/else``` statement to check if the second date is more than 1 second after the first date. If it is, we print a message stating that the second date is after the first date.

You can also use the ```Year()```, ```Month()```, and ```Day()``` methods to compare specific components of a date. Let's see how that works with a different example program:

```
Arduino

unsigned long date1 = millis(); //set first date
delay(500); //wait 0.5 seconds
unsigned long date2 = millis(); //set second date
if(Year(date2) > Year(date1)){ //if second date's year is after first date's year
  Serial.println("Second date's year is after first date's year."); //print message
} else if(Year(date2) == Year(date1)) { //if second date's year is equal to first date's year
  if(Month(date2) > Month(date1)) { //if second date's month is after first date's month
    Serial.println("Second date's month is after first date's month."); //print message
  } else if(Month(date2) == Month(date1)) { //if second date's month is equal to first date's month
    if(Day(date2) > Day(date1)) { //if second date's day is after first date's day
      Serial.println("Second date's day is after first date's day."); //print message
    } else { //if second date's day is equal or less than first date's day
      Serial.println("Second date's day is not after first date's day."); //print message
    }
  }
}
```

In this example, we use the ```if/else if``` statements to compare the year, month, and day components of two dates. This allows for more specific comparisons and can be useful in certain situations.

## Deep Dive

When comparing two dates, it's important to keep in mind how the dates are stored and the limitations of the data types used. In Arduino, the ```millis()``` function returns an ```unsigned long``` data type, which can hold values up to 4,294,967,295. This means that the date comparison will only be accurate up to that number of milliseconds since the start of the program. If your program runs for a long time, you may need to use a different approach or data type to accurately compare two dates.

It's also important to note that the ```millis()``` function returns the number of milliseconds since the program started, not the actual date and time. This means that if you need to compare dates and times in a real-world scenario, you may need to use a real-time clock (RTC) module and associated libraries to get accurate results.

## See Also

- Official Arduino Website: https://www.arduino.cc/
- Arduino Reference: https://www.arduino.cc/reference/en/
- Arduino Forum: https://forum.arduino.cc/