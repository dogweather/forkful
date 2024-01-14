---
title:                "Arduino recipe: Comparing two dates"
programming_language: "Arduino"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Why

Comparing two dates may seem like a simple task, but it can actually be quite complex. Whether you are trying to check if a certain date has passed or if two events occur on the same day, understanding how to compare dates can be a useful skill in many projects. In this blog post, we will explore how to compare dates using Arduino programming.

## How To

To compare two dates in Arduino, we will use the built-in "millis()" function. This function returns the number of milliseconds that have passed since the Arduino board began running the current program. We can use this value to create a reference point in time.

Let's take a look at an example where we want to check if a certain amount of time has passed since the Arduino board was turned on. First, we need to create a variable to store the reference point:

```Arduino
long startTime = millis(); //store the current time in milliseconds
```

Next, we can use a conditional statement to check if the current time minus the reference point is equal to or greater than the desired time interval. For example, if we want to check if 5 seconds have passed, our code would look like this:

```Arduino
if (millis() - startTime >= 5000) { //check if the current time minus the reference point is greater than or equal to 5 seconds
    //do something
}
```

Now, let's look at an example where we want to check if two dates are the same. We will use the "day()" and "month()" functions to get the day and month of each date, and then use a conditional statement to compare them. Our code would look like this:

```Arduino
int firstDay = day(2020, 8, 20); //store the day of the first date
int secondDay = day(2020, 8, 20); //store the day of the second date
int firstMonth = month(2020, 8, 20); //store the month of the first date
int secondMonth = month(2020, 8, 20); //store the month of the second date

if (firstDay == secondDay && firstMonth == secondMonth) { //check if the days and months are the same
    //do something
}
```

By using these functions and conditional statements, we can easily compare two dates in our Arduino projects.

## Deep Dive

When comparing dates, it is important to understand different date formats and the limitations of the Arduino board. The millis() function returns a value of type "unsigned long", which has a maximum value of 4,294,967,295. This means that the reference point we create using millis() will only work for a certain amount of time before it resets.

Additionally, the day(), month(), and year() functions only accept values from 2000 to 2049. This means that we cannot use these functions for dates outside of this range. If we need to compare dates from a wider range, we will need to use other techniques such as converting the dates to an integer value.

## See Also

- [Arduino Reference - millis() function](https://www.arduino.cc/reference/en/language/functions/time/millis/)
- [Arduino Reference - day() function](https://www.arduino.cc/reference/en/language/functions/time/day/)
- [Arduino Reference - month() function](https://www.arduino.cc/reference/en/language/functions/time/month/)
- [Arduino Reference - year() function](https://www.arduino.cc/reference/en/language/functions/time/year/)