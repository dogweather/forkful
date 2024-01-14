---
title:    "Arduino recipe: Calculating a date in the future or past"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Why

Have you ever needed to calculate a date in the future or past for a project or task? Well, with the help of Arduino programming, you can easily achieve this task without having to manually calculate it. Saving you time and effort, using Arduino for date calculations can be a game changer. 

## How To

### Setting Up

Before we begin, make sure you have an Arduino board and the Arduino software downloaded. Once you have these, connect your board to your computer and open the Arduino software. 

### Code and Output

To calculate a date in the future or past, we will be using the `Time.h` library. This library allows us to work with dates and times in Arduino. Let's start by including the library in our code:

```Arduino
#include <Time.h>
```

Next, we will need to define a variable for the date we want to calculate. For this example, let's say we want to calculate 10 days from today's date. We will use the `makeTime()` function to do this. Here's what our code will look like:

```Arduino
tmElements_t date;
date = makeTime(0,0,0, day(), month(), year()+1);
```

In this code, we are setting the date variable to be 10 days from today's date by using the `day(), month()` and `year()` functions. The `0,0,0` represents the time which we don't need for this calculation. 

Now, let's print our calculated date to the serial monitor using the `setTime()` and `asctime()` functions:

```Arduino
setTime(date);
Serial.println(asctime(date));
```

Our output will look like this:

```
Fri May 29 00:00:00 2020
```

Congratulations! You have successfully calculated a date using Arduino programming.

## Deep Dive

If you want to calculate a date in the past, you can use the same code as above but change the `year()+1` to `year()-1` or whichever year you desire. In addition, the `day(), month() and year()` functions return the current date according to your computer's time, so make sure it is set correctly.

There are also other useful functions in the `Time.h` library such as `second(), minute(), hour(), day(), month() and year()` that allow you to retrieve specific date and time information. Play around with these functions to see what you can achieve.

## See Also

Here are some helpful links for further resources on using Arduino for date calculations:
- [Arduino Time Library Reference](https://www.arduino.cc/en/Reference/Time)
- [Calculating Time with Arduino](https://www.makerguides.com/time-arduino-ds3231-real-time-clock-module/#calculating-time)
- [Arduino date/time manipulation using strings](https://www.raspberrypi.org/forums/viewtopic.php?t=32760)

Now that you know how to calculate a date in the future or past using Arduino, you can save yourself time and effort in your projects. With the `Time.h` library, you can easily work with dates and times in your code. Happy coding!