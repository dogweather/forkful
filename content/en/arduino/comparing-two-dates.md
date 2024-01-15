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

## Why

Do you need to know whether one date is before, after, or the same as another? Look no further! With a few simple lines of code, you can easily compare two dates in your Arduino project.

## How To

To compare two dates in Arduino, we will use the built-in `time` library.

1. First, declare two variables for your dates. These can be in any format, as long as they are the same for both dates.

    ```Arduino
    int date1 = 20210523;
    int date2 = 20210602;
    ```

2. Next, create two `tmElements_t` structs and initialize them with your dates. This will allow us to use the `time` library's built-in functions for date comparison.

    ```Arduino
    tmElements_t tm1 = {0, 0, 0, 0, 0, 0, 0};
    tm1.Year = date1 / 10000; // extract year from date1
    tm1.Month = (date1 % 10000) / 100; // extract month from date1
    tm1.Day = date1 % 100; // extract day from date1
    
    tmElements_t tm2 = {0, 0, 0, 0, 0, 0, 0};
    tm2.Year = date2 / 10000; // extract year from date2
    tm2.Month = (date2 % 10000) / 100; // extract month from date2
    tm2.Day = date2 % 100; // extract day from date2
    ```

3. Now, we can compare the two dates using the `makeTime()` and `timeDiff()` functions. `makeTime()` converts our `tmElements_t` structs into `time_t` values, which can then be used by `timeDiff()` to calculate the difference between the two dates.

    ```Arduino
    time_t time1 = makeTime(tm1); // convert tm1 to a time_t value
    time_t time2 = makeTime(tm2); // convert tm2 to a time_t value
    time_t diff = timeDiff(time1, time2); // calculate the difference between time1 and time2
    ```

4. Finally, we can use the `diff` variable to determine if one date is before, after, or the same as the other.

    ```Arduino
    if (diff > 0) {
        Serial.println("date1 is after date2");
    }
    else if (diff < 0) {
        Serial.println("date1 is before date2");
    }
    else {
        Serial.println("date1 is the same as date2");
    }
    ```

5. Upload the code to your Arduino and open the Serial Monitor to see the output.

    ```
    date1 is before date2
    ```

## Deep Dive

The `makeTime()` function is used to convert a `tmElements_t` struct into a `time_t` value, which is the number of seconds since January 1, 1970. This is known as the Unix timestamp and is used to easily compare dates and times.

The `timeDiff()` function calculates the difference between two `time_t` values, which can then be used to determine which date is before, after, or the same as the other.

You can also use the `dayOfTheWeek()` function to determine the day of the week for a given date. This function returns an integer from 0-6, with 0 representing Sunday.

For more advanced date and time operations, you can check out the `TimeLib` library, which includes functions for adding and subtracting dates, handling leap years, and more.

## See Also

- [Arduino Time Library Reference](https://www.arduino.cc/reference/en/libraries/time/)
- [TimeLib Library Reference](https://github.com/PaulStoffregen/Time/blob/master/README.md)
- [Unix Time Wikipedia Page](https://en.wikipedia.org/wiki/Unix_time)