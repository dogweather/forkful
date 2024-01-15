---
title:                "Converting a date into a string"
html_title:           "Java recipe: Converting a date into a string"
simple_title:         "Converting a date into a string"
programming_language: "Java"
category:             "Java"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Why
Have you ever needed to display a date in a particular format while working on a Java project? Maybe you wanted to print out the date in a specific way or store it as a string for future use. Converting a date into a string allows you to do just that, making it a useful skill for any Java programmer.

## How To
Converting a date into a string may seem like a daunting task, but with Java's built-in methods, it's quite simple. Let's take a look at an example:
```Java
import java.util.Date;
import java.text.SimpleDateFormat;

public class DateToStringExample {
  public static void main(String[] args) {
    // Create a Date object for today's date
    Date today = new Date();

    // Create a SimpleDateFormat object with desired date format
    SimpleDateFormat dateFormat = new SimpleDateFormat("MM/dd/yyyy");

    // Use the format method to convert date into a string
    String convertedDate = dateFormat.format(today);

    // Print the converted date
    System.out.println(convertedDate);
  }
}
```
This code will print out the current date in the format of "MM/dd/yyyy", such as 09/15/2020. You can change the format to your desired one by adjusting the pattern in the SimpleDateFormat constructor.

Another way to convert a date into a string is by using the toString method of the Date class. This will give you a more detailed string representation of the date, including the time and time zone.

## Deep Dive
Behind the scenes, the format method of the SimpleDateFormat class uses the format pattern to convert the date into a string. The pattern symbols represent different parts of the date, such as "MM" for the month, "dd" for the day, and "yyyy" for the year.

In the second example, the toString method uses the default format of the Date class, which displays the date and time in the format "EEE MMM dd HH:mm:ss zzz yyyy". This format is not as customizable as the SimpleDateFormat format, but it provides more information about the date.

## See Also
Here are some additional resources for converting a date into a string in Java:
- [Java Date and Time API](https://docs.oracle.com/javase/8/docs/api/java/time/package-summary.html)
- [Java SimpleDateFormat class](https://docs.oracle.com/javase/8/docs/api/java/text/SimpleDateFormat.html)
- [Java Date class](https://docs.oracle.com/javase/8/docs/api/java/util/Date.html)