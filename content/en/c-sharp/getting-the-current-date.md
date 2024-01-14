---
title:                "C# recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

As a C# programmer, you may find yourself needing to know the current date for a variety of reasons. Whether it's for logging purposes, displaying the date on a user interface, or performing calculations based on the current date, having access to this information can be beneficial to your code.

## How To

Getting the current date in C# is a simple task and can be done using the built-in `DateTime` class. This class allows you to retrieve the current date and time, as well as perform various operations on it.

To begin, you'll need to add the `System` namespace to your code file. This can be done by adding the following line of code at the top of your file:

```C#
using System;
```

Next, you can create a new instance of the `DateTime` class and assign it to a variable. This can be done using the `Now` property, as shown below:

```C#
DateTime currentDate = DateTime.Now;
```

You can then use the various properties and methods of the `DateTime` class to retrieve specific information about the current date. For example, you can use the `Day`, `Month`, and `Year` properties to retrieve the day, month, and year of the current date, respectively.

```C#
int day = currentDate.Day;
int month = currentDate.Month;
int year = currentDate.Year;
```

You can also use the `ToString()` method to format the current date in a specific way. For example, you can use the "D" format specifier to get the long date format, which includes the day of the week, month, day, and year.

```C#
string longDate = currentDate.ToString("D");
```

The sample output for the above code would be:

```
Monday, February 8, 2021
```

You can explore the `DateTime` class further to discover more ways to retrieve and format the current date.

## Deep Dive

Behind the scenes, the `DateTime` class uses the system clock to retrieve the current date and time. This means that if the system clock is incorrect, the current date and time retrieved by the `DateTime` class will also be incorrect.

It's also worth noting that the current date and time retrieved by the `Now` property is based on the time zone of the computer running the code. If you need to get the current date and time in a specific time zone, you can use the `Now` property in conjunction with the `ToLocalTime()` or `ToUniversalTime()` methods.

## See Also

- Official documentation for the `DateTime` class: https://docs.microsoft.com/en-us/dotnet/api/system.datetime
- Tutorial on formatting dates and times in C#: https://www.c-sharpcorner.com/article/c-sharp-datetime-format/#:~:text=DateTime%20format%20is%20used%20to,10%3A08%3A41%20AM.
- Blog post on using time zones in C#: https://visualstudiomagazine.com/articles/2008/04/03/how-to-deal-with-date-and-time-in-c-part-2-tackling-time-zones.aspx