---
title:                "Calculating a date in the future or past"
html_title:           "C# recipe: Calculating a date in the future or past"
simple_title:         "Calculating a date in the future or past"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Why
It's common for developers to need to calculate a date in the future or past for various reasons, such as scheduling events, setting reminders, or determining deadlines. In this article, we will explore how to easily accomplish this task using C#.

## How To

Calculating a future or past date in C# is a simple task that can be done using the built-in "DateTime" object and its methods. Let's take a look at some code examples to see how it works:

```
// Calculating a future date by adding days to today's date
DateTime futureDate = DateTime.Today.AddDays(10);
Console.WriteLine("The future date is: " + futureDate);

// Calculating a past date by subtracting days from today's date
DateTime pastDate = DateTime.Today.AddDays(-10);
Console.WriteLine("The past date is: " + pastDate);

// Specifying a specific date and adding months to it
DateTime specificDate = new DateTime(2021, 1, 1);
DateTime futureSpecific = specificDate.AddMonths(3);
Console.WriteLine("The future specific date is: " + futureSpecific);
```

The output of the above code will be:

```
The future date is: {Current Date} + 10 days
The past date is: {Current Date} - 10 days
The future specific date is: 2021-04-01 00:00:00
```

As you can see, by using the appropriate "DateTime" methods, we can easily calculate future or past dates according to our needs.

## Deep Dive

Behind the scenes, C# stores dates as the number of ticks (100 nanoseconds) that have passed since January 1, 0001 at 12:00:00 AM. This means that any calculations we do with dates will be based on this starting point.

When adding or subtracting values to a date, the "DateTime" methods use this starting point and perform the necessary calculations. This is why we can easily add or subtract days, months, years, or even time spans to dates, and get accurate results.

It's important to note that while the "DateTime" object is capable of handling dates from the years 0001 to 9999, it does have some limitations when using dates before the introduction of the Gregorian calendar in October 1582. In such cases, the "DateTime" object will use the Julian calendar, which may result in slightly different calculations for certain dates.

To avoid any potential issues, it's always recommended to check for any specific calendar requirements when working with dates that fall before the Gregorian calendar transition.

## See Also
- [DateTime Struct documentation](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
- [Date and Time Manipulation in C#](https://www.c-sharpcorner.com/blogs/date-and-time-manipulation-in-c-sharp1)
- [Working with dates and times in C#](https://www.dotnetperls.com/datetime)