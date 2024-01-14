---
title:                "C# recipe: Getting the current date"
programming_language: "C#"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

In any programming language, manipulating dates and times is a common task. Whether it's for displaying current data, performing date calculations, or tracking time-sensitive events, being able to get the current date is an essential skill for any coder. In this blog post, we will explore how to retrieve the current date in C# and dive into some underlying concepts.

## How To

In C#, there are several ways to get the current date. One simple method is to use the `DateTime` class, which has a built-in `Now` property that returns the current date and time. Let's take a look at an example:

```C#
DateTime currentDateTime = DateTime.Now;
Console.WriteLine(currentDateTime);
```

Output: `5/17/2021 2:45:23 PM`

As you can see, the above snippet uses the `Console.WriteLine()` method to display the current date and time on the console. We can also format the output to display only the date or time using the `ToString()` method and a custom format string. For example:

```C#
DateTime currentDate = DateTime.Now;
Console.WriteLine(currentDate.ToString("d"));
```

Output: `5/17/2021`

In this example, the "d" format string displays only the short date format without the time. Similarly, we can use a custom format string to display only the time:

```C#
DateTime currentTime = DateTime.Now;
Console.WriteLine(currentTime.ToString("t"));
```

Output: `2:45 PM`

There are also other useful properties and methods in the `DateTime` class for manipulating dates, such as `DayOfWeek`, `AddDays()`, and `Compare()`. To learn more about them, I recommend checking out the official documentation (see "See Also" section below).

## Deep Dive

Under the hood, the `DateTime.Now` property retrieves the current date and time based on the system's clock. This means that if the system clock is changed, the output will also be affected. Additionally, the current date and time are represented internally as a number of ticks - the number of 100-nanosecond intervals that have elapsed since January 1, 0001 at 12:00:00 midnight. This explains why the output is displayed in a specific format and why we need to use the `ToString()` method to format it as desired.

Another important concept to keep in mind is time zones. The `DateTime.Now` property will return the current date and time based on the local time zone of the system. If you want to get the current date and time in a specific time zone, you can use the `DateTime.UtcNow` property and then use the `ToLocalTime()` method to convert it to the desired time zone.

## See Also

- [DateTime.Now Property (DateTime) - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.now)
- [DateTime Structure - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/api/system.datetime)
- [Custom Date and Time Format Strings - Microsoft Docs](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)