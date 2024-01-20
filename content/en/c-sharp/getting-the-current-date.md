---
title:                "Getting the current date"
html_title:           "Elm recipe: Getting the current date"
simple_title:         "Getting the current date"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Getting the Current Date in C#: A Pragmatic Guide

## What & Why?

In C#, getting the current date is as simple as calling a built-in function. We do this to timestamp data, monitor events, or schedule tasks.

## How To:

You can do this by using the DateTime.Now property:

```C#
DateTime currentDate = DateTime.Now;
Console.WriteLine(currentDate);
```

When you run the code, you will get something that resembles this:

```Output
2022-02-07 23:59:59
```
That's it! You've just printed the current date and time.

## Deep Dive

Now a bit of a deep dive. The `DateTime.Now` property in C# has a history that goes way back to the early days of .NET framework. Prior to .NET 2.0, if you needed precise timing, you were out of luck.

In terms of alternatives, you've also got `DateTime.UtcNow` if you want the current date and time in coordinated universal time (UTC), without any time zone offset.

When you're working across timezones, consider using DateTimeOffset. Its Now property includes the timezone offset in relation to UTC, helping mitigate timing shifts across geographical locations.

Here's how:
```C#
DateTimeOffset currentDateTimeOffset = DateTimeOffset.Now;
Console.WriteLine(currentDateTimeOffset);
```

Deep down, when you call `DateTime.Now`, it's not just fetching the current system time. It's also converting it into the local timezone by reading the system's timezone settings.

## See Also

For more info on the DateTime.Now property, you can check it out in [Microsoft's official documentation](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.now?view=net-6.0).

To delve into the complexities of timezones, dates, and times in .NET, Jon Skeet's [Noda Time](https://nodatime.org/) is a great asset. Noda Time is an alternative date and time API for .NET, which is more powerful and flexible than the built-in DateTime struct.