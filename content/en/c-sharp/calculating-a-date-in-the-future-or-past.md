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

## What & Why?

Computing a future or past date involves date arithmetic: additions, subtractions, etc. This skill is essential for developers because dates and time represent a substantial component of real-world applications, from setting reminders to tracking project milestones.

## How to:

Here's how to manipulate dates in C#:

```C#
using System;

var baseDate = DateTime.Now;

var futureDate = baseDate.AddDays(5);
Console.WriteLine($"Future date is: {futureDate}");

var pastDate = baseDate.AddDays(-3);
Console.WriteLine($"Past date is: {pastDate}");
```

Output:

```C#
Future date is: 09/28/2022 12:00:00 PM
Past date is: 09/20/2022 12:00:00 PM
```

## Deep Dive

Historically, date manipulation was complex, but it's now greatly simplified by robust built-in methods in C#. The `DateTime` structure provides several methods for adding and subtracting time intervals.

Alternatives include using `TimeSpan`, a structure that represents a time interval, or DateTimeOffset for dealing with UTC and local times. The `NodaTime` library can also be a good alternative for complex date/time calculations.

The methods `AddYears`, `AddMonths`, `AddDays`, `AddHours`, `AddMinutes`, `AddSeconds`, and `AddMilliseconds` allow you to manipulate `DateTime` objects. The subtract operation can be performed by adding a negative value.

Note that `DateTime` is immutable. Every time you use an `Add` method, it returns a new `DateTime` instance without changing the original.

## See Also

- Official Documentation: [`DateTime`](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0)
- C# Programming Guide: [Dates, times, and time zones](https://docs.microsoft.com/dotnet/csharp/programming-guide/concepts/datetime/)
- NodaTime: [NodaTime library](https://nodatime.org/)
- StackOverflow: [Date and Time Mathematics](https://stackoverflow.com/questions/tagged/datetime+arithmetic)