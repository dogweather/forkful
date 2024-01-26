---
title:                "Обчислення дати у майбутньому чи минулому"
date:                  2024-01-20T17:28:30.640954-07:00
model:                 gpt-4-1106-preview
html_title:           "C#: Обчислення дати у майбутньому чи минулому"
simple_title:         "Обчислення дати у майбутньому чи минулому"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## What & Why? (Що і Чому?)
Calculating a future or past date is manipulating a current date to get another date. Programmers do it to manage events, deadlines, or schedules.

## How to: (Як це зробити:)
C# makes date manipulation straightforward. Use `DateTime` and `TimeSpan`. Here’s how:

```C#
using System;

class Program
{
    static void Main()
    {
        DateTime today = DateTime.Now;
        
        // Future date - 30 days from today
        DateTime futureDate = today.AddDays(30);
        Console.WriteLine("Future Date: " + futureDate.ToString("dd/MM/yyyy"));
        
        // Past date - 60 days ago
        DateTime pastDate = today.AddDays(-60);
        Console.WriteLine("Past Date: " + pastDate.ToString("dd/MM/yyyy"));
    }
}
```

Sample output:
```
Future Date: 30/04/2023 // this will be +30 days from the current date
Past Date: 30/01/2023 // this will be -60 days from the current date
```

## Deep Dive (Глибоке Занурення):
In the past, date manipulation was tied to complex calendar systems. C# eases this with `DateTime` and `TimeSpan`. But remember, time zones and leap years can complicate things. 

Alternatives? `NodaTime` library is robust for complex scenarios. `AddDays` is good for simple cases. You can also manipulate hours, minutes, and seconds with `AddHours`, `AddMinutes`, and `AddSeconds`.

Implementation details? `DateTime` is part of the .NET base class library — accurate, but not timezone-aware. For that, use `DateTimeOffset`.

## See Also (Дивіться також):
- [DateTime Struct (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime)
- [TimeSpan Struct (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.timespan)
- [NodaTime](https://nodatime.org/)
