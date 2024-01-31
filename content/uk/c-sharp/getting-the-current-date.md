---
title:                "Отримання поточної дати"
date:                  2024-01-20T15:13:46.337863-07:00
html_title:           "Bash: Отримання поточної дати"
simple_title:         "Отримання поточної дати"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Що та навіщо?

Getting the current date means accessing the system's clock to find out today's date. Programmers use it for logging, timestamping events, or displaying dates in applications.

## How to:
Як це робити:

Get the current date using `DateTime.Now`:

```C#
using System;

class Program
{
    static void Main()
    {
        DateTime currentDate = DateTime.Now;
        Console.WriteLine(currentDate.ToString("d")); // виводить у форматі MM/dd/yyyy
    }
}
```
Sample output if run on April 1, 2023:

```
04/01/2023
```

Use `DateTime.UtcNow` for a timezone-independent Universal Time Coordinate (UTC):

```C#
using System;

class Program
{
    static void Main()
    {
        DateTime currentDateUtc = DateTime.UtcNow;
        Console.WriteLine(currentDateUtc.ToString("d")); // виводить у форматі MM/dd/yyyy
    }
}
```

## Deep Dive
Поглиблений огляд:

The `System.DateTime` type has been around since .NET Framework 1.0, reflecting the computer's system clock. Alternatives like `DateTimeOffset` contain time zone info for more precision. For .NET 6 and later, consider using `DateOnly` if you need just the date with no time.  

Historically, `DateTime` could be problematic across time zones, leading to the introduction of `DateTimeOffset` in .NET Framework 2.0. 

For calendar calculations, customize `CultureInfo` if you need support for non-Gregorian calendars. 

Implementation-wise, the system's clock can be influenced by time synchronization services and might not be monotonic.

## See Also
Дивіться також:

- Microsoft Docs on `DateTime`: [https://docs.microsoft.com/en-us/dotnet/api/system.datetime](https://docs.microsoft.com/en-us/dotnet/api/system.datetime)
- Microsoft Docs on `DateTimeOffset`: [https://docs.microsoft.com/en-us/dotnet/api/system.datetimeoffset](https://docs.microsoft.com/en-us/dotnet/api/system.datetimeoffset)
- A comparison of different DateTime representations in .NET: [https://docs.microsoft.com/en-us/dotnet/standard/datetime/choosing-between-datetime](https://docs.microsoft.com/en-us/dotnet/standard/datetime/choosing-between-datetime)

Remember to check the TimeZone part of the documentation if you're working in a global context. Keep it precise and happy coding!
