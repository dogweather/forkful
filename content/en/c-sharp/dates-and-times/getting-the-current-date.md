---
date: 2024-02-03 19:02:38.771485-07:00
description: "How to: C# provides a straightforward way to get the current date using\
  \ the `DateTime` class which is part of the .NET Framework's System namespace. The\u2026"
lastmod: '2024-03-13T22:45:00.098691-06:00'
model: gpt-4-0125-preview
summary: C# provides a straightforward way to get the current date using the `DateTime`
  class which is part of the .NET Framework's System namespace.
title: Getting the current date
weight: 29
---

## How to:
C# provides a straightforward way to get the current date using the `DateTime` class which is part of the .NET Framework's System namespace. The example below demonstrates how to get the current date, and optionally, the time.

```csharp
using System;

class Program
{
    static void Main()
    {
        // Gets the current date only
        DateTime currentDate = DateTime.Today;
        Console.WriteLine(currentDate.ToString("d"));  // Output: MM/dd/yyyy
        
        // Gets the current date and time
        DateTime currentDateTime = DateTime.Now;
        Console.WriteLine(currentDateTime.ToString()); // Output: MM/dd/yyyy HH:mm:ss

        // Gets the current UTC date and time
        DateTime currentUtcDateTime = DateTime.UtcNow;
        Console.WriteLine(currentUtcDateTime.ToString()); // Output: MM/dd/yyyy HH:mm:ss
    }
}
```

In terms of third-party libraries, NodaTime offers a robust alternative for date and time manipulation, including fetching the current date in different calendars and time zones.

```csharp
using NodaTime;
using System;

class Program
{
    static void Main()
    {
        // Using NodaTime to get the current date in the ISO calendar
        LocalDate currentDate = SystemClock.Instance.GetCurrentInstant().InUtc().Date;
        Console.WriteLine(currentDate.ToString()); // Output: yyyy-MM-dd

        // For timezone-specific dates
        DateTimeZone zone = DateTimeZoneProviders.Tzdb["America/New_York"];
        LocalDate currentZonedDate = SystemClock.Instance.GetCurrentInstant().InZone(zone).Date;
        Console.WriteLine(currentZonedDate.ToString()); // Output: yyyy-MM-dd
    }
}
```

This showcases the basic usage with the built-in `DateTime` class and the enhanced capabilities provided by NodaTime, especially useful for applications that require handling of different time zones or calendar systems.
