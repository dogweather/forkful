---
date: 2024-01-20 17:32:34.069538-07:00
description: 'How to: .'
lastmod: '2024-03-13T22:44:40.809479-06:00'
model: gpt-4-1106-preview
summary: .
title: Sammenlikning av to datoer
weight: 27
---

## How to:
```C#
using System;

class DatesComparison {
    static void Main() {
        DateTime date1 = new DateTime(2023, 03, 15);
        DateTime date2 = new DateTime(2023, 04, 20);
        
        int comparison = DateTime.Compare(date1, date2);
        
        if (comparison < 0)
            Console.WriteLine("date1 er før date2.");
        else if (comparison == 0)
            Console.WriteLine("date1 er samme som date2.");
        else
            Console.WriteLine("date1 er etter date2.");
    }
}
```
Output:
```
date1 er før date2.
```

## Deep Dive
Comparing dates in C# has been around since the .NET Framework's inception, intrinsic to the `DateTime` structure. Alternatives include `TimeSpan` for duration calculations, and `DateTimeOffset` for timezone-aware comparisons. Using `DateTime.Compare()` is straightforward; it returns an integer signifying the relationship. Internally, dates are represented as ticks (100-nanosecond intervals since 12:00 midnight, January 1, 0001) enabling precise comparisons and calculations.

## See Also
- Microsoft documentation on `DateTime`: [DateTime Struct (System)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime)
- Microsoft documentation on `DateTime.Compare()`: [DateTime.Compare Method (System)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.compare)
- For timezone considerations: [DateTimeOffset Struct (System)](https://docs.microsoft.com/en-us/dotnet/api/system.datetimeoffset)
