---
title:                "Att hämta aktuellt datum"
date:                  2024-01-20T15:13:29.290723-07:00
html_title:           "Bash: Att hämta aktuellt datum"
simple_title:         "Att hämta aktuellt datum"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why?
Att hämta det aktuella datumet i C# är grundläggande. Det låter oss spåra händelser, tidstämpla data och hantera schemalagda uppgifter. Man använder det för allt från loggfilers tidstämplar till att utlösa dagliga jobb.

## How to:
För att få tag på dagens datum använder vi `DateTime`. Här är hur det ser ut:

```C#
using System;

class Program
{
    static void Main()
    {
        DateTime currentDate = DateTime.Now;
        Console.WriteLine(currentDate.ToString("yyyy-MM-dd"));
    }
}
```

Och så här ser resultatet ut:
```
2023-03-15
```

Men om du bara vill ha datumet utan tid, använd `DateTime.Today` så här:

```C#
using System;

class Program
{
    static void Main()
    {
        DateTime currentDate = DateTime.Today;
        Console.WriteLine(currentDate.ToString("yyyy-MM-dd"));
    }
}
```

Resultat:
```
2023-03-15
```

## Deep Dive
`DateTime.Now` och `DateTime.Today` är byggstenar i .NET. `DateTime.Now` ger oss nuvarande datum och tid, medan `DateTime.Today` ger oss datumet med tiden satt till midnatt.

Före .NET fanns det systemanrop på varje operativsystem för att hämta datum och tid. Nu ger .NET oss en enhetlig gränssnitt över plattformar.

Om du behöver hantera tidszoner, då finns `DateTimeOffset`. För prestandakritiska behov, titta på `Stopwatch`.

## See Also
- Microsofts datum och tid guide: [DateTime Struktur](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=netframework-4.8)
- För djupare förståelse kring tidzoner: [DateTimeOffset Struktur](https://docs.microsoft.com/en-us/dotnet/api/system.datetimeoffset?view=net-6.0)
- `DateTime` vs `DateTimeOffset` jämförelse: [Välja mellan DateTime, DateTimeOffset, TimeSpan, and TimeZoneInfo](https://docs.microsoft.com/en-us/dotnet/standard/datetime/choosing-between-datetime)
- För precision med tid: [Stopwatch Klass](https://docs.microsoft.com/en-us/dotnet/api/system.diagnostics.stopwatch?view=net-6.0)