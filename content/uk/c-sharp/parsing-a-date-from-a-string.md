---
title:                "Аналіз дати з рядка"
date:                  2024-01-20T15:35:16.481704-07:00
html_title:           "Arduino: Аналіз дати з рядка"
simple_title:         "Аналіз дати з рядка"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
## Що та Навіщо?

Parsing a date means converting a string into a DateTime object. Programmers do it to handle date information within a program, such as saving or comparing dates.

## How to:
## Як це зробити:

```C#
using System;
using System.Globalization;

class Program
{
    static void Main()
    {
        string dateString = "24-03-2023";
        string format = "dd-MM-yyyy";
        CultureInfo provider = CultureInfo.InvariantCulture;

        try
        {
            DateTime parsedDate = DateTime.ParseExact(dateString, format, provider);
            Console.WriteLine(parsedDate.ToString("dddd, dd MMMM yyyy"));
        }
        catch (FormatException)
        {
            Console.WriteLine($"{dateString} is not in the correct format.");
        }
    }
}
```

Sample Output:
```
Friday, 24 March 2023
```

## Deep Dive:
## Поглиблений Аналіз:

Historically, date parsing in C# has evolved. Previous versions relied on `DateTime.Parse` which works well but lacks precision. `DateTime.ParseExact` and `TryParseExact` methods provide more control by requiring a specific date format.

Alternatives include using DateTimeOffset for time zone-aware applications or third-party libraries like NodaTime for more complex scenarios.

Implementation-wise, it's crucial to use `CultureInfo`, as date formats vary worldwide. For example, the US uses "MM-dd-yyyy", while most of Europe prefers "dd-MM-yyyy". You have to use the right culture to avoid date interpretation errors.

## See Also:
## Дивіться Також:

- [Microsoft Docs - DateTime.ParseExact Method](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parseexact)
- [Microsoft Docs - Custom date and time format strings](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [NodaTime Documentation](https://nodatime.org/)