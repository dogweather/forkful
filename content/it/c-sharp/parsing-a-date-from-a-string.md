---
title:                "Estrarre una data da una stringa"
date:                  2024-01-20T15:35:07.903254-07:00
html_title:           "Arduino: Estrarre una data da una stringa"
simple_title:         "Estrarre una data da una stringa"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Il parsing di una data da una stringa trasforma il testo in un oggetto `DateTime`, permettendoci di gestire le date con precisione. I programmatori lo fanno per manipolare e confrontare date, pianificare eventi, o lavorare con archivi e databases.

## How to:
Ecco come usare `DateTime.Parse` e `DateTime.TryParse` in C#:

```C#
using System;
using System.Globalization;

class DateParsingExample
{
    static void Main()
    {
        string dateString = "24/03/2023";
        
        // Uso della Parse
        DateTime parsedDate = DateTime.Parse(dateString, new CultureInfo("it-IT"));
        Console.WriteLine(parsedDate); // Output: 24/03/2023 00:00:00

        // Uso della TryParse
        DateTime tryParsedDate;
        if (DateTime.TryParse(dateString, new CultureInfo("it-IT"), DateTimeStyles.None, out tryParsedDate))
        {
            Console.WriteLine(tryParsedDate);  // Output: 24/03/2023 00:00:00
        }
        else
        {
            Console.WriteLine("Parsing non riuscito.");
        }
    }
}
```

## Deep Dive
Il parsing di date è essenziale sin dagli albori dell'informatica. In C#, il parsing supporta diversi formati grazie alle culture (`CultureInfo`). Metodi come `DateTime.ParseExact` o `DateTime.TryParseExact` permettono un controllo più rigoroso sui formati. È importante anche gestire eccezioni con `Parse`, mentre `TryParse` evita eccezioni restituendo un booleano.

Alternativamente, librerie esterne come NodaTime offrono maggior flessibilità per gestire date e orari.

## See Also
- [DateTime.Parse Method](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parse?view=netcore-3.1)
- [DateTime.TryParse Method](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tryparse?view=netcore-3.1)
- [Custom date and time format strings](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [NodaTime](https://nodatime.org/)
