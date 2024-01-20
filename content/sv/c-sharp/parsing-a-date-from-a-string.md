---
title:                "Tolka ett datum från en sträng"
date:                  2024-01-20T15:35:14.707595-07:00
html_title:           "Bash: Tolka ett datum från en sträng"
simple_title:         "Tolka ett datum från en sträng"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Att tolka ett datum från en sträng betyder att du konverterar text till ett `DateTime` objekt i C#. Varför? För att vi ofta får datum i textform från användare eller filer och behöver jobba med dem i kod.

## How to:
Låt oss dyka rätt in:

```C#
using System;
using System.Globalization;

class DateParsingExample
{
    static void Main()
    {
        var dateString = "2023-04-05";
        var format = "yyyy-MM-dd";
        var culture = CultureInfo.InvariantCulture;

        if(DateTime.TryParseExact(dateString, format, culture, DateTimeStyles.None, out DateTime parsedDate))
        {
            Console.WriteLine($"Parsed Date: {parsedDate}");
        }
        else
        {
            Console.WriteLine("Could not parse the date!");
        }
    }
}
```

Kör koden. Om allt gått bra ser du:
```
Parsed Date: 2023-04-05 00:00:00
```

## Deep Dive
Historiskt sett har datumparsering alltid varit en utmaning. Diverse datumformat och lokala skillnader skapar huvudbry. C# löser detta genom `DateTime` strukturen och kulturellt medvetna metoder som `TryParseExact`. 

Alternativ till `DateTime` inkluderar `DateTimeOffset` för tidszoner och `CultureInfo` för särskilda lokala inställningar. För äldre C# versioner var parsing oftare knuten till egendefinerade metoder och mindre flexibel.

Implementationen av datumparsering i C# använder `IFormatProvider`, som `CultureInfo`, för att tolka strängen enligt rätt kulturella konventioner. Det hjälper med att tyda allt från dagar, månader och år, till timmar, minuter och sekunder på ett korrekt sätt.

## See Also
- [DateTime.TryParseExact Method](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.tryparseexact)
- [Custom date and time format strings](https://docs.microsoft.com/en-us/dotnet/standard/base-types/custom-date-and-time-format-strings)
- [CultureInfo Class](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo)