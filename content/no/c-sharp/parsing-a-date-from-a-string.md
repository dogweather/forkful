---
title:                "Tolke en dato fra en streng"
date:                  2024-01-20T15:35:15.195961-07:00
html_title:           "Arduino: Tolke en dato fra en streng"
simple_title:         "Tolke en dato fra en streng"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## What & Why?
Parsing en dato fra en streng er å konvertere tekst til et `DateTime`-objekt. Vi gjør det for å jobbe med datoer på et standardformat og manipulere tiden i applikasjonene våre.

## How to:
For å parse en dato fra en streng i C#, bruk `DateTime.Parse()` eller `DateTime.TryParse()` for mer kontroll. Her er et eksempel:

```C#
using System;
using System.Globalization;

class Program
{
    static void Main()
    {
        // Parse med forventet format
        string dateString = "23.04.2023"; // Norsk datoformat
        string format = "dd.MM.yyyy";
        CultureInfo provider = CultureInfo.InvariantCulture;

        try
        {
            DateTime parsedDate = DateTime.ParseExact(dateString, format, provider);
            Console.WriteLine(parsedDate.ToString()); // Ut: 23.04.2023 00:00:00
        }
        catch (FormatException)
        {
            Console.WriteLine($"Ugyldig datoformat: {dateString}");
        }

        // TryParse for sikker parsing uten å kaste unntak
        DateTime tryParsedDate;
        bool isSuccess = DateTime.TryParseExact(dateString, format, provider, DateTimeStyles.None, out tryParsedDate);

        if (isSuccess)
        {
            Console.WriteLine(tryParsedDate.ToString()); // Ut: 23.04.2023 00:00:00
        }
        else
        {
            Console.WriteLine("Parsing feilet.");
        }
    }
}
```

## Deep Dive
Tidligere, brukte C#-utviklere ofte `Convert.ToDateTime()`. Men dette gav mindre kontroll og førte til flere feil. Med `DateTime.ParseExact()` og `DateTime.TryParseExact()`, angir du formatet eksplisitt, noe som reduserer risikoen for misforståelser.

Alternativer? `DateTimeOffset` er nyttig når tidssone er viktig. `NodaTime`-biblioteket er en tredjeparts løsning for mer komplekse tidsoperasjoner.

Implementeringsdetaljer? Å parse datoer krever forståelse for `CultureInfo`. For eksempel, om du parser en dato fra en streng med amerikansk format, men appen kjører på en norsk lokalisering, vil det oppstå feil. Derfor brukes `CultureInfo.InvariantCulture` i vårt tilfelle.

## See Also
- [DateTime.ParseExact Method](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parseexact)
- [CultureInfo Class](https://docs.microsoft.com/en-us/dotnet/api/system.globalization.cultureinfo)
- [NodaTime](https://nodatime.org/)
- [DateTime vs DateTimeOffset](https://docs.microsoft.com/en-us/dotnet/standard/datetime/choosing-between-datetime)