---
title:                "Beregning av en dato i fremtiden eller fortiden"
html_title:           "C#: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor

Å beregne en dato i fremtiden eller fortiden kan være nyttig når du for eksempel planlegger en begivenhet eller trenger å vite hva datoen var for en spesifikk hendelse.

## Hvordan

```C#
// Importer nødvendige biblioteker
using System;
using System.Globalization;

// Definer datoen du vil utgangspunktet
DateTime utgangspunkt = new DateTime(2021, 5, 10);

// Beregne en dato i fremtiden ved å legge til dager/måneder/år
DateTime fremtidig = utgangspunkt.AddDays(10).AddMonths(2).AddYears(1);

// Beregne en dato i fortiden ved å trekke fra dager/måneder/år
DateTime fortidig = utgangspunkt.AddDays(-10).AddMonths(-2).AddYears(-1);

// Skriv ut resultatet
Console.WriteLine($"Datoen i fremtiden: {fremtidig.ToString("dd.MM.yyyy", CultureInfo.InvariantCulture)}");
Console.WriteLine($"Datoen i fortiden: {fortidig.ToString("dd.MM.yyyy", CultureInfo.InvariantCulture)}");
```

Utskrift:
```
Datoen i fremtiden: 20.07.2022
Datoen i fortiden: 30.03.2020
```

## Dypdykk

Det er flere tilgjengelige metoder for å beregne en dato i fremtiden eller fortiden i C#. For eksempel kan du bruke `Add`-funksjonen og angi hvor mange dager, måneder eller år du vil legge til eller trekke fra. Du kan også bruke `AddDays`, `AddMonths` eller `AddYears` separat. I tillegg kan du bruke `DateTime`-strukturen til å utføre ulike operasjoner på datoer.

## Se også

- [DateTime Struktur (C# Programmeringsguide)](https://docs.microsoft.com/nb-no/dotnet/csharp/programming-guide/datetime/)
- [DateTime.Add Metode (System)](https://docs.microsoft.com/nb-no/dotnet/api/system.datetime.add)
- [CultureInfo Klassen (System.Globalization)](https://docs.microsoft.com/nb-no/dotnet/api/system.globalization.cultureinfo)