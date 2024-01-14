---
title:                "C#: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor
Det kan være mange grunner til hvorfor noen vil beregne en dato i fremtiden eller fortiden. Kanskje du planlegger en ferietur eller må organisere en viktig hendelse. Uansett årsak, kan det være nyttig å kunne beregne datoer i C# for å forenkle prosessen.

## Hvordan gjøre det
Det første trinnet er å importere `System`-biblioteket i C#, som inneholder funksjoner for å håndtere datoer og kalendere. Deretter kan du bruke funksjonen `AddDays()` til å legge til eller trekke fra et gitt antall dager fra en eksisterende dato.

```C#
using System;

DateTime nå = DateTime.Today; // Dagens dato
DateTime nyDato = nå.AddDays(14); // Legge til 14 dager til nå
Console.WriteLine(nyDato); // Skriver ut datoen 14 dager fra nå 
// Output: 02/08/2021
```

Du kan også bruke funksjonen `AddMonths()` og `AddYears()` for å legge til eller trekke fra måneder og år fra en dato. Det er også mulig å spesifisere et negativt tall for å beregne en dato i fortiden.

```C#
DateTime nå = DateTime.Today; // Dagens dato
DateTime nyDato = nå.AddYears(-2); // Trekke fra 2 år fra nå
Console.WriteLine(nyDato); // Skriver ut datoen fra 2 år siden
// Output: 29/01/2019
```

## Dykk ned i detaljene
Innenfor `System`-biblioteket finnes det flere funksjoner for å håndtere datoer og kalendere. Det er også mulig å spesifisere en bestemt dato og formatere den etter behov. Det er også viktig å merke seg at alle operasjoner blir utført basert på datoen og klokkeslettet som er satt på maskinen din.

## Se også
- [DateTime.AddYears metode i C# (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.addyears?view=net-5.0)
- [DateTime struct i C# (Microsoft Docs)](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-5.0)
- [Date and Time Functions i C# (W3Schools)](https://www.w3schools.com/cs/cs_ref_date_time.asp)