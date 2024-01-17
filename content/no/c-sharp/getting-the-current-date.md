---
title:                "Få dagens dato"
html_title:           "C#: Få dagens dato"
simple_title:         "Få dagens dato"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å få tak i nåværende dato er en viktig funksjon i programmering. Det refererer til å få tak i dagens dato og klokkeslett, og det gjøres vanligvis ved å bruke koden "DateTime.Now". Dette er viktig i programmering for å kunne utføre forskjellige oppgaver basert på nåværende tid.

## Hvordan:

Å få tak i nåværende dato i C# er veldig enkelt. Alt du trenger å gjøre er å bruke koden "DateTime.Now" som nevnt tidligere. Dette vil returnere en DateTime-objekt som inneholder nåværende dato og klokkeslett. La oss se på et eksempel:

```C#
DateTime now = DateTime.Now;
Console.WriteLine("Dagens dato er: " + now.ToShortDateString());
Console.WriteLine("Klokken er nå: " + now.ToShortTimeString());
```

Output:
```
Dagens dato er: 13.04.2021
Klokken er nå: 12:30
```

Som du kan se, bruker vi "ToShortDateString()" og "ToShortTimeString()" for å få en enklere og mer lesbar output. Du kan også bruke andre formateringsfunksjoner for å endre utseendet på datoen og klokkeslettet.

## Dypdykk:

Å få tak i nåværende dato er ikke noe nytt i programmering. Det har eksistert i lang tid og er en viktig funksjon for å kunne håndtere tidrelaterte oppgaver. Det finnes også forskjellige alternativer for å få tak i nåværende dato, som for eksempel "DateTime.UtcNow", som returnerer nåværende dato og klokkeslett i UTC-format. Imidlertid er "DateTime.Now" den mest brukte metoden.

Implementeringen av "DateTime.Now" er avhengig av operativsystemet. I Windows vil den bruke systemklokken, mens i Linux vil den bruke systemklokken fra Linux Kernel. Det er viktig å merke seg dette for å få nøyaktige resultater.

## Se også:

- [Microsoft Docs - DateTime.Now Property](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.now)
- [GeeksforGeeks - C# | DateTime.Now Property](https://www.geeksforgeeks.org/c-sharp-datetime-now-property/)
- [C# - Datum akalender](https://www.c-sharpcorner.com/resource/date-and-calendar-in-c-sharp/)