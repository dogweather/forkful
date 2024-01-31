---
title:                "Slik får du tak i dagens dato"
date:                  2024-01-20T15:13:30.476156-07:00
html_title:           "C: Slik får du tak i dagens dato"
simple_title:         "Slik får du tak i dagens dato"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
I koding brukes det ofte til å hente ut dagens dato for å ha tidsreferanser, logge hendelser eller håndtere dato-sensitive operasjoner. Det er en standard funksjon – enkel, men viktig.

## Hvordan:
```
using System;

class Program {
    static void Main() {
        DateTime today = DateTime.Now; // Får dagens dato og klokkeslett
        Console.WriteLine(today.ToString("d")); // Utskriftsformat: 24/03/2023
    }
}
```
Output:
```
24/03/2023
```

## Dypdykk:
`DateTime.Now` er en del av .NET rammeverkets `System` navneområdet og har vært der siden de første versjonene. Alternativer inkluderer `DateTime.UtcNow` for koordinert universaltid (UTC), som unngår tidssonekomplikasjoner. Detaljer i implementasjonen omfatter tidssonehåndtering og internnanvendelse av `DateTimeKind`-egenskapen for å skille mellom ulike typer tidsdata.

## Se Også:
- Microsoft dokumentasjon om `DateTime` klasse: https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=net-6.0
- Tidssonebehandling i .NET: https://docs.microsoft.com/en-us/dotnet/standard/datetime/choosing-between-datetime
- Artikkel om datohåndteringsmønstre i C#: https://www.codeproject.com/Articles/144159/DateTime-Management-in-C-
