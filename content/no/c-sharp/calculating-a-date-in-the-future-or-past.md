---
title:                "Beregning av dato i fremtiden eller fortiden"
html_title:           "C#: Beregning av dato i fremtiden eller fortiden"
simple_title:         "Beregning av dato i fremtiden eller fortiden"
programming_language: "C#"
category:             "C#"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Hva og hvorfor?
Å beregne en dato i fremtiden eller fortiden er en vanlig oppgave for programmerere. Det lar oss programmere en hendelse basert på et bestemt tidsintervall, for eksempel å planlegge et møte eller sende en påminnelse. Ved å beregne datoer kan vi automatisere mange oppgaver og gjøre livet enklere for oss selv.

# Hvordan gjør man det?
I C# kan du bruke DateTime klassen og dens metoder for å beregne datoer. La oss si at vi ønsker å beregne datoen 7 dager fra nå. Da kan vi bruke AddDays() metoden og skrive følgende:

```C#
DateTime today = DateTime.Today;
DateTime futureDate = today.AddDays(7);
Console.WriteLine(futureDate);
```
Dette vil gi oss utdatoen, som i dette tilfellet er 7 dager frem i tid.

# Dykk dypere
Denne tilnærmingen til å beregne datoer er inspirert av ISO-kalenderen, som er en internasjonal standard for å representere dato og tid. Det finnes også andre måter å beregne datoer på, som for eksempel ved hjelp av TimeSpan klassen.

En annen ting å merke seg er at DateTime klassen har begrensninger når det kommer til datoer før år 1 e.Kr. og etter år 9999. Hvis du trenger å håndtere disse datoene, kan du bruke DateTimeOffset klassen.

# Se også
- [Microsoft sin dokumentasjon om DateTime klassen](https://docs.microsoft.com/en-us/dotnet/api/system.datetime?view=netcore-3.1)
- [Wikipedia om ISO-kalenderen](https://en.wikipedia.org/wiki/ISO_8601)
- [The C# Programming Language (4th Edition) by Anders Hejlsberg](https://www.pearson.com/us/higher-education/program/Stutzman-C-The-Programming-Language-4th-Edition/PGM724256.html)