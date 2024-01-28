---
title:                "Loggføring"
date:                  2024-01-26T01:00:41.547277-07:00
model:                 gpt-4-1106-preview
simple_title:         "Loggføring"
programming_language: "C#"
category:             "C#"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c-sharp/logging.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Logging er prosessen med å registrere applikasjonshendelser og datautskrift under kjøretid. Programmerere logger for å diagnostisere feil, overvåke programvarens ytelse, spore brukerhandlinger og opprettholde samsvar med sikkerhets- og forretningsstandarder.

## Hvordan:
I C# kan du bruke det innebygde `System.Diagnostics` navneområdet eller tredjepartsbiblioteker som NLog eller log4net. Her er et raskt eksempel som bruker `ILogger` grensesnittet tilgjengelig i .NET Core:

```C#
using Microsoft.Extensions.Logging;
using System;

public class Program
{
    public static void Main()
    {
        using var loggerFactory = LoggerFactory.Create(builder => {
            builder.AddConsole();
        });

        ILogger logger = loggerFactory.CreateLogger<Program>();

        logger.LogInformation("Dette er en informasjonsmelding.");
        logger.LogWarning("Dette er en advarselmelding.");
        logger.LogError("Dette er en feilmelding.");
    }
}
```

Eksempel på utskrift:
```
info: Program[0]
      Dette er en informasjonsmelding.
warn: Program[0]
      Dette er en advarselmelding.
fail: Program[0]
      Dette er en feilmelding.
```

## Dypdykk
Historien om logging i programvareutvikling er nesten like gammel som programmering selv; det har utviklet seg fra enkle utskriftssetninger til sofistikerte, konfigurerbare systemer. Opprinnelig ble logging gjort ved å skrive til filer eller konsollen, men dette har vokst til å inkludere mer komplekse strukturer som loggaggregeringssystemer og distribuerte sporingssystemer (som ELK-stacken eller Jaeger).

Alternativer til den innebygde loggingen i .NET inkluderer tredjepartsbiblioteker:
- **NLog**: fleksibelt og enkelt å sette opp, med mange funksjoner for ruting, formatering og filtrering av logger.
- **log4net**: inspirert av Java-biblioteket log4j, det er svært konfigurerbart fra XML og støtter en rekke loggerepositorier.

Når det kommer til implementeringsdetaljer, kan valget av din loggingabstraksjon (som Microsoft.Extensions.Logging) og den underliggende loggingleverandøren betydelig påvirke applikasjonens ytelse og pålitelighet. Det er avgjørende å konfigurere loggingnivåene riktig og sørge for at skriving av logger ikke blir en flaskehals.

Også strukturert logging - hvor du logger ikke bare strenger men nøkkel-verdipar eller objekter - muliggjør mer presise og handlingsdyktige logger, som er enklere å spørre og analysere.

## Se også
- [Microsoft.Extensions.Logging dokumentasjon](https://docs.microsoft.com/en-us/aspnet/core/fundamentals/logging/)
- [NLog-dokumentasjon](https://nlog-project.org/documentation/)
- [log4net-dokumentasjon](https://logging.apache.org/log4net/)
- [Serilog-dokumentasjon](https://serilog.net/) (for et eksempel på strukturert logging)
