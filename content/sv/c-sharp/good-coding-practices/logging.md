---
title:                "Loggning"
aliases: - /sv/c-sharp/logging.md
date:                  2024-01-26T01:01:18.530477-07:00
model:                 gpt-4-1106-preview
simple_title:         "Loggning"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/c-sharp/logging.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Loggning är processen att registrera applikationshändelser och datautskrift under körning. Programmerare loggar för att diagnostisera buggar, övervaka programvarans prestanda, spåra användaraktiviteter och upprätthålla efterlevnad av säkerhets- och affärsstandarder.

## Hur man gör:
I C# kan du använda det inbyggda namnrymdet `System.Diagnostics` eller tredjepartskomponenter som NLog eller log4net. Här är ett snabbt exempel som använder `ILogger`-gränssnittet tillgängligt i .NET Core:

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

        logger.LogInformation("Det här är ett informationmeddelande.");
        logger.LogWarning("Det här är ett varningsmeddelande.");
        logger.LogError("Det här är ett felmeddelande.");
    }
}
```

Exempel på utskrift:
```
info: Program[0]
      Det här är ett informationmeddelande.
warn: Program[0]
      Det här är ett varningsmeddelande.
fail: Program[0]
      Det här är ett felmeddelande.
```

## Fördjupning
Historien om loggning i mjukvaruutvecklingen är nästan lika gammal som programmeringen själv; den har utvecklats från enkla utskriftsdeklarationer till sofistikerade, konfigurerbara system. Ursprungligen utfördes loggning genom att skriva till filer eller konsolen, men detta har utökats till att inkludera mer komplexa strukturer såsom loggaggregeringssystem och plattformar för distribuerad spårning (som ELK-stacken eller Jaeger).

Alternativ till den inbyggda loggningen i .NET inkluderar tredjepartskomponenter:
- **NLog**: mångsidig och enkel att ställa in, med massor av funktioner för ruttning, formatering och filtrering av loggar.
- **log4net**: inspirerad av Java-biblioteket log4j, det är mycket konfigurerbart från XML och stödjer en mängd olika loggrepositorier.

När det kommer till implementationsdetaljer, kan valet av din loggningsabstraktion (som Microsoft.Extensions.Logging) och den underliggande loggningsleverantören avsevärt påverka din applikations prestanda och tillförlitlighet. Det är avgörande att konfigurera loggningsnivåerna på ett lämpligt sätt och säkerställa att loggning inte blir en flaskhals.

Dessutom tillåter strukturerad loggning - där du loggar inte bara strängar men nyckel-värdepar eller objekt - för mer precisa och handlingsbara loggar, vilka är enklare att söka i och analysera.

## Se även
- [Dokumentation för Microsoft.Extensions.Logging](https://docs.microsoft.com/en-us/aspnet/core/fundamentals/logging/)
- [Dokumentation för NLog](https://nlog-project.org/documentation/)
- [Dokumentation för log4net](https://logging.apache.org/log4net/)
- [Dokumentation för Serilog](https://serilog.net/) (för ett exempel på strukturerad loggning)
