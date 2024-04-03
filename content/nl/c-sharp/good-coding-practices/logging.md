---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:38.728664-07:00
description: "Loggen is het proces van het vastleggen van applicatiegebeurtenissen\
  \ en data-output tijdens uitvoeringstijd. Programmeurs loggen om bugs te\u2026"
lastmod: '2024-03-13T22:44:50.816703-06:00'
model: gpt-4-0125-preview
summary: Loggen is het proces van het vastleggen van applicatiegebeurtenissen en data-output
  tijdens uitvoeringstijd.
title: Logboekregistratie
weight: 17
---

## Wat & Waarom?
Loggen is het proces van het vastleggen van applicatiegebeurtenissen en data-output tijdens uitvoeringstijd. Programmeurs loggen om bugs te diagnosticeren, softwareprestaties te monitoren, gebruikersacties te volgen en naleving van beveiligings- en bedrijfsnormen te handhaven.

## Hoe te:
In C# kun je de ingebouwde `System.Diagnostics`-naamruimte of externe bibliotheken zoals NLog of log4net gebruiken. Hier is een snel voorbeeld met behulp van de `ILogger` interface beschikbaar in .NET Core:

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

        logger.LogInformation("Dit is een informatief bericht.");
        logger.LogWarning("Dit is een waarschuwingsbericht.");
        logger.LogError("Dit is een foutmelding.");
    }
}
```

Voorbeeldoutput:
```
info: Program[0]
      Dit is een informatief bericht.
warn: Program[0]
      Dit is een waarschuwingsbericht.
fail: Program[0]
      Dit is een foutmelding.
```

## Diepgaande Duik
De geschiedenis van loggen in softwareontwikkeling is bijna zo oud als programmeren zelf; het is geëvolueerd van simpele printstatements naar verfijnde, configureerbare systemen. Oorspronkelijk werd er gelogd door te schrijven naar bestanden of de console, maar dit is uitgegroeid tot meer complexe structuren zoals logsamenvoegingssystemen en gedistribueerde traceringsplatforms (zoals ELK-stack of Jaeger).

Alternatieven voor het ingebouwde loggen in .NET zijn externe bibliotheken:
- **NLog**: veelzijdig en eenvoudig in te stellen, met veel functies voor routing, formattering en filtering van logs.
- **log4net**: geïnspireerd door de Java log4j-bibliotheek, het is zeer configureerbaar vanuit XML en ondersteunt een verscheidenheid aan logrepositories.

Wat betreft de implementatiedetails, de keuze van je logabstractie (zoals Microsoft.Extensions.Logging) en de onderliggende logprovider kunnen een aanzienlijke invloed hebben op de prestaties en betrouwbaarheid van je applicatie. Het is cruciaal om logniveaus gepast te configureren en ervoor te zorgen dat het schrijven van logs geen knelpunt wordt.

Ook gestructureerd loggen - waarbij je niet alleen strings logt maar sleutel-waardeparen of objecten - maakt logs nauwkeuriger en actiegericht, wat ze gemakkelijker te bevragen en analyseren maakt.

## Zie Ook
- [Microsoft.Extensions.Logging Documentatie](https://docs.microsoft.com/en-us/aspnet/core/fundamentals/logging/)
- [NLog Documentatie](https://nlog-project.org/documentation/)
- [log4net Documentatie](https://logging.apache.org/log4net/)
- [Serilog Documentatie](https://serilog.net/) (voor een voorbeeld van gestructureerd loggen)
