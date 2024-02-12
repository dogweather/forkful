---
title:                "Protokollierung"
aliases:
- /de/c-sharp/logging/
date:                  2024-01-26T01:00:15.006700-07:00
model:                 gpt-4-1106-preview
simple_title:         "Protokollierung"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/de/c-sharp/logging.md"
---

{{< edit_this_page >}}

## Was & Warum?
Logging ist der Vorgang des Aufzeichnens von Anwendungsereignissen und Datenausgaben während der Laufzeit. Programmierer verwenden Logging, um Fehler zu diagnostizieren, die Softwareleistung zu überwachen, Benutzeraktionen zu verfolgen und die Einhaltung von Sicherheits- und Geschäftsstandards zu gewährleisten.

## Wie geht das:
In C# kann man den eingebauten Namensraum `System.Diagnostics` oder Drittanbieter-Bibliotheken wie NLog oder log4net verwenden. Hier ist ein schnelles Beispiel, das die in .NET Core verfügbare `ILogger` Schnittstelle verwendet:

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

        logger.LogInformation("Dies ist eine informative Nachricht.");
        logger.LogWarning("Dies ist eine Warnmeldung.");
        logger.LogError("Dies ist eine Fehlermeldung.");
    }
}
```

Beispielausgabe:
```
info: Program[0]
      Dies ist eine informative Nachricht.
warn: Program[0]
      Dies ist eine Warnmeldung.
fail: Program[0]
      Dies ist eine Fehlermeldung.
```

## Vertiefung
Die Geschichte des Loggings in der Softwareentwicklung ist fast so alt wie die Programmierung selbst; sie hat sich von einfachen Print-Befehlen zu ausgeklügelten, konfigurierbaren Systemen entwickelt. Ursprünglich wurde das Logging durch Schreiben in Dateien oder die Konsole durchgeführt, aber es hat sich zu komplexeren Strukturen wie Log-Aggregationssystemen und verteilten Tracing-Plattformen (wie ELK-Stack oder Jaeger) entwickelt.

Alternativen zum eingebauten Logging in .NET umfassen Drittanbieter-Bibliotheken:
- **NLog**: vielseitig und einfach einzurichten, mit vielen Funktionen für das Routing, Formatieren und Filtern von Logs.
- **log4net**: inspiriert von der Java-Bibliothek log4j, ist es hochkonfigurierbar über XML und unterstützt eine Vielzahl von Log-Repositories.

Wenn es um Implementierungsdetails geht, kann die Wahl Ihrer Logging-Abstraktion (wie Microsoft.Extensions.Logging) und des zugrundeliegenden Logging-Providers die Leistung und Zuverlässigkeit Ihrer Anwendung erheblich beeinflussen. Es ist entscheidend, Logging-Ebenen angemessen zu konfigurieren und sicherzustellen, dass das Schreiben von Logs keinen Engpass wird.

Auch strukturiertes Logging - bei dem man nicht nur Strings, sondern Schlüssel-Wert-Paare oder Objekte loggt - ermöglicht präzisere und umsetzbare Logs, die einfacher zu abfragen und zu analysieren sind.

## Siehe auch
- [Microsoft.Extensions.Logging Dokumentation](https://docs.microsoft.com/de-de/aspnet/core/fundamentals/logging/)
- [NLog Dokumentation](https://nlog-project.org/documentation/)
- [log4net Dokumentation](https://logging.apache.org/log4net/)
- [Serilog Dokumentation](https://serilog.net/) (als Beispiel für strukturiertes Logging)
