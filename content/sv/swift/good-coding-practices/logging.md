---
date: 2024-01-26 01:08:19.841487-07:00
description: "Loggning \xE4r processen att registrera applikationsbeteenden, fel och\
  \ annan viktig information till ett best\xE4ndigt medium, som en fil eller databas.\u2026"
lastmod: 2024-02-19 22:04:57.497882
model: gpt-4-1106-preview
summary: "Loggning \xE4r processen att registrera applikationsbeteenden, fel och annan\
  \ viktig information till ett best\xE4ndigt medium, som en fil eller databas.\u2026"
title: Loggning
---

{{< edit_this_page >}}

## Vad & Varför?
Loggning är processen att registrera applikationsbeteenden, fel och annan viktig information till ett beständigt medium, som en fil eller databas. Programmerare gör detta för att spåra sina applikationers hälsa och prestanda, för att felsöka problem och för att hålla ett öga på vad som händer under huven i produktionsmiljöer.

## Hur man gör:
I Swift kan du skriva loggar till konsolen med utskriftsuttryck eller den mer flexibla `os.log` API-et, vilket kopplar in i det Enhetsamma Loggsystemet på Apples plattformar.

```Swift
import os.log

let logger = OSLog(subsystem: "com.yourapp.domain", category: "network")

func fetchData() {
    // Enkelt utskriftsuttryck
    print("Börjar hämtning")
    
    // Loggar en info-nivå händelse med os.log
    os_log(.info, log: logger, "Hämtar data från API.")
    
    do {
        let data = try performNetworkRequest()
        // Loggar en debug-nivå händelse
        os_log(.debug, log: logger, "Data mottagen: %@", data.description)
    } catch {
        // Loggar en fel-nivå händelse
        os_log(.error, log: logger, "Misslyckades med att hämta data: %@", error.localizedDescription)
    }
}

func performNetworkRequest() throws -> Data {
    // Simulerar en nätverksförfrågan
    return Data()
}
```

Exempelutdata på konsolen kan se ut så här:

```
Börjar hämtning
Hämtar data från API.
Data mottagen: Lite databitar...
```

För fel kan det vara:

```
Misslyckades med att hämta data: Internetanslutningen verkar vara offline.
```

## Djupdykning
Loggning i Swift tar ny kraft och effektivitet med det Enhetsamma Loggsystemet som introducerades i iOS 10 och macOS Sierra. Till skillnad från `print`-uttrycket som går direkt till konsolen, är detta system aktivitetsbaserat och låter dig filtrera loggmeddelanden baserat på deras betydelse och om de är för debug- eller release-byggen.

Den historiska kontexten ramar in utvecklingen av loggning i iOS och macOS från grundläggande print-uttryck till omfattande verktyg som integrerar med Instrument-appen och Konsolen, och ger avancerade sätt att analysera loggar.

Det finns ett utbud av alternativ till loggning inom Swift, såsom tredjepartsbibliotek likt CocoaLumberjack, som erbjuder ett makrolager över det Enhetsamma Loggsystemet. Det ger ökad kontroll över loggformatering, filhantering och prestandainställningar.

Avslutningsvis, implementationdetaljer; OSLog är designat för att inte bara vara effektivt men också integritetsmedvetet, med förmågan att dölja privat data vid loggning. Det kategoriserar loggar i nivåerna fault, error, info och debug, var och en erbjuder en annan granularitet för felsökning.

## Se även
- [Apples dokumentation om Enhetsamt Loggsystem](https://developer.apple.com/documentation/os/logging)
- [Ray Wenderlichs loggtutorial](https://www.raywenderlich.com/605079-logging-in-swift-oslog)
- [CocoaLumberjacks GitHub-repository](https://github.com/CocoaLumberjack/CocoaLumberjack)
