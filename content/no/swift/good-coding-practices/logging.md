---
date: 2024-01-26 01:08:48.674887-07:00
description: "Hvordan gj\xF8re det: I Swift kan du skrive logger til konsollen med\
  \ utskriftssetninger eller det mer fleksible `os.log` API-et, som kobler seg p\xE5\
  \ det\u2026"
lastmod: '2024-03-13T22:44:41.148566-06:00'
model: gpt-4-1106-preview
summary: "I Swift kan du skrive logger til konsollen med utskriftssetninger eller\
  \ det mer fleksible `os.log` API-et, som kobler seg p\xE5 det enhetlige loggsystemet\
  \ p\xE5 Apple-plattformer."
title: "Loggf\xF8ring"
weight: 17
---

## Hvordan gjøre det:
I Swift kan du skrive logger til konsollen med utskriftssetninger eller det mer fleksible `os.log` API-et, som kobler seg på det enhetlige loggsystemet på Apple-plattformer.

```Swift
import os.log

let logger = OSLog(subsystem: "com.dinapp.domene", category: "nettverk")

func fetchData() {
    // Enkel utskriftssetning
    print("Henting startet")
    
    // Logger en hendelse på infonivå ved bruk av os.log
    os_log(.info, log: logger, "Henter data fra API.")
    
    do {
        let data = try performNetworkRequest()
        // Logger en hendelse på debug-nivå
        os_log(.debug, log: logger, "Data mottatt: %@", data.description)
    } catch {
        // Logger en hendelse på feilnivå
        os_log(.error, log: logger, "Klarte ikke å hente data: %@", error.localizedDescription)
    }
}

func performNetworkRequest() throws -> Data {
    // Simulerer et nettverksforespørsel
    return Data()
}
```

Eksempel på utskrift i konsollen kan se slik ut:

```
Henting startet
Henter data fra API.
Data mottatt: Noen databyter...
```

For feil, kan det være:

```
Klarte ikke å hente data: Internettforbindelsen ser ut til å være frakoblet.
```

## Dypdykk
Logging i Swift tar i bruk ny kraft og effektivitet med det enhetlige loggsystemet introdusert i iOS 10 og macOS Sierra. I motsetning til `print`-setningen som går rett til konsollen, er dette systemet basert på aktivitet, og lar deg filtrere loggmeldinger basert på deres viktighet og om de er debug- eller release-bygg.

Den historiske konteksten rammer inn utviklingen av logging i iOS og macOS fra grunnleggende utskriftssetninger mot omfattende verktøy som integrerer med Instruments-appen og Konsoll, og gir sofistikerte måter å analysere logger på.

Det finnes et utvalg av alternativer til logging innen Swift, som tredjepartsbiblioteker som CocoaLumberjack, som tilbyr et makrolag over det enhetlige loggsystemet. Det gir forbedret kontroll over loggformatering, filhåndtering, og ytelsesalternativer.

Til slutt, implementeringsdetaljer; OSLog er designet for å ikke bare være effektivt, men også personvernsbevisst, med evnen til å obfuskere privat data når det logges. Det kategoriserer logger i feil, feil, info, og debug-nivåer, hver tilbyr ulik granularitet for feilsøking.

## Se også
- [Apples dokumentasjon for enhetlig logging](https://developer.apple.com/documentation/os/logging)
- [Ray Wenderlich Logging-opplæring](https://www.raywenderlich.com/605079-logging-in-swift-oslog)
- [CocoaLumberjack GitHub-repositorium](https://github.com/CocoaLumberjack/CocoaLumberjack)
