---
title:                "Loggføring"
date:                  2024-01-26T01:08:48.674887-07:00
model:                 gpt-4-1106-preview
simple_title:         "Loggføring"

category:             "Swift"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/logging.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Logging er prosessen med å registrere applikasjonsatferd, feil, og annen viktig informasjon til et vedvarende medium, som en fil eller database. Programmerere gjør dette for å spore helsen og ytelsen til appene sine, for å feilsøke problemer, og for å holde et øye med hva som skjer under panseret i produksjonsmiljøer.

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
