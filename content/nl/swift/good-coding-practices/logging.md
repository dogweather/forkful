---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:02:56.051492-07:00
description: "Loggen is het proces van het vastleggen van applicatiegedragingen, fouten\
  \ en andere belangrijke informatie in een blijvend medium, zoals een bestand of\u2026"
lastmod: '2024-03-13T22:44:51.163737-06:00'
model: gpt-4-0125-preview
summary: Loggen is het proces van het vastleggen van applicatiegedragingen, fouten
  en andere belangrijke informatie in een blijvend medium, zoals een bestand of database.
title: Logboekregistratie
weight: 17
---

## Hoe:
In Swift kun je logs naar de console schrijven met printopdrachten of de flexibelere `os.log` API, die aansluit op het Unified Logging System op Apple-platforms.

```Swift
import os.log

let logger = OSLog(subsystem: "com.jouwapp.domein", category: "netwerk")

func fetchData() {
    // Eenvoudige printopdracht
    print("Ophalen gestart")
    
    // Info-niveau event loggen met os.log
    os_log(.info, log: logger, "Data ophalen van API.")
    
    do {
        let data = try performNetworkRequest()
        // Debug-niveau event loggen
        os_log(.debug, log: logger, "Gegevens ontvangen: %@", data.description)
    } catch {
        // Fout-niveau event loggen
        os_log(.error, log: logger, "Mislukt om gegevens op te halen: %@", error.localizedDescription)
    }
}

func performNetworkRequest() throws -> Data {
    // Simuleer een netwerkaanvraag
    return Data()
}
```

Voorbeelduitvoer op de console zou er zo uit kunnen zien:

```
Ophalen gestart
Data ophalen van API.
Gegevens ontvangen: Enkele databytes...
```

Voor fouten zou het kunnen zijn:

```
Mislukt om gegevens op te halen: De internetverbinding lijkt offline te zijn.
```

## Diepgaand
Loggen in Swift krijgt met het Uniform Logging System, geïntroduceerd in iOS 10 en macOS Sierra, nieuwe kracht en efficiëntie. In tegenstelling tot de `print` opdracht die rechtstreeks naar de console gaat, is dit systeem activiteit-gebaseerd en stelt het je in staat om logberichten te filteren op basis van hun belangrijkheid en of ze debug- of releasebuilds zijn.

De historische context omkadert de evolutie van loggen in iOS en macOS van rudimentaire printopdrachten naar uitgebreide hulpmiddelen die integreren met de Instruments-app en Console, die geavanceerde manieren bieden om logs te analyseren.

Er zijn een reeks alternatieven voor loggen binnen Swift, zoals externe bibliotheken zoals CocoaLumberjack, dat een macro-laag biedt over het Uniform Logging System. Het biedt verbeterde controle over logformattering, bestandsbeheer en prestatieopties.

Tot slot, implementatiedetails; OSLog is ontworpen om niet alleen efficiënt te zijn, maar ook privacybewust, met de mogelijkheid om privégegevens te verhullen bij het loggen. Het categoriseert logs in fault, error, info en debug niveaus, elk biedt een andere granulariteit voor probleemoplossing.

## Zie Ook
- [Documentatie van Apple's Uniform Logging](https://developer.apple.com/documentation/os/logging)
- [Ray Wenderlich Logging tutorial](https://www.raywenderlich.com/605079-logging-in-swift-oslog)
- [CocoaLumberjack GitHub-repository](https://github.com/CocoaLumberjack/CocoaLumberjack)
