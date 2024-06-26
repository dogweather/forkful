---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:13:42.540861-07:00
description: 'Hoe te: Swift maakt schrijven naar `stderr` eenvoudig. Zie het onderstaande
  voorbeeld.'
lastmod: '2024-03-13T22:44:51.174299-06:00'
model: gpt-4-0125-preview
summary: Swift maakt schrijven naar `stderr` eenvoudig.
title: Schrijven naar standaardfout
weight: 25
---

## Hoe te:
Swift maakt schrijven naar `stderr` eenvoudig. Zie het onderstaande voorbeeld:

```Swift
import Foundation

// Schrijven naar standaard fout
func writeToStdErr(_ message: String) {
    if let data = "\(message)\n".data(using: .utf8) {
        FileHandle.standardError.write(data)
    }
}

// Voorbeeldgebruik
writeToStdErr("Oeps! Er is iets fout gegaan.")

// Uitvoer wanneer uitgevoerd in een console zou er zo uit kunnen zien
// (hoewel dit niet zichtbaar zal zijn in de console van Xcode):
// Oeps! Er is iets fout gegaan.
```

## Diepgaande duik
In de vroege dagen van het programmeren was het onderscheiden tussen `stdout` (standaard uitvoer) en `stderr` (standaard fout) vitaal voor het parseren van commando-uitvoer en het afhandelen van fouten. Andere talen bieden vergelijkbare constructies, en in Unix-gebaseerde systemen hebben deze stromen direct betrekking op de terminal. Dit implementeren in Swift tapt in dezelfde onderliggende principes, waar `stderr` functioneert als een ongebufferde stroom, wat betekent dat het onmiddellijk de uitvoer doorstuurt. Dit gedrag is cruciaal voor realtime foutenrapportage.

Alternatieven omvatten logboekregistratiekaders die meer functies kunnen bieden, zoals logniveaus en berichtformaten. Swift's eigen standaardbibliotheken zijn vrij minimalistisch; als je behoefte hebt aan verfijning, zul je waarschijnlijk naar externe bibliotheken of het Apple-verenigde logboeksysteem kijken.

## Zie ook
Voor een dieper begrip en aanvullende context, bekijk deze bronnen:

- [Documentatie van Apple's Unified Logging](https://developer.apple.com/documentation/os/logging)
- [Swift’s Standard Library referentie voor FileHandle](https://developer.apple.com/documentation/foundation/filehandle)
