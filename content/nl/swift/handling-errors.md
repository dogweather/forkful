---
title:                "Fouten afhandelen"
aliases:
- nl/swift/handling-errors.md
date:                  2024-01-28T22:01:46.196692-07:00
model:                 gpt-4-0125-preview
simple_title:         "Fouten afhandelen"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/swift/handling-errors.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Foutafhandeling in Swift betekent dat je anticipeert op en reageert op problemen die opduiken wanneer je code wordt uitgevoerd. We doen dit om de chaos te beheersen—om te voorkomen dat apps crashen en om de gebruiker een soepele ervaring te bieden.

## Hoe te:
Swift gebruikt foutafhandeling met `do`, `try` en `catch` blokken. Laten we eens kijken:

```Swift
enum FileError: Error {
    case fileDoesNotExist
    case noPermission
}

func readFile(atPath path: String) throws -> String {
    // Stel je voor dat we hier wat logica hebben om te controleren of een bestand bestaat en of we toestemming hebben om het te lezen
    let fileExists = false
    let havePermission = true

    if !fileExists {
        throw FileError.fileDoesNotExist
    }

    if !havePermission {
        throw FileError.noPermission
    }

    return "Inhoud van het bestand gaat hier"
}

do {
    let fileContent = try readFile(atPath: "/path/to/file")
    print(fileContent)
} catch FileError.fileDoesNotExist {
    print("Oeps! Bestand niet gevonden.")
} catch FileError.noPermission {
    print("Ah! Geen toestemming om het bestand te lezen.")
} catch {
    print("Er is een onbekende fout opgetreden.")
}

```

Voorbeelduitvoer:

```
Oeps! Bestand niet gevonden.
```

## Diepere Duik
Foutafhandeling was niet altijd zo soepel als het nu is. In Objective-C zou je omgaan met pointers naar NSError-objecten, wat onhandig aanvoelde. Nu hebben we een eleganter systeem met Swift enums en het `Error` protocol.

Swift’s `throw` laat ons signaleren dat er iets mis is gegaan. `do` blokken werken als foutbewuste domeinen, `try` prefix roept het risicovolle op, en `catch` handelt dingen af als ze misgaan.

Optionals zijn een alternatief voor situaties die niet echt "fout" status zijn, maar toch "geen resultaat" kunnen hebben. Ze zijn een beetje als Schrödinger's variabelen—ze hebben een waarde of ze hebben het niet.

Voor echt diepgang, bekijk `Result` types, die zijn snazzy hybriden tussen gewone-retour en fout patronen.

## Zie Ook
- Officiële Swift Foutafhandeling Gids: [Apple Docs](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)
- Swift Foutafhandeling Beste Praktijken: [RayWenderlich.com](https://www.raywenderlich.com/1851-beginning-swift-error-handling)
- Geavanceerde Foutafhandeling in Swift: [Medium Artikel](https://medium.com/better-programming/advanced-error-handling-in-swift-4f6bdf6b01d8)
