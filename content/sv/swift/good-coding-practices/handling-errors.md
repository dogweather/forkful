---
date: 2024-01-26 00:58:06.229216-07:00
description: "Hur g\xF6r man: Swift anv\xE4nder felhantering med `do`, `try`, och\
  \ `catch`-block. L\xE5t oss ta en titt."
lastmod: '2024-03-13T22:44:38.258383-06:00'
model: gpt-4-1106-preview
summary: "Swift anv\xE4nder felhantering med `do`, `try`, och `catch`-block."
title: Hantering av fel
weight: 16
---

## Hur gör man:
Swift använder felhantering med `do`, `try`, och `catch`-block. Låt oss ta en titt:

```Swift
enum FileError: Error {
    case fileDoesNotExist
    case noPermission
}

func readFile(atPath path: String) throws -> String {
    // Låtsas att vi har någon logik här för att kontrollera om en fil finns och om vi har tillåtelse att läsa den
    let fileExists = false
    let havePermission = true

    if !fileExists {
        throw FileError.fileDoesNotExist
    }

    if !havePermission {
        throw FileError.noPermission
    }

    return "Filinnehållet skulle vara här"
}

do {
    let fileContent = try readFile(atPath: "/path/to/file")
    print(fileContent)
} catch FileError.fileDoesNotExist {
    print("Hoppsan! Filen hittades inte.")
} catch FileError.noPermission {
    print("Åh! Ingen tillåtelse att läsa filen.")
} catch {
    print("Ett okänt fel inträffade.")
}

```

Exempel på utdata:

```
Hoppsan! Filen hittades inte.
```

## Fördjupning
Felhantering var inte alltid så smidigt som det är nu. I Objective-C skulle du hantera pekare till NSError-objekt, vilket kändes klumpigt. Nu har vi ett mer elegant system med Swift enums och `Error`-protokollet.

Swifts `throw` låter oss signalera att något har blivit fel. `do`-block fungerar som områden medvetna om fel, `try`-prefixet anropar den riskfyllda verksamheten, och `catch` hanterar saker om de går snett.

Optionals är ett alternativ för situationer som inte riktigt är "fel"-status, men som ändå kanske inte har något "resultat". De liknar lite Schrödingers variabler – de har ett värde eller så har de inte det.

För verklig fördjupning, kolla in `Result`-typer, som är finurliga hybrider mellan ordinarie retur- och felpatron.

## Se även
- Officiell Swift-felhanteringsguide: [Apple Docs](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)
- Bästa praxis för felhantering i Swift: [RayWenderlich.com](https://www.raywenderlich.com/1851-beginning-swift-error-handling)
- Avancerad felhantering i Swift: [Medium Artikel](https://medium.com/better-programming/advanced-error-handling-in-swift-4f6bdf6b01d8)
