---
date: 2024-01-26 00:57:55.209145-07:00
description: "\xC5 h\xE5ndtere feil i Swift betyr \xE5 forutse og svare p\xE5 problemer\
  \ som dukker opp n\xE5r koden din kj\xF8rer. Vi gj\xF8r det for \xE5 kontrollere\
  \ kaoset\u2014holde apper fra \xE5\u2026"
lastmod: '2024-03-13T22:44:41.149527-06:00'
model: gpt-4-1106-preview
summary: "\xC5 h\xE5ndtere feil i Swift betyr \xE5 forutse og svare p\xE5 problemer\
  \ som dukker opp n\xE5r koden din kj\xF8rer."
title: "Feilh\xE5ndtering"
weight: 16
---

## Hvordan:
Swift bruker feilhåndtering med `do`, `try` og `catch` blokker. La oss ta en titt:

```Swift
enum FileError: Error {
    case fileDoesNotExist
    case noPermission
}

func readFile(atPath path: String) throws -> String {
    // Forestill deg at vi har litt logikk her for å sjekke om en fil finnes og om vi har tillatelse til å lese den
    let fileExists = false
    let havePermission = true

    if !fileExists {
        throw FileError.fileDoesNotExist
    }

    if !havePermission {
        throw FileError.noPermission
    }

    return "Filinnhold går her"
}

do {
    let fileContent = try readFile(atPath: "/path/to/file")
    print(fileContent)
} catch FileError.fileDoesNotExist {
    print("Oisann! Filen ble ikke funnet.")
} catch FileError.noPermission {
    print("Åh! Ingen tillatelse til å lese filen.")
} catch {
    print("En ukjent feil oppstod.")
}

```

Eksempel på utdata:

```
Oisann! Filen ble ikke funnet.
```

## Dypdykk
Feilhåndtering var ikke alltid så elegant som den er nå. I Objective-C ville du håndtere pekere til NSError-objekter, noe som føltes klumpete. Nå har vi et mer elegant system med Swift enums og `Error` protokollen.

Swifts `throw` lar oss signalisere at noe har gått galt. `do` blokker fungerer som feilbevisste områder, `try` prefiks kaller risikabel forretning, og `catch` håndterer ting hvis de går sørover.

Optionals er et alternativ for situasjoner som ikke helt er "feil" status, men som likevel kan ha "intet resultat". De er litt som Schrödingers variabler—de har en verdi eller så har de ikke det.

For virkelig dybde, sjekk ut `Result` typer, som er elegante hybrider mellom vanlig-retur og feilmønstre.

## Se også
- Offisiell Swift Feilhåndteringsguide: [Apple Docs](https://docs.swift.org/swift-book/LanguageGuide/ErrorHandling.html)
- Beste praksiser for feilhåndtering i Swift: [RayWenderlich.com](https://www.raywenderlich.com/1851-beginning-swift-error-handling)
- Avansert feilhåndtering i Swift: [Medium Artikkel](https://medium.com/better-programming/advanced-error-handling-in-swift-4f6bdf6b01d8)
