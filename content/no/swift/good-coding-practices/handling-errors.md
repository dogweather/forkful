---
title:                "Feilhåndtering"
aliases: - /no/swift/handling-errors.md
date:                  2024-01-26T00:57:55.209145-07:00
model:                 gpt-4-1106-preview
simple_title:         "Feilhåndtering"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/handling-errors.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Å håndtere feil i Swift betyr å forutse og svare på problemer som dukker opp når koden din kjører. Vi gjør det for å kontrollere kaoset—holde apper fra å krasje og gi brukeren en jevn opplevelse.

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
