---
title:                "Hantering av fel"
date:                  2024-01-26T00:58:06.229216-07:00
model:                 gpt-4-1106-preview
simple_title:         "Hantering av fel"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/handling-errors.md"
---

{{< edit_this_page >}}

## Vad och Varför?
Att hantera fel i Swift innebär att förutse och svara på problem som dyker upp när din kod körs. Vi gör det för att kontrollera kaoset – för att förhindra att appar kraschar och för att ge användaren en smidig upplevelse.

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
