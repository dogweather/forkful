---
title:                "Skrive ut feilsøkingsdata"
aliases:
- /no/swift/printing-debug-output/
date:                  2024-01-20T17:53:32.458244-07:00
model:                 gpt-4-1106-preview
simple_title:         "Skrive ut feilsøkingsdata"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why? (Hva & Hvorfor?)
Å skrive ut debug-informasjon innebærer å vise verdier og meldinger under kjøring av programmet, for å forstå hva som skjer "under hetta". Vi gjør dette for å finne og fikse feil mer effektivt.

## How to: (Hvordan:)
Vi bruker `print()`-funksjonen i Swift til å skrive ut verdier til konsollen. La oss sjekke det ut:

```Swift
var currentStatus = "Working"
print(currentStatus)  // Skriver ut: Working

let numbers = [1, 2, 3]
for number in numbers {
    print("Number is \(number)")  // Gjentatte utskrifter: Number is 1, Number is 2, osv.
}
```

Vi kan også bruke `debugPrint()` for mer detaljert output, som kan inkludere typen til variablene:

```Swift
debugPrint(currentStatus) // Skriver ut: "Working"
debugPrint(numbers)       // Skriver ut: [1, 2, 3]
```

## Deep Dive (Dypdykk)
Historisk sett har `print`-statements vært gullstandarden for å feilsøke kode, siden det er enkelt å sette dem inn og se resultatene umiddelbart. Men rene `print`-statements kan bli rotete i komplekse applikasjoner. Derfor har vi nå bedre alternativer som `debugPrint()` og til og med `os_log()`, som gir oss mer kontroll og filtreringsmuligheter for loggmeldingene våre.

I Swift kan vi også vurdere bruk av `assert()` for å stoppe programmet om noe uventet oppstår, eller `precondition()` om vi vil at sjekken også skal kjøres i produksjonsmodus.

For komplekse prosjekter kan det være lurt å implementere en skikkelig logger som kan håndtere ulike loggnivåer (info, debug, error, osv.).

## See Also (Se Også)
For å utforske mer avansert feilsøkingsmetodikk i Swift, ta en titt på følgende ressurser:

- Apple's documentation on [Unified Logging and Activity Tracing](https://developer.apple.com/documentation/os/logging).
- Swift.org's guide on [API Design Guidelines](https://www.swift.org/documentation/api-design-guidelines/).
- Ray Wenderlich's tutorial on [Swift Logging](https://www.raywenderlich.com/605079-swift-logging).
