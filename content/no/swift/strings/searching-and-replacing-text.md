---
title:                "Søking og erstatting av tekst"
aliases:
- /no/swift/searching-and-replacing-text.md
date:                  2024-01-20T17:58:36.284973-07:00
model:                 gpt-4-1106-preview
simple_title:         "Søking og erstatting av tekst"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Søke og erstatte tekst lar deg finne og skifte ut ord eller fraser i en streng. Det sparer tid og minimerer feil når du jobber med store datamengder eller oppdaterer kode.

## Hvordan:
```Swift
let originalText = "Det var en gang en programmerer som kodet Swift."
let searchText = "programmerer"
let replacementText = "utvikler"

if let range = originalText.range(of: searchText) {
    let newText = originalText.replacingCharacters(in: range, with: replacementText)
    print(newText)
} else {
    print("Teksten ble ikke funnet.")
}

// Output
// Det var en gang en utvikler som kodet Swift.
```

## Dypdykk
Før i tiden krevde tekstbehandling tunge operasjoner. Nå, med Swift og dens String API, er søk og erstatting enkel. Alternativer inkluderer regulære uttrykk for komplekse mønster. Ved implementering, vær obs på Swift sin Unicode-representasjon av strenger, som påvirker hvordan de indekseres.

## Se også:
- Swift dokumentasjon om String: https://developer.apple.com/documentation/swift/string
- Tutorial om Swift regulære uttrykk: https://www.raywenderlich.com/5765-regular-expressions-tutorial-getting-started
- Apple's String Programming Guide: https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/Strings/
