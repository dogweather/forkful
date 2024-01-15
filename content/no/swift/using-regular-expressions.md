---
title:                "Bruk av regulære uttrykk"
html_title:           "Swift: Bruk av regulære uttrykk"
simple_title:         "Bruk av regulære uttrykk"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Å bruke regulære uttrykk i Swift kan hjelpe deg med å effektivt søke, filtrere og manipulere tekstbaserte data. Det er en kraftig og fleksibel måte å behandle strenger på, og kan være spesielt nyttig for å analysere og tolke store datamengder.

## Hvordan

Det første trinnet for å bruke regulære uttrykk i Swift er å importere foundation biblioteket:

```Swift
import Foundation
```

For å opprette et regulært uttrykk, bruk følgende syntaks:

```Swift
do {
    let regex = try NSRegularExpression(pattern: "regex pattern", options: [])
} catch {
    // handle errors
}
```

Her kan du erstatte "regex pattern" med det faktiske mønsteret du vil søke etter. Det er mange spesielle tegn som betyr forskjellige ting i regulære uttrykk, så det kan være lurt å se på en tutorial for å forstå hvordan mønstre fungerer.

Når du har opprettet et regulært uttrykk, kan du bruke den til å søke gjennom en streng og finne matcher. Her er et eksempel som finner alle numre i en streng:

```Swift
let input = "There are 5 apples and 2 oranges."
do {
    let regex = try NSRegularExpression(pattern: "[0-9]+", options: .caseInsensitive)
    let results = regex.matches(in: input, options: [], range: NSRange(location: 0, length: input.count))
    for result in results {
        print("Found match: \(input[Range(result.range, in: input)!])")
    }
} catch {
   // handle errors
}

// Output:
// Found match: 5
// Found match: 2
```

## Dypdykk

Det er mange muligheter når det kommer til å bruke regulære uttrykk i Swift, som inkluderer erstatning og splittelse av strenger. Du kan også bruke såkalte "capture groups" til å hente ut spesifikk informasjon fra en match. For mer avanserte funksjoner, kan du utforske NSTextCheckingResult og NSTextCheckingType.

En av fordelene med å bruke regulære uttrykk i Swift er at du kan teste dem ut raskt og enkelt ved hjelp av online verktøy som [RegExr](https://regexr.com/) og [Regex101](https://regex101.com/). Disse nettstedene tilbyr støtte for Swift-syntaks og kan hjelpe deg med å feilsøke og perfeksjonere dine regulære uttrykk.

## Se Også

- [NSRegularExpression dokumentasjon](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Swift RegEx tutorial by Hacking with Swift](https://www.hackingwithswift.com/articles/108/how-to-use-regular-expressions-in-swift)