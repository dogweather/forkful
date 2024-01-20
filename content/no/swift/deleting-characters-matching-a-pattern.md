---
title:                "Slette tegn som samsvarer med et mønster"
html_title:           "Arduino: Slette tegn som samsvarer med et mønster"
simple_title:         "Slette tegn som samsvarer med et mønster"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

---

# Fjerne tegn som passer til et mønster i Swift!

## Hva & Hvorfor?

Å slette tegn som passer til et mønster er en prosess der du fjerner bestemte tegn basert på et sett med kriterier fra en streng. Dette er nyttig for å fjerne unødvendige, uønskede data eller lage mer lesbare og renere koder.

## Hvordan:

Her er et kort eksempel i Swift på hvordan du kan fjerne tegn som passer til et mønster. Vi vil bruke en regulær uttrykksinstans til å definere mønsteret vårt og deretter bruke `replacingOccurrences(of:with:options:range:)` for å fjerne tegnene.

```Swift
import Foundation

let tekst = "abc_123_def_456_ghi_789"
let pattern = "[_\\d]"

let regex = try! NSRegularExpression(pattern: pattern)
let newTekst = regex.stringByReplacingMatches(in: tekst, options: [], range: NSRange(tekst.startIndex..., in: tekst), withTemplate: "")

print(newTekst)  // Output: "abcdefghi"
```

## Dypdykk

Historisk, før Swift, brukte andre programmeringsspråk som JavaScript og Python lignende teknikker for å håndtere tegnfjerning. Swift har imidlertid moderne funksjoner, som den innebygde regulære uttrykksstøtten, som gjør det lettere å arbeide med mønstre.

Swift tilbyr også forskjellige metoder for å håndtere utfordringen. Du kan bruke `filter`, `reduce`, `replacingOccurrences` eller bruk av `Character` typen, avhengig av situasjonen. Men å bruke regulære uttrykk gir oss den mest bøyelige løsningen ved å arbeide direkte med strenger.

Implementasjonen av denne funksjonaliteten i Swift er effektiv og rask, takket være Swifts sterke støtte for tekstbehandling og optimalisering av datastrukturene den bruker.

## Se Også

For flere detaljer om Swift og avansert tekstbehandling, ta en titt på disse ressursene:

1. Apples Swift Programming Language Guide: [Swift Programming Guide](https://docs.swift.org/swift-book/)
2. NSHipster's article on `NSRegularExpression`: [NSRegularExpression](https://nshipster.com/nsregularexpression/)
   
---