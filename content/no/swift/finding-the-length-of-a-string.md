---
title:                "Å finne lengden av en streng"
html_title:           "Swift: Å finne lengden av en streng"
simple_title:         "Å finne lengden av en streng"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hva og Hvorfor? 
Nå kan du finne lengden på en streng i Swift enkelt! Programmører trenger ofte å vite hvor mange tegn som er i en streng for å behandle data riktig.

## Hvordan:
```Swift
//Eksempel:
let tekst = "Hei, verden!"
print("Lengden på strengen er: \(tekst.count)")
// Output: Lengden på strengen er: 12
```

## Dypdykk:
- Historisk kontekst: Å finne lengden på en streng har lenge vært en viktig del av programmering og har vært tilgjengelig i mange programmeringsspråk.
- Alternativer: Det finnes flere måter å finne lengden på en streng i Swift, som for eksempel å bruke `NSRange`.
- Implementeringsdetaljer: I Swift er lengden på en streng basert på antall utf8-kodeenheter den inneholder, i motsetning til antall tegn. Derfor kan lengden variere fra antall synlige tegn.

## Se Også:
- [Apple Dokumentasjon for String](https://developer.apple.com/documentation/swift/string)
- [NSRange for å finne lengden på en streng](https://www.hackingwithswift.com/example-code/strings/how-to-count-the-number-of-characters-in-a-string)
- [Utforsk andre Swift string metoder](https://www.swiftbysundell.com/articles/the-power-of-swift-s-string-interpolation/)