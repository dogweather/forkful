---
title:                "Uthenting av delstrenger"
aliases:
- no/swift/extracting-substrings.md
date:                  2024-01-20T17:46:28.602465-07:00
model:                 gpt-4-1106-preview
simple_title:         "Uthenting av delstrenger"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Utdrag av understrenger betyr å velge spesifikke deler av en streng. Programmerere gjør dette for å manipulere, analysere eller formatere tekstdata mer spesifikt.

## Slik gjør du:
For å ekstrahere understrenger i Swift, bruk `String`-indekser og substring-metoder som `prefix`, `suffix`, `split`, eller `subscript`-området.

```Swift
let tekst = "Hallo, Norge!"
let start = tekst.startIndex
let slutt = tekst.index(start, offsetBy: 5)
let hilsen = tekst[start..<slutt] // Hallo
print(hilsen)

let hale = tekst.suffix(6) // Norge!
print(hale)

let ord = tekst.split(separator: ',') // ["Hallo", " Norge!"]
print(ord[1].trimmingCharacters(in: .whitespaces))
```

## Dypdykk
Før Swift 4 brukte vi `NSString`-metoder for strengmanipulering, som var mindre typetrygge og Swift-idiomatiske. Med Swift endret dette seg, substrings ble mer effektive ved å dele underliggende data med originalstrengen fremfor å kopiere. Når du trenger ytterligere tilpasning, kan `RegularExpression` komme til unnsetning. For ytelse er det verdt å merke seg at `String` i Swift er verditype, men med optimering kalt "copy-on-write" som forhindrer unødvendige kopier.

## Se også
- Swift-dokumentasjon for `String` og `Substring`: [Swift String and Characters](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Swift standardbiblioteksreferanse: [Swift Standard Library](https://developer.apple.com/documentation/swift/swift_standard_library)
