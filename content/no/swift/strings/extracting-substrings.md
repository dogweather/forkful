---
date: 2024-01-20 17:46:28.602465-07:00
description: "Utdrag av understrenger betyr \xE5 velge spesifikke deler av en streng.\
  \ Programmerere gj\xF8r dette for \xE5 manipulere, analysere eller formatere tekstdata\
  \ mer\u2026"
lastmod: '2024-03-13T22:44:41.131273-06:00'
model: gpt-4-1106-preview
summary: "Utdrag av understrenger betyr \xE5 velge spesifikke deler av en streng."
title: Uthenting av delstrenger
weight: 6
---

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
