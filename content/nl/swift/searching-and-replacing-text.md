---
title:                "Tekst zoeken en vervangen"
aliases:
- nl/swift/searching-and-replacing-text.md
date:                  2024-01-28T22:07:54.660899-07:00
model:                 gpt-4-0125-preview
simple_title:         "Tekst zoeken en vervangen"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/swift/searching-and-replacing-text.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Zoeken en vervangen van tekst in programmeren is precies wat het lijkt: het scannen van strings op bepaalde patronen en deze vervangen door iets anders. Programmeurs doen dit veel - voor dataopruiming, updates van gebruikersinterfaces of het voorbereiden van strings voor verwerking.

## Hoe doe je het:

```Swift
var begroetingen = "Hallo, oude vriend!"

// Eenvoudig vervangen
begroetingen = begroetingen.replacingOccurrences(of: "oude", with: "nieuwe")
print(begroetingen) // "Hallo, nieuwe vriend!"

// Gebruikmakend van opties voor hoofdletterongevoelig vervangen
let hoofdletterongevoeligResultaat = begroetingen.replacingOccurrences(
    of: "hallo",
    with: "Hoi",
    options: .caseInsensitive
)
print(hoofdletterongevoeligResultaat) // "Hoi, nieuwe vriend!"

// Vervangen met reguliere expressies
let regexResultaat = begroetingen.replacingOccurrences(
    of: "\\bnieuwe\\b",
    with: "beste",
    options: .regularExpression
)
print(regexResultaat) // "Hallo, beste vriend!"
```

## Diepgaand

We vervangen al tekst in strings sinds de vroege dagen van computertechniek. Aanvankelijk was dit met eenvoudige command-line tools zoals `sed`. In Swift doet `replacingOccurrences(of:with:)` het zware werk, en je krijgt meer controle met opties zoals `.caseInsensitive` of `.regularExpression`.

Alternatieven in Swift omvatten het gebruik van `NSRegularExpression` voor complexe patronen en `NSMutableString` voor bewerkbare stringbewerkingen. Onder de motorkap bruggen Swift's methoden voor tekstvervanging naar krachtige Objective-C-tegenhangers, waardoor snelheid en veelzijdigheid worden geboden.

## Zie ook

- [Swift String Documentatie](https://developer.apple.com/documentation/swift/string/)
- [Reguliere Expressies in Swift](https://nshipster.com/swift-regular-expressions/)
- [Swift.org - Werken met Strings](https://swift.org/documentation/api-design-guidelines/#strive-for-fluent-usage)
