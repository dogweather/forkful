---
title:                "Swift: Sammenslåing av strenger"
programming_language: "Swift"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

# Hvorfor
Det å koble sammen strenger er en viktig ferdighet for enhver Swift-programmerer. Det lar deg kombinere ulike tekstbiter for å lage dynamiske meldinger og beskrivelser i programmene dine.

# Slik gjør du det
```swift
let navn = "Marie"
let alder = 25
let intro = "Hei, jeg heter \(navn) og jeg er \(alder) år gammel."
print(intro)
```
Output: Hei, jeg heter Marie og jeg er 25 år gammel.

# Dykk dypere
Når vi kombinerer strenger ved hjelp av innsettingsoperatøren `\()`, gjør Swift en rekke bak-kulissene endringer for oss. Det oppretter en ny streng ved å kopiere begge de eksisterende strengene og deretter sette dem sammen. Dette kan være en viktig ting å vite når du jobber med store tekstdata som kan føre til ytelsesproblemer.

# Se også
- [Offisiell Swift-dokumentasjon](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Swift blogs](https://swift.org/blog/)