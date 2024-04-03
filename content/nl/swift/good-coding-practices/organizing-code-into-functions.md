---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:03:22.758031-07:00
description: "Code groeperen in functies is het opsplitsen van taken in herbruikbare\
  \ blokken. Hierdoor wordt de code overzichtelijk, minder foutgevoelig en makkelijker\u2026"
lastmod: '2024-03-13T22:44:51.162688-06:00'
model: gpt-4-0125-preview
summary: Code groeperen in functies is het opsplitsen van taken in herbruikbare blokken.
title: Code organiseren in functies
weight: 18
---

## Hoe:
Stel je een taak voor: het berekenen van het gemiddelde van een array. Zonder functies zou je alles in main stoppen. Met functies doe je dit:

```swift
func berekenGemiddelde(van getallen: [Double]) -> Double {
    let som = getallen.reduce(0, +)
    return getallen.isEmpty ? 0 : som / Double(getallen.count)
}

// Gebruik
let scores = [92.5, 88.75, 99.0, 70.5]
let gemiddeldeScore = berekenGemiddelde(van: scores)
print("Gemiddelde score is \(gemiddeldeScore)")
```

De voorbeelduitvoer zou zijn:
```
Gemiddelde score is 87.6875
```

## Diepgaand
Historisch gezien, naarmate programmeren complexer werd, werden functies een sleutelsteen voor het beheren van complexiteit. Alternatieven omvatten inline codering en code kopiëren-plakken (spaghetti-code) – nu grotendeels beschouwd als slechte praktijk. In Swift zijn functies van eerste klasse; ze kunnen worden toegewezen aan variabelen, doorgegeven als argumenten en geretourneerd door andere functies, waardoor de code modulairder en flexibeler wordt.

Wat implementatie betreft, ontwerp je functies om één ding goed te doen. Streef naar functies met een duidelijk doel en een naam die dit weerspiegelt. Let op het aantal parameters - te veel en je doet waarschijnlijk te veel. Foutafhandeling? Overweeg het gebruik van werpende functies en ga problemen gracieus aan. Onthoud: Swift draait allemaal om leesbaarheid en gemak van onderhoud.

## Zie Ook
- [Swift Programmeertaal Gids - Functies](https://docs.swift.org/swift-book/LanguageGuide/Functions.html)
- [Ray Wenderlich's Swift Stijlgids](https://github.com/raywenderlich/swift-style-guide)
- [Martin Fowler's Refactoring: Het verbeteren van het ontwerp van bestaande code](https://martinfowler.com/books/refactoring.html)
