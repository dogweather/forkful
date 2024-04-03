---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 22:09:44.356476-07:00
description: "Reguliere expressies, of regex, zijn patronen die gebruikt worden om\
  \ combinaties van karakters in strings te matchen. Programmeurs gebruiken ze voor\
  \ het\u2026"
lastmod: '2024-03-13T22:44:51.146289-06:00'
model: gpt-4-0125-preview
summary: Reguliere expressies, of regex, zijn patronen die gebruikt worden om combinaties
  van karakters in strings te matchen.
title: Reguliere expressies gebruiken
weight: 11
---

## Hoe?
In Swift gebruik je de `NSRegularExpression` klasse om met regex om te gaan. Je definieert een patroon, maakt een regex-object aan en gebruikt het vervolgens om tekst te zoeken of te vervangen. Hier is een basisvoorbeeld:

```Swift
import Foundation

let input = "Bel me op 555-1234 of 555-5678."
let pattern = "\\d{3}-\\d{4}" // Matcht een patroon zoals 555-1234

do {
    let regex = try NSRegularExpression(pattern: pattern)
    let matches = regex.matches(in: input, range: NSRange(input.startIndex..., in: input))
    
    for match in matches {
        if let range = Range(match.range, in: input) {
            let telefoonnummer = String(input[range])
            print("Gevonden telefoonnummer: \(telefoonnummer)")
        }
    }
} catch {
    print("Regex fout: \(error.localizedDescription)")
}
```

Voorbeelduitvoer:
```
Gevonden telefoonnummer: 555-1234
Gevonden telefoonnummer: 555-5678
```

## Diepere duik
Regex bestaat sinds de jaren 1950, met zijn oorsprong in de formele taaltheorie en is breed gebruikt in Unix-tools. In Swift gebruiken we de `NSRegularExpression` klasse, die is geërfd van Objective-C, en die vertrouwt op de ICU-bibliotheek voor Unicode-ondersteuning.

Alternatieven voor regex in Swift omvatten het gebruik van `String`'s `contains`, `split`, of `range(of:)` methoden voor eenvoudige gevallen. Voor complexere patroonmatching biedt Swift geen ingebouwde alternatieven voor regex.

Bij het implementeren van regex is het cruciaal om het patroon te optimaliseren om trage zoekopdrachten te vermijden, vooral bij grote tekstlichamen. Daarnaast, onthoud dat regex-operaties uitzonderingen kunnen veroorzaken, dus behandel ze altijd met `try-catch` blokken.

## Zie ook
- [NSRegularExpression Documentatie](https://developer.apple.com/documentation/foundation/nsregularexpression)
- [Swift String Documentatie](https://developer.apple.com/documentation/swift/string)
- [Ray Wenderlich's Gids voor NSRegularExpression in Swift](https://www.raywenderlich.com/2725-nsregularexpression-tutorial-and-cheat-sheet)
