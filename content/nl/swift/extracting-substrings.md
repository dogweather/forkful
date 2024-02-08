---
title:                "Substrings extraheren"
aliases:
- nl/swift/extracting-substrings.md
date:                  2024-01-28T22:00:27.970541-07:00
model:                 gpt-4-0125-preview
simple_title:         "Substrings extraheren"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/swift/extracting-substrings.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het extraheren van substrings betekent dat je slechts een deel van een string pakt—alsof je een lint knipt tot de lengte die je nodig hebt. Programmeurs doen dit om specifieke stukjes tekstgegevens te isoleren, analyseren of manipuleren, zoals gebruikersinvoer, bestandsnamen of tekstverwerking.

## Hoe:

Swift maakt het vrij eenvoudig om met substrings te werken. Laten we er direct induiken met enkele voorbeelden.

```swift
let fullString = "Hallo, Swift Programmer!"
let startIndex = fullString.index(fullString.startIndex, offsetBy: 7)
let endIndex = fullString.index(fullString.startIndex, offsetBy: 12)

// Een substring extraheren met String.Index
let substring = fullString[startIndex...endIndex]

print(substring) // "Swift"

// Een andere manier, met NSRange en NSString
import Foundation

let nsRange = NSRange(location: 7, length: 6)
if let range = Range(nsRange, in: fullString) {
    let substring = fullString[range]
    print(substring) // "Swift"
}

// Korte weg, als je de exacte indices kent
let quickSubstring = fullString[7...12]

print(quickSubstring) // Dit zal een foutmelding geven omdat Swift strings geen integerindexering ondersteunen
```

Uitvoer:
```
Swift
Swift
// Fout: 'subscript(_:)' is niet beschikbaar: kan String niet indexeren met een Int, zie de documentatie voor String voor meer informatie
```

## Diep duiken

Substrings extraheren in Swift houdt in dat je begrijpt hoe Swift strings behandelt, wat een beetje anders is dan talen zoals Python of C#. In Swift zijn strings verzamelingen van karakters die geen integerindexen gebruiken. Dit komt voort uit de ondersteuning van Swift voor Unicode-conforme karakters, waardoor strings niet een vaste lengte hebben, maar eerder een verzameling van grafeme clusters (wat een gebruiker ziet als een enkel karakter).

Dit ontwerp betekent dat directe integer-subscriptie niet werkt met Swift-strings; je moet werken met `String.Index`. Hoewel het niet zo onmiddellijk intuïtief is als het gebruik van integers, behandelt het verschillende tekstscripts en emoji consequent.

Alternatieven omvatten het gebruik van `NSString` uit Objective-C, zoals getoond in de voorbeelden, wat NSRange toelaat, maar dat is een beetje ouderwets en niet Swift-achtig. Sinds Swift 4 heeft String echter veel liefde gekregen, met rijkere, intuïtievere API-opties om met substrings te werken, waardoor `NSString` voor de meeste taken in het stof blijft liggen.

Implementatiedetails zijn cruciaal—naïeve substring-extractie kan leiden tot prestatieproblemen, omdat elke oproep aan `index(_: offsetBy:)` O(n) kan zijn bij het omgaan met Unicode-conforme strings. Bovendien, wanneer je een substring in Swift maakt, deelt deze het geheugen van de oorspronkelijke string, wat efficiënt is, maar iets om rekening mee te houden als je later de oorspronkelijke string muteert.

## Zie ook

Voor meer over dit onderwerp, raadpleeg de officiële documentatie:

- Swift String en Karakters: [https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- String Programmeringshandleiding: [https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/Strings/](https://developer.apple.com/library/archive/documentation/Cocoa/Conceptual/Strings/)

Ga aan de slag met oefenen en speel rond in een Swift-playground om het echt onder de knie te krijgen.
