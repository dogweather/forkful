---
title:                "Een string interpoleren"
date:                  2024-01-28T22:02:44.359274-07:00
model:                 gpt-4-0125-preview
simple_title:         "Een string interpoleren"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/swift/interpolating-a-string.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?
Het interpoleren van een string houdt in dat je variabelen in een letterlijke string invoegt. Programmeurs doen dit om dynamisch strings te construeren, waardoor het gemakkelijker wordt om variabele gegevens in de output op te nemen.

## Hoe:
Swift maakt stringinterpolatie een fluitje van een cent met de syntax `\(variabeleNaam)`.

```Swift
let naam = "Jane"
let leeftijd = 28
let begroeting = "Hallo, \(naam), je bent \(leeftijd) jaar oud."
print(begroeting)  // Uitvoer: Hallo, Jane, je bent 28 jaar oud.
```

Je kunt zelfs operaties uitvoeren binnen de interpolatie:

```Swift
let appels = 3
let sinaasappels = 5
let fruitSamenvatting = "Ik heb \(appels + sinaasappels) stuks fruit."
print(fruitSamenvatting)  // Uitvoer: Ik heb 8 stuks fruit.
```

## Diepere Duik
Okay, laten we een beetje historisch worden. Stringinterpolatie is niet uniek voor Swift. Het bestaat in veel talen (zoals JavaScript, Python, etc.), maar de versie van Swift is type-veilig wat betekent dat de compiler de typen voor je controleert, waardoor fouten worden verminderd.

Voor Swift 5 was stringinterpolatie minder krachtig en omslachtiger. Maar Swift 5 introduceerde Uitgebreide Stringinterpolatie, waardoor je stringinterpolatie kunt aanpassen, wat indrukwekkende flexibiliteit biedt.

Alternatieven voor stringinterpolatie in Swift omvatten concatenatie met `+`, en de oudere `String(format:)` methode. Echter, deze zijn minder handig en, voor format strings, moeilijker te lezen.

Implementatiedetails? Met Swift's stringinterpolatie kun je aanpassen hoe typen worden weergegeven binnen strings door het `StringInterpolation` protocol uit te breiden. Dit betekent dat je kunt definiëren hoe aangepaste typen worden weergegeven tijdens interpolatie, wat super handig is.

```Swift
extension String.StringInterpolation {
    mutating func appendInterpolation(_ waarde: Date) {
        let formatter = DateFormatter()
        formatter.dateStyle = .medium
        appendLiteral(formatter.string(from: waarde))
    }
}

let vandaag = Date()
let datumString = "De datum van vandaag is \(vandaag)."
print(datumString) // Uitvoer zal de datum van vandaag zijn in middelmatige stijlformattering.
```

## Zie Ook
Voor de fijne kneepjes over stringinterpolatie, Swift's documentatie is goud waard:
- [String Interpolation](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID292)
- [Swift Evolution Voorstel voor Verbeterde String Interpolatie](https://github.com/apple/swift-evolution/blob/main/proposals/0228-fix-expressiblebystringinterpolation.md)

Voor dieper duiken in het formatteren van aangepaste typen:
- [Aanpassen van String Interpolatie in Swift](https://www.hackingwithswift.com/articles/178/super-powered-string-interpolation-in-swift-5)
