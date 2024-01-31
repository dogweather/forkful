---
title:                "Karakters verwijderen die overeenkomen met een patroon"
date:                  2024-01-28T21:59:06.937061-07:00
model:                 gpt-4-0125-preview
simple_title:         "Karakters verwijderen die overeenkomen met een patroon"

category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/nl/swift/deleting-characters-matching-a-pattern.md"
changelog:
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## Wat & Waarom?

Het verwijderen van tekens die overeenkomen met een patroon betekent specifieke reeksen tekens uit een tekenreeks halen op basis van een gedefinieerd patroon, zoals cijfers of interpunctie. Programmeurs doen dit om invoer te zuiveren, gegevens op te schonen of voor te bereiden op verwerking waar specifieke patronen niet nodig zijn.

## Hoe te doen:

```swift
import Foundation

// Voorbeeld: Alle cijfers uit een String verwijderen
let originalString = "Contacteer mij op 123-456-7890 na 09:00 PM."
let digitsPattern = "[0-9]"
let resultString = originalString.replacingOccurrences(of: digitsPattern, with: "", options: .regularExpression)

print(resultString)  // Uitvoer: "Contacteer mij op -- na : PM."
```

```swift
// Voorbeeld: Niet-alfanumerieke karakters verwijderen
let messyString = "H3!llo, W%@rld-"
let nonAlphanumericPattern = "[^A-Za-z0-9]"
let cleanString = messyString.replacingOccurrences(of: nonAlphanumericPattern, with: "", options: .regularExpression)

print(cleanString)  // Uitvoer: "H3lloWrld"
```

## Diepgaande duik

Voor Swift en moderne programmering was pattern matching het domein van specifieke tools en talen zoals `sed`, `awk`, of Perl, bekend om hun mogelijkheden voor tekstverwerking. Swift, met zijn robuuste Foundation-framework, vereenvoudigt deze taken binnen de taal, waardoor het toegankelijker wordt voor ontwikkelaars.

Een alternatief voor reguliere expressies is het itereren door de string met behulp van Swift’s `filter` methode gekoppeld aan een aangepaste voorwaarde, wat ook tijdrovend en minder leesbaar kan zijn. Reguliere expressies bieden een compacte, zij het soms cryptische, manier om het patroon dat we willen verwijderen of manipuleren te beschrijven.

Achter de schermen, wanneer je `replacingOccurrences(of:with:options:)` uitvoert met de optie `.regularExpression`, gebruikt Swift ICU's (International Components for Unicode) reguliere expressiemotor om het patroon te verwerken. ICU is een volwassen, veelgebruikte bibliotheek voor Unicode-ondersteuning, inclusief patroonmatching, die is ingebouwd in veel high-level programmeertalen.

## Zie ook

- Swift String Documentatie: https://developer.apple.com/documentation/swift/string
- Swift Reguliere Expressies: https://developer.apple.com/documentation/foundation/nsregularexpression
- ICU Gebruikershandleiding voor Reguliere Expressies: https://unicode-org.github.io/icu/userguide/strings/regexp.html
