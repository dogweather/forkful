---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:58:04.829681-07:00
description: "Een tekenreeks omzetten naar kleine letters betekent dat alle hoofdletters\
  \ worden vervangen door hun kleine letter equivalenten. Programmeurs doen dit\u2026"
lastmod: 2024-02-19 22:05:10.229686
model: gpt-4-0125-preview
summary: "Een tekenreeks omzetten naar kleine letters betekent dat alle hoofdletters\
  \ worden vervangen door hun kleine letter equivalenten. Programmeurs doen dit\u2026"
title: Een string omzetten naar kleine letters
---

{{< edit_this_page >}}

## Wat & Waarom?

Een tekenreeks omzetten naar kleine letters betekent dat alle hoofdletters worden vervangen door hun kleine letter equivalenten. Programmeurs doen dit voor consistentie, vaak voor hoofdletterongevoelige vergelijkingen of het standaardiseren van tekstinput.

## Hoe:

Swift maakt dit eenvoudig met een eigenschap genaamd `lowercased`. Zo gebruik je het:

```Swift
let originalString = "Hello, World!"
let lowercasedString = originalString.lowercased()
print(lowercasedString) // "hello, world!"
```

Voorbeeld van uitvoer:
```
hello, world!
```

## Diepere Duik:

Historisch gezien is het zorgen voor een consistente tekenreeks hoofdlettergebruik cruciaal in programmeren, vooral omdat vroege computers erg hoofdlettergevoelig waren. In Swift is `lowercased()` een methode die beschikbaar is op instanties van het type `String`. Door het aan te roepen, converteer je alle tekens binnen de tekenreeks die kleine letter varianten hebben naar hun kleine lettervormen.

Alternatieven voor `lowercased()` kunnen zijn het handmatig doorlopen van de tekenreeks en elk teken vervangen door zijn kleine letter equivalent door gebruik te maken van een mappingfunctie. Maar eerlijk gezegd, dat is het wiel opnieuw uitvinden.

Het omzetten naar kleine letters heeft enkele nuances. Bijvoorbeeld, de `lowercased()` methode gebruikt de huidige locatie om specifieke taal hoofdletterregels te hanteren, wat niet altijd het gewenste gedrag is. Als je locatie-onafhankelijke conversies moet uitvoeren, kun je terugvallen op `lowercased(with: Locale?)` en `nil` doorgeven als de Locatie:

```Swift
let turkishString = "İstanbul"
let lowercasedTurkishString = turkishString.lowercased(with: nil)
print(lowercasedTurkishString) // "i̇stanbul", correct in Unicode, maar een 'I' zonder punt wordt misschien verwacht in Turkije.
```

De implementatie van `lowercased()` gebruikt onder de motorkap de Unicode standaard die complexe karteringsregels voor tekens in verschillende scripts bevat, waarvan niet allemaal simpelweg een kwestie zijn van 'a' die 'A' vervangt.

## Zie Ook:

Om meer te ontdekken over tekenreeksen en karaktertransformaties in Swift, duik in de volgende bronnen:

- Swift Tekenreeks en Karakters documentatie: [Swift.org](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Unicode hoofdletter mapping details: [Unicode Standard](https://www.unicode.org/reports/tr21/tr21-5.html)
- Een discussie over tekenreeksvergelijking en locatie: [NSHipster Artikel over Locatie](https://nshipster.com/locale/)
