---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:55:35.073632-07:00
description: 'Hoe te: Swift maakt het capitaliseren van strings eenvoudig. Hier is
  een snelle rondleiding.'
lastmod: '2024-03-13T22:44:51.138903-06:00'
model: gpt-4-0125-preview
summary: Swift maakt het capitaliseren van strings eenvoudig.
title: Een string met hoofdletters maken
weight: 2
---

## Hoe te:
Swift maakt het capitaliseren van strings eenvoudig. Hier is een snelle rondleiding:

```Swift
let lowercasedString = "hallo, wereld!"
let titleCased = lowercasedString.capitalized // "Hallo, Wereld!"
let uppercasedString = lowercasedString.uppercased() // "HALLO, WERELD!"

// Voorbeelduitvoer:
print(titleCased)  // Print "Hallo, Wereld!"
print(uppercasedString)  // Print "HALLO, WERELD!"
```

Voor meer controle, zullen we spelen met de `Locale`:

```Swift
let sentence = "de snelle bruine vos"
let titleCasedWithLocale = sentence.capitalized(with: Locale(identifier: "nl_NL"))
// "De Snelle Bruine Vos"

// Voorbeelduitvoer:
print(titleCasedWithLocale)  // Print "De Snelle Bruine Vos"
```

## Diepere Duik
Capitalisatie in programmering bestaat al zolang we digitale tekstverwerking hebben - het gaat allemaal om het voldoen aan de verwachtingen van de gebruiker. Terwijl `capitalized` in Swift strings standaardiseert naar Title Case, waarbij het eerste karakter van elk woord een hoofdletter is, zijn er nuances.

Historisch gezien hadden programmeurs aangepaste methodes nodig om te capitaliseren, waarbij ze zelf randgevallen moesten afhandelen. Swift’s `capitalized` houdt rekening met een locale, wat belangrijk is voor eigennamen of locatie-specifieke hoofdlettergebruik regels.

Over alternatieven gesproken, diegenen die niet tevreden zijn met `capitalized`, wenden zich vaak tot regex of schrijven extensies op `String` voor meer complexe regels. Wat implementatie betreft, is `capitalized` in wezen een ingebouwde methode die door de string loopt, hoofdletters toepassend op de eerste letter na een niet-letterkarakter.

```Swift
extension String {
    func customCapitalized() -> String {
        return self.lowercased().replacingOccurrences(of: "\\b\\w", with: { 
            guard let firstChar = $0.first else { return $0 }
            return String(firstChar).uppercased() + $0.dropFirst()
        }, options: .regularExpression)
    }
}
```

De bovenstaande extensie gebruikt een reguliere expressie om de eerste letter van elk woord te capitaliseren.

## Zie Ook
Voor een diepere duik in Swift string manipulatie, hier zijn enkele nuttige bronnen:
- [Swift Documentatie over Strings](https://developer.apple.com/documentation/swift/string)
- [Ray Wenderlich's String Tutorial voor Swift](https://www.raywenderlich.com/5492-working-with-strings-in-swift)
