---
title:                "Swift: Sletting av tegn som matcher et mønster"
simple_title:         "Sletting av tegn som matcher et mønster"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

I denne bloggposten skal vi utforske et vanlig problem i Swift-programmering - hvordan å slette tegn som matcher et bestemt mønster. Dette kan være nyttig når du arbeider med strenger og trenger å fjerne uønskede tegn.

## Hvordan

For å slette tegn som matcher et mønster i Swift, kan du bruke metoden `replacingOccurrences(of:with:)` på en streng. Denne metoden tar to parametere - mønsteret du vil matche og den nye strengen du vil erstatte den matchende delen med.

La oss si at vi har en streng med navn og ønsker å slette alle mellomrommene i navnet. Vi kan bruke følgende kode:

```Swift
let navn = "Mari Olsen"
let nyttNavn = navn.replacingOccurrences(of: " ", with: "")
print(nyttNavn) // Output: MariOlsen
```

I dette eksempelet brukte vi metoden til å erstatte alle mellomrom med en tom streng, dermed sletter vi dem. Dette kan også gjøres med andre karakterer og mønstre, avhengig av hva du trenger å fjerne.

## Dypdykk

Når du bruker metoden `replacingOccurrences`, er det viktig å vite at den ikke endrer den opprinnelige strengen, men returnerer en ny, modifisert kopi. Dette betyr at hvis du vil beholde den opprinnelige strengen, må du tilordne den nye verdien til en ny variabel. Ellers vil den opprinnelige strengen forbli uendret.

I tillegg kan du bruke denne metoden til å erstatte flere mønstre på en gang ved å bruke en `for`-loop og en dictionary med alle mønstrene og deres tilsvarende erstatning. Dette kan være nyttig når du jobber med komplekse strenger og trenger å utføre flere endringer på en gang.

## Se også
- [Swift Strings](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- [Working with Strings in Swift](https://www.hackingwithswift.com/articles/141/working-with-strings-in-swift)
- [Regular Expressions in Swift](https://www.raywenderlich.com/86205/nsregularexpression-swift-tutorial)

Takk for at du leste denne bloggposten om hvordan å slette tegn som matcher et mønster i Swift. Jeg håper det var nyttig og husk å bruke denne kunnskapen for å forbedre kodingen din!