---
changelog:
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 21:57:16.275230-07:00
description: "Het samenvoegen van strings is het aan elkaar plakken van afzonderlijke\
  \ strings om een nieuwe te maken. Programmeurs doen dit om tekst dynamisch te\u2026"
lastmod: '2024-03-11T00:14:24.986296-06:00'
model: gpt-4-0125-preview
summary: "Het samenvoegen van strings is het aan elkaar plakken van afzonderlijke\
  \ strings om een nieuwe te maken. Programmeurs doen dit om tekst dynamisch te\u2026"
title: Samenvoegen van strings
---

{{< edit_this_page >}}

## Wat & Waarom?
Het samenvoegen van strings is het aan elkaar plakken van afzonderlijke strings om een nieuwe te maken. Programmeurs doen dit om tekst dynamisch te combineren, zoals het samenstellen van begroetingen, berichten, of gewoon om gegevens op een leesbare manier te structureren.

## Hoe te:
```Swift
let firstName = "Taylor"
let lastName = "Swift"
let fullName = firstName + " " + lastName  // Gebruikmakend van de + operator
print(fullName)  // Uitvoer: "Taylor Swift"

let age = 31
let greeting = "Hallo, \(firstName)! Je bent \(age) jaar oud."  // Gebruikmakend van string interpolatie
print(greeting)  // Uitvoer: "Hallo, Taylor! Je bent 31 jaar oud."

var message = "Dit"
message += " is" // Gebruikmakend van += operator om aan een string toe te voegen
message += " Sparta!"
print(message)  // Uitvoer: "Dit is Sparta!"
```

## Diepere Duik
Lang geleden, moesten mensen in programmeertalen zoals C handmatig strings rondschuiven met functies, omgaand met arrays en null-afgesloten strings. Swift maakt het makkelijk. De '+' operator voor strings komt uit talen zoals Java en C++, en brengt de vertrouwde manier om strings samen te klikken over.

Er zijn opties voorbij de '+'. String interpolatie in Swift gaat niet alleen over fancy zijn – het is een type-veilige manier om waarden rechtstreeks in je string in te bedden. Geen behoefte om types te casten of je zorgen te maken dat je iets verkeerd matcht.

Geavanceerde concatenatie betreft meer dan alleen woorden rond slingeren. Wanneer prestaties belangrijk zijn, kan het roekeloos gebruiken van '+=' je vertragen. Waarom? Omdat als je toevoegt aan een string in een lus, Swift elke keer nieuwe strings kan creëren, wat niet zo snel is. Overweeg in plaats daarvan 'join()' of 'append()' van 'String' te gebruiken voor efficiëntie, vooral bij grote data of complexe lussen.

```Swift
// Efficiënte concatenatie met `join()`
let words = ["Eens", "was", "er", "een"]
let story = words.joined(separator: " ")  // Efficiënt voor het samenvoegen van array-elementen
print(story)  // Uitvoer: "Eens was er een"

// Gebruikmakend van 'append(contentsOf:)' voor het toevoegen van substrings
var quote = "Ik denk, "
quote.append(contentsOf: "dus ik ben")
print(quote)  // Uitvoer: "Ik denk, dus ik ben"
```

## Zie Ook
- Swift Documentatie over Strings: [Swift.org Documentatie](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Apple's String Programmeergids: [Apple Ontwikkelaarsdocumentatie](https://developer.apple.com/documentation/swift/string)
