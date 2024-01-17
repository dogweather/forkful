---
title:                "Sammenslåing av strenger"
html_title:           "Swift: Sammenslåing av strenger"
simple_title:         "Sammenslåing av strenger"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Sammenføyning av strenger er når man kombinerer to eller flere tekststrenger til en lengre tekststreng. Dette er en vanlig operasjon i programmering for å manipulere tekst eller lage utskrifter.

## Hvordan:
```Swift
let firstName = "Sarah"
let lastName = "Smith"
let greeting = "Hei, mitt navn er" + firstName + lastName + "og jeg er en programmerer."

print(greeting) // Utskrift: Hei, mitt navn er Sarah Smith og jeg er en programmerer.
```

## Dypdykk:
Sammenføyning av strenger har vært en grunnleggende operasjon i programmering i lang tid. Før i tiden ble det gjort ved å bruke spesielle funksjoner eller symbolske operatører. I Swift bruker vi pluss-tegnet (+) for å sammenføye to strenger. Alternativene til å sammenføye strenger inkluderer bruk av formatteringsfunksjoner eller maler for å strukturere teksten på en mer effektiv måte. Bak kulissene er det en prosess kalt string interning som gjør at sammenføyning av strenger ikke lager en ny strengobjekt, men i stedet peker til en eksisterende objekt for å spare på ressurser.

## Se også:
https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html#ID291
https://www.appcoda.com/swift-string/
https://developer.apple.com/documentation/swift/string#1669700