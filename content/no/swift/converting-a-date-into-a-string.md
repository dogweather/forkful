---
title:                "Konvertere en dato til en streng"
html_title:           "Arduino: Konvertere en dato til en streng"
simple_title:         "Konvertere en dato til en streng"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Konvertering av en dato til en streng er prosessen hvor vi omgjør ett Date objekt til en String. Vi gjør dette for å gjøre datoer lettere å vise og forstå for brukere.

## Hvordan gjør vi det:

```Swift
import Foundation
let dato = Date()

let formatter = DateFormatter()
formatter.dateFormat = "dd.MM.yyyy"

let strengDato = formatter.string(from: dato)
print(strengDato)
```
Når du kjører dette eksemplet vil du se den siste dagen satt til datoen i følgende format: "dd.MM.yyyy".

## Dypdykk

1. Historisk kontekst: Swift begynte å bruke Date og DateFormatter klassene fra grunnlag av. Disse klassene er arvet fra Objective-C, det tidligere primære programmeringsspråket for Apple-utviklere.

2. Alternativer: Istedenfor å bruke en datoformat, kan du også bruke Date sin `description` funksjon for å få en strengrepresentasjon av datoen. Men, dette vil gi deg en UTC dato og tid, hvilket er ikke alltid det du ønsker.

3. Implementering: `DateFormatter` er en kraftig klasse som lar deg konfigurere hvordan du vil at datoen skal se ut. Du kan sette dato- og tidsstiler, lokaliseringsinnstillinger, tidssone og mer.

## Se Også

For mer informasjon sjekk ut disse linkene:

- Apple sin dokumentasjon på [Date](https://developer.apple.com/documentation/foundation/date) og [DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- Swift sin offisielle [String interpolasjon av datoer](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Stack Overflow diskusjon om [beste metode for å formatere datoer](https://stackoverflow.com/questions/35700281/date-format-in-swift) i Swift.
- Tutorial på [hvordan arbeide med datoer og tider](https://www.hackingwithswift.com/articles/141/8-useful-swift-extensions) i Swift.