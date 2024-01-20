---
title:                "Interpolering av en streng"
html_title:           "Bash: Interpolering av en streng"
simple_title:         "Interpolering av en streng"
programming_language: "Swift"
category:             "Swift"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/interpolating-a-string.md"
---

{{< edit_this_page >}}

---

## Hva & Hvorfor?

Stringinterpolering er prosessen å sette variabler eller uttrykk inn i en streng. Programmerere bruker det for å gjøre koden lettere å lese og for å samle og presentere data på en enkel måte.

---

## Hvordan gjøre det: 

Her er et enkelt eksempel på string-interpolering i Swift:

```swift
var brukerNavn = "Ola"
print("Hei, \(brukerNavn)!")
```

Utskriften vil være: 

```
Hei, Ola!
```

Du kan også legge inn matematiske uttrykk direkte i stringen:

```swift
print("Fire ganger fire blir \(4 * 4)")
```

Utskriften vil være: 

```
Fire ganger fire blir 16
```

---

## Dypdykk

Swift ble introdusert i 2014 av Apple, som en forbedring fra Objective-C, men med metoder for stringinterpolering inspirert av andre moderne programmeringsspråk som Ruby.

Et alternativ til stringinterpolering ville være traditionelle metoder for sammenføyning av strenger, men disse er ofte mindre leselige og mer tidkrevende å skrive.

Når det kommer til implementasjonsdetaljer, konverterer Swift de interpolerte delene av strengen til den riktige stringify-representasjonen først og deretter setter sammen de fullførte strengene.

---

## Se også:

- Swift dokumentasjon om String interpolation: [Hvis du vil lære mer](https://docs.swift.org/swift-book/LanguageGuide/StringsAndCharacters.html)
- Andre moderne bruk av stringinterpolering: [Besøk denne linken](https://www.hackingwithswift.com/read/0/5/string-interpolation)