---
title:                "Konvertering av en dato til en streng."
html_title:           "Swift: Konvertering av en dato til en streng."
simple_title:         "Konvertering av en dato til en streng."
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor
Det å konvertere en dato til en streng kan være nyttig i mange ulike situasjoner. For eksempel når man skal vise datoer til brukere i en app, eller når man skal lagre datoer på en spesifikk måte i en database. Ved å konvertere datoen til en streng, kan man gi den et lesevennlig format som er enklere å tolke for både brukere og maskiner.

## Slik gjør du det
For å konvertere en dato til en streng i Swift kan du bruke funksjonen `DateFormatter()`. Først må du definere hvordan du ønsker å formatere datoen ved å opprette en `DateFormatter`-instans og sette ønsket format:

```Swift
let formatter = DateFormatter()
formatter.dateFormat = "dd.MM.yy"
```

Deretter kan du bruke denne instansen til å konvertere en dato til en streng ved å kalle på `string(from:)`-metoden og gi den en `Date`-parameter:

```Swift
let currentDate = Date()
let stringFromDate = formatter.string(from: currentDate)
```

Dette vil resultere i at `stringFromDate` blir en streng som inneholder dagens dato i formatet "dd.MM.yy". Du kan også sette andre formateringsalternativer som f.eks. dato og klokkeslett, månedsnavn i stedet for tall osv. For mer informasjon om formateringsalternativer kan du sjekke ut dokumentasjonen til `DateFormatter()`.

## Dykk dypere
Når man konverterer en dato til en streng, er det viktig å være klar over at datoen vil bli konvertert til en lokal tidssone basert på enhetens innstillinger. Dette betyr at datoen kan se annerledes ut for ulike brukere, avhengig av hvor de befinner seg. Om man ønsker å konvertere datoen til en spesifikk tidssone, kan man bruke `TimeZone`-klassen sammen med `DateFormatter`.

Et annet viktig poeng er at noen av formateringsalternativene til `DateFormatter` ikke er tilgjengelige på alle operativsystemer og enheter. Det kan derfor være lurt å både teste ut og håndtere ulike formater for å sikre at appen fungerer som den skal på alle enheter.

## Se også
- [Apple Developer Documentation - DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [Stack Overflow - How to convert Date to String in Swift](https://stackoverflow.com/questions/27067243/how-to-convert-date-to-string-in-swift)