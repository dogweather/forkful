---
title:                "Få nåværende dato"
html_title:           "Swift: Få nåværende dato"
simple_title:         "Få nåværende dato"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor
Det å få den nåværende datoen kan være nyttig for å informere brukere om når noe ble opprettet, oppdatert eller slettet. Det kan også være praktisk for å planlegge framtidige hendelser eller for å lage dynamiske tidsstempel for logger.

## Slik gjør du det
For å få den nåværende datoen i Swift, kan du bruke Date() -klassen og DateFormatter() -objektet. Først må du importere Foundation-biblioteket i din Swift-fil:

```Swift
import Foundation
```
Deretter kan du definere en DateFormatter og sette et ønsket datoformat:

```Swift
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd/MM/yyyy"
```

Nå kan du bruke Date() -klassen for å få den nåværende datoen, og formattere den med DateFormatter:

```Swift
let currentDate = Date()
let formattedDate = dateFormatter.string(from: currentDate)
print(formattedDate) // Output: 05/10/2021
```
Hvis du ønsker å få den nåværende dato og klokkeslett, kan du bruke et annet datoformat:

```Swift
dateFormatter.dateFormat = "dd/MM/yyyy HH:mm:ss"
let formattedDateTime = dateFormatter.string(from: currentDate)
print(formattedDateTime) // Output: 05/10/2021 11:34:21
```

## Dypdykk
I tillegg til å få den nåværende datoen, kan du også gjøre ulike operasjoner med datoen. For eksempel kan du bruke DateComponents() -objektet for å få tilgang til ulike deler av datoen, som måned, år, dag osv.

```Swift
let dateComponents = Calendar.current.dateComponents([.year, .month, .day], from: currentDate)
print(dateComponents.year) // Output: 2021
print(dateComponents.month) // Output: 10
print(dateComponents.day) // Output: 5
```

Du kan også gjøre beregninger med datoer ved å bruke Calendar() -klassen og DateComponents. For eksempel kan du legge til eller trekke fra en viss mengde tid fra den nåværende datoen.

```Swift
var dateComponent = DateComponents()
dateComponent.month = 2
let newDate = Calendar.current.date(byAdding: dateComponent, to: currentDate)
print(newDate) // Output: Optional(2021-12-05 10:34:21 +0000)
```

Det finnes også andre nyttige metoder for å arbeide med datoer, som for eksempel å finne forskjellen mellom to datoer, teste om en dato er før eller etter en annen, eller om en dato faller innenfor et bestemt tidsintervall. For å lære mer, kan du se dokumentasjonen for Date() og Calendar() -klassene.

## Se også
- [Apple Dokumentasjon: Date](https://developer.apple.com/documentation/foundation/date)
- [Apple Dokumentasjon: Calendar](https://developer.apple.com/documentation/foundation/calendar)
- [Swift by Sundell: Working with Dates & Times in Swift](https://www.swiftbysundell.com/basics/date-properties/)