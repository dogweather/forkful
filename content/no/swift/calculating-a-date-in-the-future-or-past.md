---
title:                "Kalkulering av en dato i fremtiden eller fortiden"
html_title:           "Swift: Kalkulering av en dato i fremtiden eller fortiden"
simple_title:         "Kalkulering av en dato i fremtiden eller fortiden"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor

Det kan være mange grunner til å ønske å beregne en dato i fremtiden eller fortiden. Det kan være for å planlegge en hendelse, følge opp tidsfrister eller bare for å tilpasse en kalender eller app. Uansett årsak, kan det være nyttig å vite hvordan man utfører denne beregningen ved hjelp av Swift-programmeringsspråket.

## Hvordan

```Swift
let today = Date() // Oppretter en variabel for dagens dato
var dateComponent = DateComponents() // Oppretter en variabel for å endre dato på
dateComponent.day = 7 // Angir antall dager som skal legges til eller trekkes fra
// ved å endre tallet på "day" kan man endre dato på ulike måter, for eksempel "month" for å endre måned eller "year" for å endre år

let futureDate = Calendar.current.date(byAdding: dateComponent, to: today) // Beregner datoen i fremtiden
let pastDate = Calendar.current.date(byAdding: -dateComponent, to: today) // Beregner datoen i fortiden

print("Datoen i fremtiden: \(futureDate!)") // Printer ut datoen i fremtiden
print("Datoen i fortiden: \(pastDate!)") // Printer ut datoen i fortiden
```

Output:

```
Datoen i fremtiden: 2021-06-16 10:00:00 +0000 // Avhengig av når koden blir utført
Datoen i fortiden: 2021-06-02 10:00:00 +0000 // Avhengig av når koden blir utført
```

## Dyp dykk

Swift har en innebygd klasse kalt "DateComponents" som lar deg manipulere datoer ved å legge til eller trekke fra dager, måneder, år osv. Dette, sammen med "Calendar.current.date", lar deg beregne datoer i fremtiden eller fortiden på en enkel måte. Det finnes også mange andre funksjoner og metoder i Swift for å håndtere datoer, som kan være nyttige å utforske for mer avanserte beregninger.

## Se også

- [Swift: How to add and subtract days from a date](https://www.hackingwithswift.com/example-code/language/how-to-add-and-subtract-dates)
- [Working with Dates and Time in Swift](https://developer.apple.com/documentation/foundation/date)
- [Using Date Components to Easily Work with Dates in Swift](https://medium.com/better-programming/using-date-components-to-easily-work-with-dates-in-swift-a31f172b088a)