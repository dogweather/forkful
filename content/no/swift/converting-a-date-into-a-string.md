---
title:                "Konvertering av dato til streng"
html_title:           "Swift: Konvertering av dato til streng"
simple_title:         "Konvertering av dato til streng"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Konvertering av et datoobjekt til en streng er en vanlig oppgave i programmering. Dette gjøres for å kunne vise datoer på en mer leselig og forståelig måte for brukeren. 

## Slik gjør du det:
Det er flere forskjellige måter å konvertere et datoobjekt til en streng i Swift. En av de enkleste måtene er å bruke DateFormatters `string(from: Date)` funksjon. Dette kan gjøres ved å følge følgende kodeeksempel:

```Swift
let date = Date()
let formatter = DateFormatter()
formatter.dateFormat = "dd.MM.yyyy"
let dateString = formatter.string(from: date)
print(dateString) // Output: 07.09.2021
```

## Dypdykk:
Konvertering av dato til en streng er en vanlig praksis i programmering og har vært en del av språket Swift siden den første versjonen ble utgitt i 2014. Det finnes flere alternativer for å konvertere datoer som for eksempel ved hjelp av `Calendar` og `NSCalendar` klassene. Det er også mulig å definere egne formattere for å konvertere datoer til spesifikke formater. Det anbefales å lese mer om disse alternativene for å finne den beste løsningen for akkurat ditt behov. 

## Se også:
- [Apple's offisielle dokumentasjon om DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [En grundig guide til konvertering av dato til streng i Swift](https://www.hackingwithswift.com/example-code/system/how-to-convert-dates-and-times-to-a-string-using-dateformatter)