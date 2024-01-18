---
title:                "Analysering av dato fra en streng"
html_title:           "Swift: Analysering av dato fra en streng"
simple_title:         "Analysering av dato fra en streng"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva og hvorfor?
Å parsere en dato fra en streng er en prosess der et programmeringsprogram tar en dato, skrevet som en streng med tall og symboler, og konverterer den til et format som kan behandles av programmet. Dette gjøres vanligvis for å kunne bruke datoen til å utføre spesifikke oppgaver i koden.

## Slik gjør du det:
For å parse en dato fra en streng i Swift kan du bruke ```DateFormatter``` klassen. Først må du spesifisere formatet som datoen er skrevet i, for eksempel "dd-MM-yyyy" for en dato skrevet i formatet dag-måned-år. Deretter kan du bruke ```date(from: String)``` metoden for å konvertere strengen til en ```Date```-instans. Se eksempelet under:

```
// Definer datoformatet
let format = "dd-MM-yyyy"

// Opprett en DateFormatter-instans
let formatter = DateFormatter()

// Konfigurer formatteren med datoformatet
formatter.dateFormat = format

// Parse datoen fra en streng
let dateString = "31-12-2020"
let date = formatter.date(from: dateString)

// Skriv ut datoen
print(date)
// Output: Optional(2020-12-31 00:00:00 +0000)
```

## Dykk dypere:
Å kunne parsere datoer fra strenger har vært en viktig del av programmering siden tidlig på 1900-tallet. I tillegg til å bruke ```DateFormatter``` kan du også bruke andre metoder, som å bruke regex-uttrykk eller en tredjeparts bibliotek som "NSDateFormatterExtensions". Det kan også være nyttig å lese dokumentasjonen for ```DateFormatter```-klassen for å lære mer om de forskjellige formatene og mulighetene for formatering.

## Se også:
- [Apple Developer - DateFormatter Documentation](https://developer.apple.com/documentation/foundation/dateformatter)
- [Swift by Sundell - Working With Dates in Swift](https://www.swiftbysundell.com/basics/dates/)
- [NSHipster - DateFormatter](https://nshipster.com/nsformatter/)