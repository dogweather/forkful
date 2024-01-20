---
title:                "Tolke en dato fra en streng"
html_title:           "Bash: Tolke en dato fra en streng"
simple_title:         "Tolke en dato fra en streng"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å parse en dato fra en streng handler om å konvertere en tekstrepresentasjon av en dato til en faktisk datoobjekt. Dette gjøres for at programmerere skal kunne manipulere og utføre operasjoner på datoen, som ikke ville vært mulig med en enkel tekststreng.

## Hvordan gjøre det:

Her er et Swift-eksempel på hvordan du kan parse en datostreng:

```Swift
import Foundation

let dateString = "2021-09-15T17:30:00Z"
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy-MM-dd'T'HH:mm:ssZ"
let date = dateFormatter.date(from: dateString)

print(date)
```

Hvis du kjører koden, vil dette være output:

```Swift
Optional(2021-09-15 17:30:00 +0000)
```

## Dypdykk:

Parsing av datostrenger har en lang historie, da dette er en vanlig operasjon i mange programmeringsspråk. Historisk sett, har forskjellige språk og biblioteker benyttet forskjellige metoder og formater for å parse datostrenger, noe som kan skape forvirring og inkonsistens.

Det finnes flere alternative måter å parse datostrenger på i Swift, inkludert bruk av `DateComponents` eller `ISO8601DateFormatter` avhengig av strengformatet.

Når du parser en datostreng i Swift, er det viktig å være klar over enkelte detaljer. DateFormatter tar hensyn til både tidsstempel og tidssone, så sørg for at datoformatet ditt matcher strengen du prøver å parse. Hvis det ikke gjør det, returnerer metoden `nil`.

## Se Også:

Du kan lese mer om date parsing og relaterte emner på følgende lenker:
- [Apple's Date and Time Programming Guide](https://developer.apple.com/documentation/foundation/date_and_time_programming/) 
- [Swift Date Formats From String](https://nsdateformatter.com/)
- [Parsing ISO-8601 DateTime with Swift](https://useyourloaf.com/blog/parsing-iso-8601-dates-with-swift/)