---
title:                "Swift: Å få den nåværende datoen"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor 

Det er en vanlig oppgave for programmerere å ønske å hente den nåværende datoen og klokkeslettet i en applikasjon. Dette kan være nyttig for å vise aktuell informasjon, lage tidsstempler eller generelt for å holde styr på tiden. Uansett hva målet ditt er, kan det være nyttig å ha kunnskap om hvordan man henter den nåværende datoen i Swift.

## Slik gjør du det

For å få tilgang til den nåværende datoen i Swift, kan du bruke `Date`-strukturen og `DateFormatter`-klassen. Her er et lite eksempel på hvordan du kan få tak i den nåværende datoen og konvertere den til en lesbar streng:

```Swift
// Henter den nåværende datoen
let nå = Date()

// Opprett formatteringsobjekt
let formatter = DateFormatter()

// Setter ønsket format
formatter.dateFormat = "dd.MM.yyyy"

// Konverterer datoen til en streng
let datoStreng = formatter.string(from: nå)

// Skriver ut resultatet
print(datoStreng)

// Output: 21.08.2021
```

Dette er bare et grunnleggende eksempel, men det viser hvordan du kan bruke `Date` og `DateFormatter` til å få tak i den nåværende datoen og formatere den slik du ønsker.

## Dypdykk

Hvis du ønsker å lære mer om hvordan `Date`-strukturen og `DateFormatter`-klassen fungerer, kan du se på dokumentasjonen fra Apple. Det er også verdt å merke seg at disse klassene støtter internasjonalisering, noe som betyr at du kan formatere datoen på forskjellige måter avhengig av hvilket språk eller land du er i.

## Se også

- [Apple dokumentasjon for Date og DateFormatter](https://developer.apple.com/documentation/foundation/date)
- [Om internasjonalisering i Swift](https://developer.apple.com/library/archive/documentation/MacOSX/Conceptual/BPInternational/StringsdictFileFormat/StringsdictFileFormat.html)