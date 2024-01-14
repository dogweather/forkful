---
title:    "Swift: Sammenligning av to datoer"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

Å sammenligne to datoer er en viktig del av Swift programmering. Det kan hjelpe deg med å organisere og sortere data, samt utføre forskjellige operasjoner basert på datoer. Å forstå hvordan man sammenligner datoer kan være avgjørende for å skrive effektiv og nøyaktig kode.

## Hvordan

For å sammenligne to datoer i Swift, må du bruke `Date` og `Calendar` klassene. Først må du definere de to datoene du ønsker å sammenligne som `Date` objekter. Deretter må du bruke `Calendar` klassen til å utføre sammenligningen ved å bruke metodene `compare()` eller `isDate()`.

```Swift
let date1 = Date()
let date2 = Date()

let calendar = Calendar.current
let result = calendar.compare(date1, to: date2, toGranularity: .day)

if result == .orderedAscending {
    print("Date 1 er før Date 2")
} else if result == .orderedDescending {
    print("Date 1 er etter Date 2")
} else {
    print("Date 1 og Date 2 er like")
}
```

Output:

```
Date 1 og Date 2 er like
```

Du kan endre `toGranularity` parameteren til `Year`, `Month`, `Day`, `Hour`, `Minute` eller `Second` for å sammenligne på en mer spesifikk nivå.

## Deep Dive

Når du sammenligner datoer, er det viktig å være oppmerksom på tidssoner. Datomodellen i Swift bruker UTC (Coordinated Universal Time) som standard, så hvis du ønsker å sammenligne datoer i en annen tidssone, må du først konvertere dem ved hjelp av `TimeZone` klassen.

En annen viktig faktor er datoforskjellig. Dette kan påvirke resultatet av sammenligningen, spesielt hvis en av datoene har en tidspunkt mens den andre ikke har det. Det er derfor viktig å bruke `timeInterval` parameteren i `compare()` metoden for å inkludere tiden i sammenligningen.

## Se også

- [Dato og klokkeslett håndtering i Swift](https://developer.apple.com/documentation/foundation/date_and_time)
- [Kalender håndtering i Swift](https://developer.apple.com/documentation/foundation/calendar)
- [Tidssone håndtering i Swift](https://developer.apple.com/documentation/foundation/timezone)