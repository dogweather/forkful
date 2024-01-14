---
title:                "Swift: Sammenligning av to datoer"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Hvorfor

Når man programmerer i Swift, kan man komme over situasjoner der man trenger å sammenligne to datoer. Dette kan være nyttig for å sortere data eller for å sjekke om en dato kommer før eller etter en annen. I denne bloggposten skal vi gå gjennom hvordan man kan gjøre dette på en enkel måte.

# Hvordan

For å sammenligne to datoer i Swift, kan man bruke compare-metoden som er tilgjengelig på Date-objekter. Denne metoden sammenligner to datoer og returnerer en ComparisonResult som kan være .orderedAscending hvis datoen kommer før, .orderedDescending hvis datoen kommer etter eller .orderedSame hvis de to datoene er lik.

```Swift
let firstDate = Date()
let secondDate = Date().addingTimeInterval(3600) // legger til en time til første dato

if firstDate.compare(secondDate) == .orderedAscending {
    print("Første dato kommer før den andre")
} else if firstDate.compare(secondDate) == .orderedDescending {
    print("Første dato kommer etter den andre")
} else {
    print("Datoene er like")
}
// Output: Første dato kommer før den andre
```

Man kan også bruke ==, < og > operatorer for å sammenligne datoer direkte.

```Swift
let thirdDate = Date()
let fourthDate = Date(timeIntervalSinceReferenceDate: 0) // unix epoch dato

if thirdDate < fourthDate {
    print("Første dato kommer før den andre")
} else if thirdDate > fourthDate {
    print("Første dato kommer etter den andre")
} else {
    print("Datoene er like")
}
// Output: Første dato kommer etter den andre
```

# Dypdykk

Det er viktig å merke seg at datoer kan bli påvirket av tidsone og sommertid. Det kan derfor være lurt å konvertere datoer til et ensartet tidsformat før man sammenligner dem for å unngå feilaktige resultater.

Man kan også bruke Calendar-objektet til å få mer detaljert informasjon om datoene man sammenligner, som for eksempel å få ut dag, måned eller år.

```Swift
let today = Date() // gjeldende dato
let tomorrow = Calendar.current.date(byAdding: .day, value: 1, to: today) // legger til en dag til gjeldende dato

let day = Calendar.current.component(.day, from: tomorrow!) // henter ut dag fra morgendatoen

print("Morgendatoen er den \(day). dagen i måneden.")
// Output: Morgendatoen er den 29. dagen i måneden.
```

# Se også

- [Apple Developer Documentation - Comparing Dates](https://developer.apple.com/documentation/foundation/date/3123307-compare)
- [Hacking with Swift - Working with Dates and Times](https://www.hackingwithswift.com/example-code/system/how-to-work-with-dates-and-times-in-swift)
- [NSHipster - DateFormatter](https://nshipster.com/dateformatter/)