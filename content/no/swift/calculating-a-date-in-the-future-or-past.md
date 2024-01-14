---
title:                "Swift: Beregning av datoer i fremtiden eller fortiden"
simple_title:         "Beregning av datoer i fremtiden eller fortiden"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/swift/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Hvorfor

Å beregne en dato i fremtiden eller fortiden kan være nyttig for å planlegge hendelser eller for å håndtere tidsbaserte funksjoner i et program.

# Slik gjør du det

For å beregne en dato i Swift må du bruke Date og Calendar-klassene. Først må du opprette en instans av Calendar og sette ønsket tids- og datoforhold. Deretter kan du bruke metoder som `date(byAdding:to value:wrappingComponents:)` for å legge til eller trekke fra tiden du ønsker.

```Swift
let calendar = Calendar.current
var futureDate = calendar.date(byAdding: .day, value: 7, to: Date())

print(futureDate) // 2021-08-18 07:00:00 +0000
```

Her har vi lagt til en uke til dagens dato og fått en ny dato som resultat.

Du kan også spesifisere en annen enhet enn dager, for eksempel måneder eller år, og også angi om datoen skal rulle over hvis den havner på en ikke-eksisterende dato, som 31. februar.

```Swift
var pastDate = calendar.date(byAdding: .month, value: -2, to: Date())

print(pastDate) // 2021-05-16 07:00:00 +0000
```

# Dykk dypere

Ved å utforske de forskjellige argumentene som kan brukes i `date(byAdding:to value:wrappingComponents:)`-metoden kan du justere datoen på mange forskjellige måter. Du kan også lære om å konvertere en dato til et spesifikt tidsstempel eller utforme en dato etter dine behov.

Se også

- [Date & Calendar - Swift Standard Library](https://developer.apple.com/documentation/swift/date)
- [Working with Dates in Swift: Tricks from a Well-written App](https://www.toptal.com/swift/ios-swift-tutorial-working-with-dates)
- [Date & Time in Swift: Ultimate Guide](https://medium.com/better-programming/date-time-in-swift-ultimate-guide-a93f3a0d0124)