---
title:                "Jämförande av två datum"
html_title:           "Swift: Jämförande av två datum"
simple_title:         "Jämförande av två datum"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Vad & Varför?
När vi pratar om att jämföra två datum i Swift, så syftar vi på att kontrollera om två datum är lika eller om ena datumet är tidigare eller senare än det andra. Detta är en vanlig uppgift för programmerare när de arbetar med datum och tid i sina program.

## Hur gör man?
För att jämföra två datum i Swift, kan vi använda inbyggda metoder och operatörer. Vi kan till exempel använda "==", "<" och ">" för att kontrollera om två datum är lika eller om ena datumet är tidigare eller senare än det andra. Kolla in kodexempel nedan:

```Swift
let date1 = Date()
let date2 = Date(timeIntervalSinceNow: 3600) // skapar ett datum en timme framåt i tiden
if date1 == date2 {
    print("Datumen är lika.")
} else if date1 < date2 {
    print("Date1 är tidigare än date2.")
} else {
    print("Date1 är senare än date2.")
}
```

Output:
```
Date1 är tidigare än date2.
```

## Djupdykning
När det kommer till att jämföra datum, är det viktigt att veta att det inte alltid är så enkelt som att bara använda operatörer. Det finns många faktorer som kan påverka resultatet, som till exempel tidszoner och olika kalendrar. Det är därför viktigt att använda rätt metoder för att få en korrekt jämförelse.

Som ett alternativ till att använda operatörer, kan man också använda metoden "compare(_:)". Denna metod jämför två datum och returnerar en "ComparisonResult" som kan vara .orderedAscending (om det första datumet är tidigare), .orderedDescending (om det andra datumet är tidigare) eller .orderedSame (om de är lika). Detta kan vara användbart om man vill använda resultatet för att göra olika uppgifter i programmet.

Implementeringsdetaljer som är viktiga att känna till är att Swift använder Gregorian Calendar som standard för att hantera datum och tid, men man kan också använda andra kalendrar genom att använda "Calendar"-klassen.

## Se även
- [Apple Developer Documentation - Date](https://developer.apple.com/documentation/foundation/date)
- [Apple Developer Documentation - Calendar](https://developer.apple.com/documentation/foundation/calendar)
- [Official Swift Forum - Date comparison](https://forums.swift.org/t/date-comparison/25249)