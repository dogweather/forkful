---
title:                "Swift: Jämföring av två datum"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

Att jämföra två datum kan vara användbart för att få ut specifik information om tidsperioder eller för att kontrollera om en specifik händelse inträffat före eller efter en annan.

## Hur man gör det

För att jämföra två datum i Swift, kan du använda metoden `compare()` som finns på `Date`-objektet. Nedan finns ett exempel på hur du kan använda denna metod för att jämföra två datum:

```Swift
let datum1 = Date()
let datum2 = Date(timeIntervalSinceNow: -3600) // Skapar ett datum som är en timme tidigare än datum1

if datum1.compare(datum2) == .orderedAscending {
    print("Datum 1 är tidigare än datum 2")
} else if datum1.compare(datum2) == .orderedDescending {
    print("Datum 1 är senare än datum 2")
} else {
    print("Datum 1 och datum 2 är samma")
}

// Output: Datum 1 är senare än datum 2
```

Du kan också jämföra datum med hjälp av operatörer, exempelvis udig `>` och `<`:

```Swift
if datum1 > datum2 {
    print("Datum 1 är senare än datum 2")
}
```

## Djupdykning

När du jämför datum med Swift, måste du vara medveten om att jämförelsen görs på millisekundsnivå. Det betyder att även om två datum kanske verkar samma på ögat, så kan en millisekund skilja dem åt. För att undvika detta kan du använda metoden `timeIntervalSince(_:)` som ger dig en tidsperiod i sekunder mellan två datum istället för att bara jämföra dem.

En annan viktig sak att komma ihåg när man jämför datum är tidszoner och kalendrar. En del länder använder en annan kalender än den gregorianska, vilket kan påverka hur datum visas och jämförs. Det är därför viktigt att du är medveten om vilken tidszon och kalender som används för att få korrekta jämförelser mellan datum.

## Se även

- [Apple Developer - Date](https://developer.apple.com/documentation/foundation/date)
- [Hacking with Swift - How to compare dates the Swift way](https://www.hackingwithswift.com/example-code/system/how-to-compare-dates-the-swift-way)
- [Swift Examples - Dates](https://swiftexamples.com/dates/)