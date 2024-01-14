---
title:                "Swift: Jämförelse av två datum"
simple_title:         "Jämförelse av två datum"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

Att jämföra två datum är en viktig del av Swift-programmering, oavsett om du utvecklar en app eller jobbar med backend-system. Att förstå hur man jämför datum är avgörande för att säkerställa att din kod fungerar korrekt och att undvika felaktig data. I denna bloggpost kommer vi att gå igenom varför det är viktigt att kunna jämföra datum och hur man gör det på ett effektivt sätt.

## Så här gör du

För att jämföra två datum i Swift, används vanligtvis det inbyggda Date-typen och dess metoder. Först måste vi skapa två Date-objekt som representerar de två datum som vi vill jämföra. Detta kan göras på flera olika sätt, men ett vanligt sätt är att använda DateComponents för att definiera ett specifikt datum och sedan använda Calendar för att skapa ett Date-objekt från dessa komponenter.

```Swift
let calendar = Calendar.current // Hämta aktuell kalender
let date1Components = DateComponents(year: 2021, month: 10, day: 1) // Skapa komponenter för första datumet
let date2Components = DateComponents(year: 2021, month: 10, day: 10) // Skapa komponenter för andra datumet

let date1 = calendar.date(from: date1Components) // Skapa ett Date-objekt från komponenterna
let date2 = calendar.date(from: date2Components) // Skapa ett Date-objekt från komponenterna
```

Nu när vi har våra två Date-objekt, kan vi använda funktionen `compare` för att jämföra dem med varandra. Denna funktion returnerar en `ComparisonResult` som antingen kan vara `.orderedDescending`, `.orderedSame` eller `.orderedAscending`, beroende på om det första datumet är senare, samma eller tidigare än det andra datumet.

```Swift
let result = date1?.compare(date2!) // Jämför datum 1 med datum 2 
if result == .orderedAscending { // Om resultatet är .orderedAscending
    print("Datum 1 är tidigare än datum 2")
} else if result == .orderedDescending { // Om resultatet är .orderedDescending
    print("Datum 1 är senare än datum 2")
} else { // Om resultatet är .orderedSame
    print("Datum 1 och datum 2 är samma datum")
}
```

Beroende på hur du vill använda resultatet av jämförelsen kan du också använda de olika metoderna som finns tillgängliga för Date, som `addingTimeInterval` eller `timeIntervalSince`, för att få mer detaljerad information om skillnaden mellan de två datum. Se Apples dokumentation för mer information om dessa metoder.

## Djupdykning

Att kunna jämföra datum är en viktig del av Swift-programmering, men det finns många fall där det inte är så enkelt som att bara använda funktionen `compare`. Till exempel, om du vill jämföra datum från olika tidszoner eller om du behöver ta hänsyn till sommartid, måste du ta hänsyn till dessa faktorer när du jämför datum.

En annan utmaning kan vara att jämföra enbart datum utan tidskomponenten. I sådana fall måste du först skapa en NormalizedDate genom att ta bort tidskomponenten från dina Date-objekt och sedan jämföra dessa istället. Detta kan göras med hjälp av `dateComponents(_:from:)`-funktionen på Calendar.

## Se även

- [Apple dokumentation om Date](https://developer.apple.com/documentation/foundation/date)
- [Apple dokumentation om DateComponents](https://developer.apple.com/documentation/foundation/datecomponents)
- [Apple dokumentation om Calendar](https://developer.apple.com/documentation/foundation/calendar)
- [Real Swift dokumentation om att jämföra datum](https://learnappmaking.com/compare-dates-swift-how-to/)