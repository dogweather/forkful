---
title:    "Swift: Att få den aktuella datumen"
keywords: ["Swift"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/sv/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att få den nuvarande datumet kan vara en viktig del av att utveckla appar eller program. Det kan hjälpa till att hålla reda på när en viss händelse inträffade eller visa realtidsinformation.

## Hur man gör

För att få den nuvarande datumet, använd Swifts ```Date``` klass. Här är ett exempel på hur man skapar en instans av ```Date``` och få datumet ur den:

```
let currentDate = Date()
print(currentDate)
```

Output:

2021-10-15 17:25:00 +0000

För att få datumet i ett visst format, kan du använda ```DateFormatter``` klassen. Till exempel, om du vill ha datumet i ISO8601-formatet, kan du göra följande:

```
let formatter = DateFormatter()
formatter.dateFormat = "yyyy-MM-dd"
let currentDate = formatter.string(from: Date())
print(currentDate)
```

Output:

2021-10-15

Det finns många olika formatalternativ att välja mellan beroende på dina behov. Se till att kolla dokumentationen för ```DateFormatter``` för en komplett lista av formatalternativ.

## Djupdykning

En ```Date``` instans representerar en specifik punkt i tiden, precis som en punkt på en tidsaxel. Det är också möjligt att skapa en ```Date``` instans från en specifik tidpunkt. Till exempel, om du vill ha datumet för 1 januari 2021, kan du göra följande:

```
let calendar = Calendar.current
let dateComponents = DateComponents(year: 2021, month: 1, day: 1)
let specificDate = calendar.date(from: dateComponents)
print(specificDate)
```

Output:

2021-01-01 00:00:00 +0000

Det finns också möjlighet att jämföra olika ```Date``` instanser för att se om de är efter, före eller samtidigt. Detta kan vara användbart för att sortera datum och hantera händelser som inträffar vid olika tidpunkter.

## Se även

- [Swift Date and Time Tutorial](https://www.raywenderlich.com/5774132-swift-date-and-time-tutorial-getting-started)
- [Apple Developer Documentation for Date](https://developer.apple.com/documentation/foundation/date)
- [NSHipster article on Date](https://nshipster.com/date/)