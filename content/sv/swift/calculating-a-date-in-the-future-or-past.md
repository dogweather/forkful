---
title:    "Swift: Beräkna ett datum i framtiden eller det förflutna"
keywords: ["Swift"]
---

{{< edit_this_page >}}

# Varför

Att kunna räkna ut ett datum i framtiden eller förflutet är en viktig del av Swift-programmering. Det kan hjälpa dig att skapa mer flexibla appar och hantera datum på ett mer effektivt sätt.

# Hur man gör det

För att kunna räkna ut ett datum i Swift behöver du använda dig av klassen `Date`. Här är ett exempel på hur du kan skapa ett datum som ligger 2 veckor framåt i tiden:

```Swift
let today = Date()
let twoWeeksFromToday = Calendar.current.date(byAdding: .weekOfYear, value: 2, to: today)
```

Du kan också räkna ut ett datum från ett befintligt datum. Till exempel, om du vill beräkna ett datum som ligger en månad bakåt i tiden från idag, kan du använda denna kod:

```Swift
let today = Date()
let oneMonthAgo = Calendar.current.date(byAdding: .month, value: -1, to: today)
```

Du kan även formatera dina datum som du vill ha dem genom att använda `DateFormatter` klassen. Till exempel, om du vill ha datumet i ett annat format, som "YYYY.MM.DD", kan du använda följande kod:

```Swift
let formatter = DateFormatter()
formatter.dateFormat = "YYYY.MM.DD"
let formattedDate = formatter.string(from: today)
```

# En djupare titt

Det finns flera olika sätt att räkna ut datum i Swift beroende på dina behov. Du kan använda olika tidsintervaller som `year`, `month`, `day` eller `minute` när du använder `Calendar.current.date(byAdding: value:, to:)` metoden. Du kan också använda dig av `DateComponents` för mer exakta beräkningar.

För att göra kodningen ännu enklare kan du skapa anpassade funktioner som hanterar beräkningar åt dig. Du kan till exempel skapa en funktion som accepterar ett datum och ett angivet antal dagar, och sedan räknar ut det nya datumet åt dig.

# Se även

- [Apple Developer documentation - Date](https://developer.apple.com/documentation/foundation/date)
- [Hacking with Swift - Working with dates in Swift](https://www.hackingwithswift.com/read/15/overview)
- [Swift by Sundell - Handling dates and times in Swift](https://www.swiftbysundell.com/basics/dates-and-times/)