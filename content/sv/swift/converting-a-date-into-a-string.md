---
title:                "Omvandla ett datum till en sträng"
html_title:           "Swift: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför
Att kunna konvertera ett datum till en sträng är en grundläggande och ofta användbar färdighet för utvecklare. Det gör det möjligt för oss att presentera datumet på ett mer läsbart sätt för användare eller för att lagra det i en databas.

## Så här gör du
Konvertering av ett datum till en sträng kan göras med Swifts inbyggda funktion `toString()`. Här är ett exempel på hur man konverterar ett aktuellt datum till en sträng:

```Swift
let currentDate = Date()
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "dd-MM-yyyy" // bestämmer strängens format
let dateString = dateFormatter.string(from: currentDate)
print(dateString) // utmatning: 30-05-2020
```

I exemplet skapar vi först ett `Date` objekt för det aktuella datumet. Sedan skapar vi en `DateFormatter` och anger det önskade formatet för vår sträng. Slutligen använder vi `string(from:)` för att konvertera datumet till en sträng enligt det angivna formatet.

## Djupdykning
Det finns många olika format som kan användas för att konvertera datum till strängar. Här är några vanliga format som kan användas med `dateFormat`:

- `dd-MM-yyyy`: dag-månad-år (ex: 30-05-2020)
- `MM/dd/yyyy`: månad/dag/år (ex: 05/30/2020)
- `EEEE, MMM d, yyyy`: veckodag, tre bokstäver för månad, dag och år (ex: Saturday, May 30, 2020)
- `h:mm a`: timme:minut AM/PM (ex: 9:30 AM)

Det är viktigt att välja rätt format för det syfte där strängen kommer att användas, såsom visning i ett gränssnitt eller lagring av data i en databas. Det finns också olika inställningar för `locale` som kan påverka hur datum och tider visas beroende på var användaren befinner sig i världen.

## Se även
- [Apple Developer Documentation - Date](https://developer.apple.com/documentation/foundation/date)
- [Apple Developer Documentation - DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [Stack Overflow: Convert Date to String in Swift](https://stackoverflow.com/questions/35700281/convert-date-to-string-in-swift)