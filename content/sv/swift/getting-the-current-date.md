---
title:                "Hämta aktuellt datum"
html_title:           "Swift: Hämta aktuellt datum"
simple_title:         "Hämta aktuellt datum"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Varför

Att kunna hämta och använda den aktuella datumet är en viktig funktion för många Swift-programmerare. Det kan användas för att visa tidslinjer, spara tidsstämplar i databaser eller helt enkelt för att hålla reda på den aktuella tiden.

## Hur man gör

För att hämta den aktuella datumet i Swift, kan du använda dig av `Date()` -klassen. Det finns flera olika sätt att använda den för att få den information du behöver. Här är några exempel:

```Swift
// Hämta den aktuella datumet i default tidszon
let currentDateTime = Date()

// Hämta den aktuella datumet i en specifik tidszon 
let dateFormatter = DateFormatter()
dateFormatter.timeZone = TimeZone(identifier: "Europe/Stockholm")
let currentDateTime = Date()
```

Du kan också formatera den aktuella datumet med hjälp av olika formatmallar. Till exempel:

```Swift
// Visa den aktuella datumet i en specifik tidszon med ett anpassat format
dateFormatter.dateFormat = "dd-MM-yyyy HH:mm"
let currentDateTime = Date()
```

Att använda `Date()` -klassen ger dig också möjlighet att göra andra operationer som att jämföra olika datum, konvertera mellan tidszoner och mycket mer. Det är en mycket kraftfull klass som är väl värt att utforska mer.

## Djupdykning

När du använder `Date()` -klassen, är det viktigt att förstå hur tidszoner fungerar för att undvika eventuella problem med datum och tider. Det är också bra att veta att detta är en del av Foundation Framework, vilket betyder att den har stöd för både iOS och macOS.

Det finns också andra alternativ för att få den aktuella datumet, som till exempel att använda `Calendar` -klassen eller `NSCalendar` för äldre projekt.

## Se även

- [Apple - Date](https://developer.apple.com/documentation/foundation/date)
- [Stack Overflow - How to get current time as per time zone in Swift](https://stackoverflow.com/questions/57647133/how-to-get-current-time-as-per-time-zone-in-swift)
- [Hacking with Swift - Handling dates and times](https://www.hackingwithswift.com/read/11/4/handling-dates-and-times)