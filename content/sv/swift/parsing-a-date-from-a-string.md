---
title:                "Tolkning av datum från en sträng"
html_title:           "Swift: Tolkning av datum från en sträng"
simple_title:         "Tolkning av datum från en sträng"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att parsea ett datum från en sträng betyder att konvertera en textsträng som innehåller ett datum till ett datumobjekt som programmet kan förstå. Programmers utför detta för att hantera och manipulera datum på ett mer läsbart sätt.

## Hur:
```Swift
let dateString = "2020-06-26"
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy-MM-dd"
if let date = dateFormatter.date(from: dateString) {
    print(date) // output: 2020-06-26 00:00:00 +0000
}
```

## Djupdykning:
Historiskt sett har olika format för datum använts runt om i världen, vilket kan orsaka problem när man arbetar med datum i ett datorprogram. Det finns även andra sätt att konvertera ett datum från en sträng, som genom användning av en kalender. I Swift finns möjligheten att skapa en egen kalender för mer kontrollerad hantering av datum.

## Se även:
- [DateFormatter dokumentation](https://developer.apple.com/documentation/foundation/dateformatter)
- [Kalender i Swift](https://www.hackingwithswift.com/example-code/system/how-to-create-a-custom-calendar-with-swift)