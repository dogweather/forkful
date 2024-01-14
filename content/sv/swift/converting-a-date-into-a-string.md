---
title:                "Swift: Konvertera ett datum till en sträng"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför

Att konvertera ett datum till en sträng kan vara mycket användbart när man arbetar med olika typer av data i Swift-programmering. Det kan hjälpa till att representera datumet på ett mer läsbart och användbart sätt i din kod.

## Så här gör du

För att konvertera ett datum till en sträng i Swift, använd funktionen "```string(from: Date)```". Här är ett exempel på hur du kan använda det i din kod:

```Swift
let formatter = DateFormatter()
formatter.dateFormat = "dd MMMM yyyy"
let date = formatter.date(from: "12 December 2020")

if let convertedDate = date {
    let dateString = formatter.string(from: convertedDate)
    print(dateString)
}
```

**Output:** 12 december 2020

## Utforska djupare

Det finns olika sätt att anpassa datumformatet beroende på dina behov. Du kan till exempel ändra formatet från "dd MMMM yyyy" till "E, d MMMM yyyy" för att få en förkortad veckodagsförkortning inkluderat i datumsträngen.

En annan viktig aspekt att tänka på är lokalisering när du konverterar ett datum till en sträng. Du kan använda "```Locale(identifier: "sv_SE")```" för att se till att datumet visas i formatet som är vanligt i Svenska.

## Se även

- [Dokumentation för DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)
- [En guide för att hantera datum och tid i Swift](https://www.hackingwithswift.com/articles/32/nsdateformatter-cheat-sheet)
- [En tutorial om lokalisering i Swift](https://www.raywenderlich.com/126663/nsformatter-swift-tutorial-working-with-nsformattedstring)