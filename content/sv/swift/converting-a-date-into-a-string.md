---
title:                "Omvandla ett datum till en sträng"
html_title:           "C#: Omvandla ett datum till en sträng"
simple_title:         "Omvandla ett datum till en sträng"
programming_language: "Swift"
category:             "Swift"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/swift/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Vad och varför?
Att konvertera ett datum till en sträng innebär att omforma datumet till läsbar text. Programmerare gör det för att göra datumdata lättare att förstå och presentera för användarna.

## Hur gör man?
Vi kan använda Swifts Date-funktioner för att konvertera ett datum till en sträng. Här är ett exempel på hur man kan göra det:

```Swift
import Foundation

let nu = Date()
let dateFormatter = DateFormatter()
dateFormatter.dateFormat = "yyyy-MM-dd HH:mm:ss"
let formateradDatum = dateFormatter.string(from: nu)
print(formateradDatum)
```

När du kör denna kod kommer du att se en strängrepresentaion av dagens datum och tid i konsolen, något i stil med:

```Swift
2022-03-10 12:34:56
```

## Fördjupning
Att konvertera datum till strängar är en vanlig uppgift inom programmering. Historiskt sett har detta varit en utmaning i många programspråk, med tanke på de olika datumformaten som används runt om i världen. Swift löser detta på ett elegant sätt genom klassen DateFormatter.

I Swift, om du inte vill använda den inbyggda DateFormatter-klassen finns andra alternativ. Ett exempel är att använda tidsstämplar och konvertera dem till datumsträngar.

Det bör dock noteras att Swifts DateFormatter är mycket kraftfull och kan hantera en rad olika datum- och tidsformat. Dessutom tar den hand om tidsskillnader och lokalinställningar automatiskt, vilket gör det till ett utmärkt val för de flesta applikationer.

## Se även
För mer information om att arbeta med datum och tider i Swift, se följande sidor:
- [Apple Developer Documentation - Date](https://developer.apple.com/documentation/foundation/date)
- [Apple Developer Documentation - DateFormatter](https://developer.apple.com/documentation/foundation/dateformatter)