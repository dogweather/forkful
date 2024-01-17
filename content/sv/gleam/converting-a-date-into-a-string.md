---
title:                "Omvandling av datum till sträng"
html_title:           "Gleam: Omvandling av datum till sträng"
simple_title:         "Omvandling av datum till sträng"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Vad & Varför?
Att konvertera ett datum till en sträng är en viktig uppgift för programmerare eftersom det ger dig möjlighet att visa datumet på ett sätt som är läsbart för mänskliga användare. Det kan vara avgörande för användbarheten i ett program eller en applikation.

## How to:
Du kan använda inbyggda funktioner i Gleam för att enkelt konvertera ett datum till en sträng. Här är ett exempel på hur du kan göra det:

``` Gleam
import gleam/time

let now = time.now()
let string = time.format(now, "%Y-%m-%d %H:%M:%S")
```

Detta kodexempel skulle ge utmatningen "2021-10-29 12:00:00" baserat på det aktuella datumet och tiden.

## Deep Dive:
Att konvertera ett datum till en sträng är en vanlig uppgift i många programmeringsspråk. Det finns många olika sätt att göra det på, men i Gleam använder man funktionen "format" från tidspaketet för att göra det.

En alternativ metod för att konvertera datum till strängar är att lägga till en operator för att göra det enklare. Detta har föreslagits i diskussioner om utveckling av Gleam, men är för närvarande inte implementerat.

## See Also:
För mer information om konvertering av datum till strängar i Gleam, rekommenderar vi att du läser dokumentationen för tidpaketet och nyhetsaggregeraren Gleam Weekly för de senaste uppdateringarna inom Gleam-utveckling.