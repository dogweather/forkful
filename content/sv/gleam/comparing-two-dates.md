---
title:                "Gleam: Jämföra två datum"
simple_title:         "Jämföra två datum"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför
Att jämföra två datum är en viktig del av många programmeringsprojekt. Det kan hjälpa till att kontrollera om ett datum kommer före eller efter ett annat, eller om de är lika.

## Hur man gör det
Att jämföra två datum i Gleam är ganska enkelt. Här är ett exempel på hur du kan göra det:

```
Gleam.mod
import gleam/datetime.{Date}

pub fn compare_dates(date1: Date, date2: Date) {
   if Date.is_before(date1, date2) {
      // date1 är innan date2
   } else if Date.is_equal(date1, date2) {
      // date1 och date2 är samma dag
   } else {
      // date1 är efter date2
   }
}
```

I detta exempel använder vi funktionerna `Date.is_before` och `Date.is_equal` för att jämföra två datum och utföra en åtgärd baserat på resultatet.

## Djupdykning
När du jämför två datum bör du också vara medveten om hur tidszoner och sommartid påverkar resultatet. I Gleams standardbibliotek finns även funktioner för att hantera dessa situationer, såsom `Date.adjust_for_timezone` och `Date.adjust_for_summertime`.

## Se också
För mer information om hur du hanterar datum i Gleam, se dessa länkar:

- [Gleam dokumentation för datetime](https://gleam.run/documentation/std-lib-datetime/)
- [Gleam-datum-paketet](https://github.com/gleam-lang/gleam-date)

Tack för att du läste! Fortsätt lära dig och utveckla med Gleam! 🚀