---
title:                "Konvertera ett datum till en sträng"
html_title:           "Elm: Konvertera ett datum till en sträng"
simple_title:         "Konvertera ett datum till en sträng"
programming_language: "Elm"
category:             "Elm"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/elm/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Varför
Du kanske undrar varför du skulle behöva konvertera ett datum till en sträng i Elm. Det finns flera anledningar till detta, bland annat för att visa datumet i ett läsbart format eller för att mata in det i ett formulär.

## Hur man gör
Att konvertera ett datum till en sträng i Elm är ganska enkelt. Först behöver vi importera modulen "Date" och sedan använda funktionen `Date.toIsoString`. Nedan följer ett exempel:

```Elm
import Date exposing (toIsoString)

date = Date.fromCalendarDate 2021 10 31
str = Date.toIsoString date
```

Värdet av strängen blir nu `"2021-10-31"`. Du kan också specificera vilka delar av datumet du vill visa, till exempel året, månaden och dagen. Läs mer om detta i dokumentationen för Date-modulen.

## Djupdykning
Du kanske undrar varför utgångsformatet för en konverterad datumsträng är "åååå-mm-dd". Detta beror på att Elm använder sig av ISO 8601-formatet, vilket är en internationell standard för representation av datum och tid. Detta gör det enkelt att översätta datum mellan olika tidszoner och underlättar kommunikationen mellan olika system.

## Se även
- Date-modulens dokumentation: https://package.elm-lang.org/packages/elm/time/latest/Date#toIsoString 
- Elm-dokumentationen för ISO 8601: https://guide.elm-lang.org/dates_and_times/iso_8601.html