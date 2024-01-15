---
title:                "Beräkna ett datum i framtiden eller förflutna"
html_title:           "Ruby: Beräkna ett datum i framtiden eller förflutna"
simple_title:         "Beräkna ett datum i framtiden eller förflutna"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/ruby/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Varför

Att kunna beräkna ett datum i framtiden eller det förflutna är en viktig del av programmering. Det kan vara användbart för att hålla reda på tidsbaserade händelser eller för att skapa dynamiska funktioner i en applikation.

## Hur man gör

Beräkna ett datum i framtiden eller förflutna är enkelt med Ruby. Det finns flera metoder som kan användas för detta ändamål.

För att beräkna ett datum i framtiden, använd `+` operatören tillsammans med `Date` klassen. Här är ett exempel på att lägga till 30 dagar till dagens datum:

```Ruby
Date.today + 30
=> "2021-08-07"
```

För att beräkna ett datum i förflutna, använd `-` operatören. Här är ett exempel på att subtrahera 2 veckor från dagens datum:

```Ruby
Date.today - 14
=> "2021-07-14"
```

För att beräkna ett datum baserat på ett specifikt datum, använd `parse` metoden tillsammans med `Date` klassen:

```Ruby
Date.parse("2021-07-15") + 10
=> "2021-07-25"
```

## Deep Dive

När man beräknar ett datum i framtiden eller förflutna kan det vara viktigt att tänka på olika tidszoner och dagar som kan påverka resultatet. Ruby har inbyggda funktioner för att hantera olika tidszoner och för att ta hänsyn till skottår.

För att använda en specifik tidszon, använd `TimeZone` klassen tillsammans med `+` operatören. Här är ett exempel på att lägga till 5 timmar för en viss tidszon:

```Ruby
TimeZone["Pacific Time (US & Canada)"].now + 5.hours
=> "2021-07-08 18:22:00 -0700"
```

För att ta hänsyn till skottår, använd `next_year` eller `prev_year` metoder tillsammans med `Date` klassen. Här är ett exempel på att beräkna nästa skottår från ett specifikt datum:

```Ruby
Date.new(2021).next_year
=> "2024-01-01"
```

## Se även

- [Ruby Date and Time Class](https://ruby-doc.org/stdlib-2.7.0/libdoc/date/rdoc/Date.html)
- [Ruby Date and Time Format](https://ruby-doc.org/core-3.0.0/Date.html#method-i-strftime)
- [Ruby Time Zones](https://ruby-doc.org/stdlib-2.7.0/libdoc/time/rdoc/Time.html#class-Time-label-Time+zone+conversion)