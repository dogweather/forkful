---
title:                "Jämförelse av två datum"
html_title:           "Gleam: Jämförelse av två datum"
simple_title:         "Jämförelse av två datum"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Varför

Att jämföra två olika datum kan vara användbart i många olika situationer, både inom professionell programmering och för personligt bruk. Det kan hjälpa dig att hålla koll på tidsintervaller, kontrollera giltigheten av ett datum eller utföra olika beräkningar baserat på datum.

## Så här gör du

```Gleam
import gleam/time

let today = time.now()

let tomorrow = today + time.Day
let yesterday = today - time.Day

// Jämföra två datum för likhet
today == tomorrow
// Output: false

// Jämföra två datum för ordning, första datumet först
yesterday < today
// Output: true

// Beräkna antalet dagar mellan två datum
let diff = tomorrow - yesterday
// Output: 2

// Kontrollera giltigheten av ett datum
let validDate = time.from_days(2021, 4, 31)
// Output: false, eftersom april 2021 inte har 31 dagar
```

## Djupdykning

Det finns flera olika metoder för att jämföra datum i Gleam. En av de mest användbara är att använda operatorerna `==` (lika med) och `<` (mindre än). Dessa kan användas för att jämföra datum på olika sätt som visas i exemplet ovan.

Gleam erbjuder också funktioner som `difference_between` för att beräkna antalet enheter (t.ex. dagar eller månader) mellan två datum och `is_valid_date` för att kontrollera giltigheten av ett angivet datum.

## Se även

- Official Gleam documentation for the `time` module: https://gleam.run/documentation/stdlib/time.html
- Date comparison in other programming languages: https://www.exploringbinary.com/date-comparisons-in-different-programming-languages/