---
title:                "Jämföra två datum"
date:                  2024-01-20T17:33:06.851682-07:00
model:                 gpt-4-1106-preview
simple_title:         "Jämföra två datum"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?
Jämföra två datum handlar om att se vilket som är tidigare, senare eller om de är samma. Programmerare gör detta för att hantera deadlines, tidslinjer, schemaläggning och mer.

## How to:
I Gleam kan du använda standardbiblioteksfunktioner för att jämföra datum. Så här gör du:

```gleam
import gleam/calendar.{Date}
import gleam/int

fn compare_dates(date1: Date, date2: Date) -> int {
  Date.cmp(date1, date2)
}

fn main() {
  let date1 = Date.new!(2023, 4, 15)
  let date2 = Date.new!(2023, 4, 18)
  
  let comparison = compare_dates(date1, date2)
  
  case comparison {
    0 -> "Dates are the same"
    int.GT -> "Date1 is later"
    int.LT -> "Date1 is earlier"
  }
}
// "Date1 is earlier"
```
## Deep Dive
I historien har datumhantering varit knepigt på grund av olika tidssystem och kalendrar. I Gleam, och de flesta modernt programmeringsspråk, används Gregorianska kalendern som standard. Alternativ för datumjämförelser inkluderar egna funktioner för specifika behov eller tredjepartsbibliotek om du behöver hantera mer komplexa tidzoner och format.

Detaljer i implementeringen inkluderar säkerställande av att datumobjekten som jämförs är i samma tidzon. Även hanteringen av skottår och olikheter mellan kalendrar måste tas i beaktning för noggrannhet.

## See Also
- For a wider context on date and time in programming, read [Computer Date and Time Implementations](https://en.wikipedia.org/wiki/System_time) on Wikipedia.
