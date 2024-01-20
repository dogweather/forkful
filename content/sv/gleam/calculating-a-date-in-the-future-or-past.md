---
title:                "Beräkna ett datum i framtiden eller förflutna"
html_title:           "Gleam: Beräkna ett datum i framtiden eller förflutna"
simple_title:         "Beräkna ett datum i framtiden eller förflutna"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/sv/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Vad & Varför?

Datumberäkningar i framtiden eller det förflutna handlar om att hitta exakta datum baserat på specifika tidsintervall. Programmers behöver detta för att hantera och planera åtaganden och händelser i programvarulösningar.

## Såhär gör man:

I Gleam används `calendar`-modulen för att utföra datum- och tidsberäkningar. 

Gleamkodexempel för att lägga till dagar till ett datum:

```Gleam
import gleam/calendar.{from_iso8601, add_days, to_iso8601}

fn main() {
    let start_date = from_iso8601("2022-08-15").unwrap()
    let future_date = add_days(start_date, 7)
    let future_date_string = to_iso8601(future_date)

  io.println(future_date_string) // Outputs: "2022-08-22"
}
```

## Fördjupning

Historiskt sett har datumberäkningar alltid varit en del av programmering, då de hjälper program att interagera med och förstå verkliga händelser. Många olika tillvägagångssätt och tekniker används, från enkla addition och subtraktionsoperationer till mer komplexa tid- och datumfunktioner.

Ett alternativ till `gleam/calendar`-modulen är `gleam/date`-modulen, som också erbjuder funktioner för att hantera datum och tid.

När det gäller genomförande av datum och tid i framtiden eller det förflutna, har alltid betydelsen av tidzoner och skottår beaktats, eftersom de spelar en stor roll i korrekta beräkningar.