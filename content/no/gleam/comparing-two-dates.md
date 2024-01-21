---
title:                "Sammenlikning av to datoer"
date:                  2024-01-20T17:33:04.712156-07:00
model:                 gpt-4-1106-preview
simple_title:         "Sammenlikning av to datoer"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why?
"## Hva & Hvorfor?"

Å sammenligne to datoer handler om å sjekke om de er like, hvilken som kommer først, eller hvor lang tid det er mellom dem. Programmerere gjør dette for å håndtere frister, tidslinjer og tidsbaserte funksjoner i apper.

## How to:
"## Slik gjør du:"

Gleam gjør date sammenligning enkel og rett frem. Her er et eksempel:
```gleam
import gleam/calendar.{Date, Duration}

fn compare_dates(date1: Date, date2: Date) -> String {
  case calendar.compare(date1, date2) {
    Lt -> "Date1 er før Date2"
    Eq -> "Date1 er den samme som Date2"
    Gt -> "Date1 er etter Date2"
  }
}

fn main() {
  let date1 = calendar.date(2023, 4, 6)
  let date2 = calendar.date(2023, 5, 1)
  compare_dates(date1, date2)
}
```

Sample output:
```"Date1 er før Date2"```

## Deep Dive:
"## Dypdykk:"

Å sammenligne datoer er en vanlig oppgave som går tilbake til tidlige dager av programmering. I Gleam og andre moderne språk, legger standardbiblioteket opp til funksjonalitet for å manipulere og sammenligne datoer. En alternativ metode er å konvertere datoer til et enhetlig format som UNIX-tid, før sammenligning. I stedet for å bare sjekke rå dato objekter, kan implementeringsdetaljer omfatte tidsonehåndtering og støtte for skuddsekunder.

## See Also:
"## Se også:"

- ISO 8601, the international standard for date and time representations: [ISO 8601 on Wikipedia](https://en.wikipedia.org/wiki/ISO_8601)
- Understanding Time Zones and Daylight Saving Time: [Time Zone Article](https://www.timeanddate.com/time/time-zones.html)