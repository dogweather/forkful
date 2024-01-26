---
title:                "Beregning av en dato i fremtiden eller fortiden"
date:                  2024-01-20T17:30:56.474863-07:00
model:                 gpt-4-1106-preview
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å regne ut en dato i fremtiden eller fortiden er prosessen med å legge til eller trekke fra tid fra en gitt dato. Programmerere gjør dette for å håndtere tidsbestemte funksjoner som frister, påminnelser eller for å beregne tid som har passert.

## Hvordan gjøre:
```gleam
import gleam/calendar.{ Date, Duration, add_duration }
import gleam/io

pub fn main() {
  let today = Date(2023, 4, 15)
  let ten_days = Duration(days: 10)
  
  let future_date = add_duration(today, ten_days)
  io.debug(future_date) // Output: Date(2023, 4, 25)

  let minus_ten_days = Duration(days: -10)
  
  let past_date = add_duration(today, minus_ten_days)
  io.debug(past_date) // Output: Date(2023, 4, 5)
}
```
Dette eksempelet viser hvordan du legger til og trekker fra dager på en dato i Gleam.

## Dypdykk
I dataprogrammering har håndtering av datoer og tid alltid vært en kritisk oppgave. Det blir spesielt viktig når vi jobber med frister, aldersberegninger, eller tidsbaserte hendelser. Beregning av datoer stammer fra behovet for å forstå og manipulere tidslinjer i programvare.

Gleam, et statisk typet språk som kompilerer til Erlang VM, tilbyr et robust kalenderbibliotek for å håndtere slike operasjoner. Alternativer til Gleam for datohåndtering inkluderer språk som Python med sitt `datetime` bibliotek, eller JavaScript med biblioteker som `moment.js` og `date-fns`. I motsetning til noen av disse alternativene som kan være mutation-baserte, foretrekker Gleam en mer funksjonell og sideeffektfri tilnærming.

I implementasjonen av tidsberegninger, bruker man ofte en Duration-datastruktur som representerer en tidsperiode, som er addert til eller substrahert fra et Date-objekt for å få det nye datoverdien.

## Se også
- ISO 8601 Date and Time Format, som brukes blant annet i Gleam for datoformat: [https://www.iso.org/iso-8601-date-and-time-format.html](https://www.iso.org/iso-8601-date-and-time-format.html)
