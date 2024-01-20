---
title:                "Tolke en dato fra en streng"
date:                  2024-01-20T15:36:20.386806-07:00
html_title:           "Arduino: Tolke en dato fra en streng"
simple_title:         "Tolke en dato fra en streng"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Parsing av dato fra en streng innebærer å oversette tekst til en datostruktur. Programmører gjør dette for å kunne håndtere datoer i logikk og lagring på en standardisert måte.

## Hvordan:
```gleam
import gleam/erlang/date
import gleam/erlang.{String, Result}
import gleam/calendar.{Date, DateParseError}

pub fn parse_date_from_string(date_string: String) -> Result(Date, DateParseError) {
  date.from_iso8601(date_string)
}

// Bruk:
fn main() {
  let date_string = "2023-04-05"
  let parsed_date = parse_date_from_string(date_string)

  case parsed_date {
    Ok(date) -> io.println("Datoen er: " ++ date.to_string())
    Error(_) -> io.println("Det var en feil ved parsing av datoen.")
  }
}

// Eksempel på output:
// "Datoen er: 2023-04-05"
```

## Dypdykk
Historisk sett har datoparsing vært en kilde til feil grunnet ulike formater og tidssoner. Gleam tilbyr en robust løsning med `date.from_iso8601` som adlyder det internasjonale standardformatet ISO 8601.

Alternativer for parsing av datoer inkluderer manuell tolking av strenger, bruk av tredjepartsbiblioteker eller integrerte funksjoner i host-språket, som Erlang i tilfellet med Gleam.

Implementasjonsdetaljer inkluderer validering av strengens format, konvertering til et Date-objekt og å håndtere feil, som i vårt eksempel med `Result(Date, DateParseError)` for å representere enten en suksessfull parsing eller en feil.

## Se Også
- Gleam Date dokumentasjon: https://hexdocs.pm/gleam_stdlib/gleam/calendar/
- ISO 8601 standarden: https://www.iso.org/iso-8601-date-and-time-format.html
- Erlang's Date funksjoner: https://erlang.org/doc/man/calendar.html