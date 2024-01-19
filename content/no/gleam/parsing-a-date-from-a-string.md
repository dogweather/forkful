---
title:                "Tolke en dato fra en streng"
html_title:           "Bash: Tolke en dato fra en streng"
simple_title:         "Tolke en dato fra en streng"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Gleam Programmering: Konvertere en Streng til en Dato.

## Hva & Hvorfor?
Konvertering av en streng til en dato innebærer å tolke teksten for å produsere en dato. Programmerere gjør dette for å håndtere og manipulere datorelatert data mer effektivt i deres applikasjoner.

## Hvordan gjøre:
Her er eksempler på hvordan du bruker Gleam til å konvertere en streng til en dato.

```Gleam
import gleam/date.{from_string, Format}

fn main() {
  let date_string = "2022-12-31"
  let date = date.from_string(date_string, Format.iso8601_date())
```

Dette vil nå gi oss en `Ok(date.Date(2022, 12, 31))` hvis strengen ble formatert riktig.

## Dyp Dykk
Historisk sett var det flere måter å håndtere streng-til-dato konverteringer i programmering. Men, dagens metoder, som den vi brukte over, tar hensyn til internasjonale standarder, som ISO-8601. 

Alternativer til dette inkluderer også bruk av biblioteker bygget for mer spesifikke use-caser, som Joda-Time i Java, eller `chrono` i Rust. Til syvende og sist avhenger valget av programmeringsspråk og applikasjonens krav.

Når det gjelder implementeringsdetaljer, innebærer parsing av en datostreng i Gleam bruk av `from_string` funksjonen, som tar en streng og et format som innganger og gir en Date-verdi tilbake.

## Se også
Her er noen lenker til relaterte ressurser:

- Gleam Date concept: https://hexdocs.pm/gleam_stdlib/gleam/date.html
- ISO-8601 wikipedia page: https://no.wikipedia.org/wiki/ISO_8601
- Joda-Time library in Java: https://www.joda.org/joda-time/
- Chrono library in Rust: https://docs.rs/chrono/0.4.19/chrono/