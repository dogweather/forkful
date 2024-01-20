---
title:                "Slik får du tak i dagens dato"
date:                  2024-01-20T15:14:27.140183-07:00
html_title:           "C: Slik får du tak i dagens dato"
simple_title:         "Slik får du tak i dagens dato"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å hente gjeldende dato i programmering er akkurat som det høres ut – grabbing av dagens dato. Vi gjør det for å loggføre hendelser, håndtere tidsavhengige funksjoner eller bare for å vise tidspunktet til brukeren.

## Hvordan gjøre det:
Gleam er praktisk, men husk at du vil trenge ekstern funksjonalitet for å jobbe med tid og dato, siden Gleam standardbibliotek ikke inkluderer dette enda. En populær tilnærming er å bruke Erlang-funksjoner gjennom Gleams interoperabilitet med BEAM (Erlang virtual machine).

```gleam
// Sørg for å ha Erlang/OTP og gleam installert på maskinen din.
// Dette eksemplet bruker `erlang`-modulen.
import erlang

fn main() {
  let now = erlang.now() // Returnerer et tuppel av `{megaseconds, seconds, microseconds}`
  erlang.io.format("Gjeldende tidspunkt er: ~p~n", [now])
}
```

Kjør programmet og få output lignende dette (basert på når du kjører det) i konsollen:
```
Gjeldende tidspunkt er: {Megasekunder, Sekunder, Mikrosekunder}
```

## Dypdykk
Før vi hadde enkle funksjoner og moduler for å hente tidsdata, måtte programmerere ty til operativsystemets klokke eller en integrert krets som RTC (Real-Time Clock) på hardwaren. Alternativt, kan du bruke tredjeparts Gleam pakker, eller Beams innebygde moduler som `:calendar` for mer avanserte funksjoner. Details om implementering kan inkludere tidszonehåndtering og skuddårlogikk.

## Se også
- Gleam's offisielle dokumentasjon: [https://gleam.run](https://gleam.run)
- Erlang's `:calendar` modul: [http://erlang.org/doc/man/calendar.html](http://erlang.org/doc/man/calendar.html)
- Community drevne Gleam pakker: [https://hex.pm](https://hex.pm)