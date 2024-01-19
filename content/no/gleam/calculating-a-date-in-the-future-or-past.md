---
title:                "Beregning av en dato i fremtiden eller fortiden"
html_title:           "Gleam: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

# Beregning av Datoer i Gleam: Blikk i Fortiden og Framtiden 

## Hva & Hvorfor?
Beregning av en dato i fremtiden eller fortiden, er å finne en spesifikk dato basert på en gitt dato og et antall dager, uker, eller år fremover eller bakover. Programmers gjør dette for å håndtere real-world scenarioer som å forutsi frister, håndtere påminnelser, osv.

## Hvordan:
Her er et enkelt eksempel på hvordan du kan beregne en framtidig dato, ved å legge til dager på en gitt dato.

```gleam
import gleam/date.{Date, add_days}

fn main() {
 let initial_date = Date.new(2022, 10, 1)
 let future_date = initial_date |> add_days(20)
 future_date.to_string() // Outputs "2022-10-21"
}
```

Og her er hvordan du kan beregne en tidligere dato, ved å trekke dager fra en gitt dato.

```gleam
import gleam/date.{Date, subtract_days}

fn main() {
 let initial_date = Date.new(2022, 10, 20)
 let past_date = initial_date |> subtract_days(19)
 past_date.to_string() // Outputs "2022-10-01"
}
```

## Dybde Dykk
Beregning av datoer er noe som har blitt gjort i dataprogrammering helt siden starten. Vi trenger å spore dette for å håndtere hendelser i fortiden og fremtiden. I eldre programmeringsspråk som C og Perl, er det mer komplisert å beregne datoer på denne måten.

Det er mange alternativer for å beregne en dato i fremtiden eller fortiden. Du kan bruke innebygde biblioteker (som vi gjorde i Gleam eksemplene), tredjeparts biblioteker, eller du kan selv lage ditt eget.

Detaljene for hvordan datoer beregnes i Gleam er ganske rett frem. Vi bruker en funksjon for å legge til eller trekke dager fra en gitt dato, og det gir oss den beregnede datoen. Dette er laget mulig ved hvordan Gleam håndterer datoer, der hver dato er en umutabel verdi og beregninger resulterer i en ny datoverdi.

## Se Også
1. Gleam dokumentasjon om dato behandling: https://gleam.run/book/tour/dates-and-times.html
2. Gleam GitHub Repo: https://github.com/gleam-lang/gleam
3. Innføring i Gleam for Erlang- og Elixir-Utviklere: https://dev.to/QuinnWilton/introduction-to-gleam-for-erlang-and-elixir-developers