---
title:                "Sammenligner to datoer"
html_title:           "Clojure: Sammenligner to datoer"
simple_title:         "Sammenligner to datoer"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

---

## Hva & Hvorfor?

Dato-sammenligning er prosessen med å bestemme hvilken av to datoer som er tidligst, eller om de er de samme. Dette er en nøkkeloperasjon i mange programmer for alt fra loggføring til å utløse hendelser.

## Hvordan:

Gleam tilbyr innebygde funksjoner for å håndtere dato-sammenligning. Se på eksemplet nedenfor:

```gleam
import gleam/utc_date.{new, compare}
import gleam/order.{greater_than, equal}

let start_date = new(2021, 10, 10)
let end_date = new(2021, 12, 31)

match compare(start_date, end_date) {
  greater_than -> 
    "Startdatoen er etter sluttdatoen."
  equal ->
    "Startdatoen er den samme som sluttdatoen."
  _ ->
    "Startdatoen er før sluttdatoen."
}
```

**Sample Output:**
```gleam
"Startdatoen er før sluttdatoen."
```

## Dybdeplunge:

Dato-sammenligning har eksistert siden tidlige datamaskiner. De viktigste alternativene til den innebygde `gleam/utc_date.compare` funksjonen inkluderer å subtrahere den ene datoen fra den andre og tolke resultatet, eller til og med å forvandle datoene til tekststrenger og sammenligne dem alfabetisk.

Implementeringen i Gleam drar fordel av Erlangs kraftige støtte for dato- og tidshantering, noe som resulterer i en rasjonell, effektiv og fleksibel tilnærming til dato-sammenligning.

## Se Også: 

1. Gleam's offisielle dokumentasjon:
   - [gleam/utc_date](https://hexdocs.pm/gleam_stdlib/gleam/utc_date)
   - [gleam/order](https://hexdocs.pm/gleam_stdlib/gleam/order)

2. Erlang's innebygde datofunksjoner:
   - [Erlang - calendar](https://erlang.org/doc/man/calendar.html)

---