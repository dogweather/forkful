---
title:                "Få dagens dato"
html_title:           "Gleam: Få dagens dato"
simple_title:         "Få dagens dato"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva er det å få nåværende dato, og hvorfor gjør programmerere det?

 Å få nåværende dato i et program betyr å hente og vise den nåværende datoen på datamaskinen eller enheten som kjører programmet. Dette kan være nyttig for å vise når et program ble kjørt, laget eller når en hendelse skjedde. Det er også en viktig del av å lage dynamiske og presise applikasjoner.

## Hvordan gjør man det:

 ```Gleam
import time
 
let nåværende_dato = time.now()
 
io.println(some_date)
 
// Eksempel på utskrift: 2021-09-22T12:34:56Z
```

Denne koden importerer "time" biblioteket og bruker så "now" funksjonen for å få den nåværende datoen. Deretter printer den datoen ved hjelp av "io.println" funksjonen. Output vil være i ISO-8601 datoformat som er standard for datorepresentasjon.

Mer kompliserte formateringsalternativer er også tilgjengelige med "strftime" funksjonen:

```Gleam
import time
 
let nåværende_dato = time.now()
 
io.println(time.strftime("%A, %e %B %Y", nåværende_dato))
 
// Eksempel på utskrift: Wednesday, 22 September 2021
```

## Dykke dypere:

Funksjonen for å få nåværende dato er ikke bare begrenset til Gleam, men er en del av språket og globale standarder. I tillegg til ISO-8601 formatet, brukes alternativer som Unix timestamp (antall sekunder siden 1. januar 1970) og klokkeslett formatene AM/PM og 24-timers format.

Alternativt kan man bruke eksterne biblioteker for å håndtere datoer og klokkeslett, for eksempel "chronos" biblioteket for Gleam.

## Se også:

- [Gleam dokumentasjon om "time" biblioteket](https://gleam.run/documentation#time)
- [ISO-8601 datoformat](https://en.wikipedia.org/wiki/ISO_8601)
- [Chronos biblioteket for Gleam](https://github.com/gleam-lang/chronos)