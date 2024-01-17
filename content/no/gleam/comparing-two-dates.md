---
title:                "Sammenligne to datoer"
html_title:           "Gleam: Sammenligne to datoer"
simple_title:         "Sammenligne to datoer"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/comparing-two-dates.md"
---

{{< edit_this_page >}}

# Hva & Hvorfor?
Sammenligning av to datoer er en vanlig oppgave for utviklere for å håndtere dato- og tidsdata på en nøyaktig og effektiv måte. Dette kan være nyttig for å sortere og filtrere data, eller for å beregne tidsintervaller og forfallsdatoer. Det er også en viktig del av å løse problemer og feil knyttet til tidsformater og tidssoner.

# Hvordan:
Gleam tilbyr ulike funksjoner og metoder for å sammenligne to datoer. Her er noen eksempler på hvordan du kan gjøre det:

```Gleam
import gleam/time

fremtidig_dato = time.DateTime.new(2022, 05, 15)
dagens_dato = time.now()

gleam/time.compare(dagens_dato, fremtidig_dato) // Output: time.Order.LessThan

gleam/time.difference(dagens_dato, fremtidig_dato, "days") // Output: -349
```

# Dykk dypere:
Mens datostyring er viktig for de fleste programmer, har det vært et konstant problem for programmerere å håndtere dato- og tidsdata riktig. Dette skyldes hovedsakelig ulike tidsstandarder og datomønstre som brukes over hele verden. Alternativer til Gleam for dato-og klokkeslett sammenligning inkluderer biblioteker som Moment.js og Luxon.

Gleam benytter seg av ISO 8601-standard for dato- og tidsformater, og implementerer funksjoner som gjør det enkelt å manipulere og sammenligne datoer og klokkeslett.

# Se også:
- Offisiell Gleam dokumentasjon for ```gleam/time``` biblioteket: https://gleam.run/modules/time
- ISO 8601-standard: https://www.iso.org/iso-8601-date-and-time-format.html