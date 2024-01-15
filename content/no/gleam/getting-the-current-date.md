---
title:                "Å få gjeldende dato"
html_title:           "Gleam: Å få gjeldende dato"
simple_title:         "Å få gjeldende dato"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Why

Hvorfor bry seg om å få dagens dato?

Det kan være mange grunner til å ville vite dagens dato, enten man skal lage en kalender-app, følge med på tidsberegninger, eller bare ønsker å holde styr på tiden. Uansett årsak, kan Gleam gjøre dette enkelt for deg med sin innebygde funksjon for å hente dagens dato.

## How To

Det er enkelt å få tak i dagens dato i Gleam. Du trenger bare å bruke funksjonen `Calendar.Date.today()` for å hente dagens dato som en `Calendar.Date`-rekord. Her er et eksempel på hvordan du kan bruke denne funksjonen i et program:

```
Gleam import time
import gleam/calendar.{ Date }

pub fn main() {
  let today = Calendar.Date.today()
  time.log_debug("Dagens dato er: {today.day}.{today.month}.{today.year}")
}
```

Når programmet kjører, vil du få output som f.eks. `Dagens dato er: 5.9.2021`. Legg merke til at vi bruker `time.log_debug` for å logge datoen til terminalen. Du kan også bruke andre funksjoner for å formatere og manipulere datoen etter dine behov.

## Deep Dive

For de som ønsker å gå dypere inn i hvordan Gleam håndterer datoer, kan det være nyttig å vite at `Calendar.Date`-rekorden inneholder flere felt som kan komme til nytte. Disse inkluderer `day`, `month` og `year` som vi allerede har nevnt, samt `day_of_week` og `day_of_year`.

Gleam bruker koordinert universaltid (UTC) for å beregne datoer og tidspunkt, og håndterer også skuddår. Dette gjør at du kan være sikker på at datoen du får tilbake alltid vil være korrekt.

## See Also

- Offisiell Gleam-dokumentasjon for `Calendar.Date`: https://gleam.run/documentation/std_lib/calendar
- Gleam sin offisielle nettside: https://gleam.run/
- Gleam på GitHub: https://github.com/gleam-lang/gleam