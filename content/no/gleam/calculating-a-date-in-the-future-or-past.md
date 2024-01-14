---
title:                "Gleam: Beregning av datoer i fremtiden eller fortiden"
programming_language: "Gleam"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/gleam/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor

Å beregne en dato i fremtiden eller fortiden kan være nyttig i mange forskjellige tilfeller. Det kan hjelpe deg med å planlegge fremtidige arrangementer eller få oversikt over historiske hendelser. Med Gleam programmeringsspråk, kan du enkelt utføre slike beregninger ved hjelp av innebygde funksjoner.

## Slik gjør du det

For å beregne en dato i fremtiden eller fortiden i Gleam, kan du bruke funksjonen `Calendar.add_days()` og `Calendar.subtract_days()`. Disse funksjonene tar inn to argumenter, antall dager og en startdato, og returnerer den beregnede datoen.

```Gleam
import gleam/calendar

let future_date = Calendar.add_days(30, Date.now())

let past_date = Calendar.subtract_days(14, Date.now())

IO.println("Fremtidig dato: %{future_date}")
// Fremtidig dato: 29.11.2021

IO.println("Tidligere dato: %{past_date}")
// Tidligere dato: 01.11.2021
```

Det er også mulig å beregne fremtidige eller fortidige datoer med andre intervaller, som uker, måneder eller år. Bare bytt ut `add_days()` eller `subtract_days()` med tilsvarende funksjoner som `add_weeks()` eller `subtract_months()`. Du kan også legge til eller trekke fra flere argumenter, for eksempel `Calendar.add_days(6, 30, Date.now())` for å beregne en dato som er 36 dager frem i tid.

## Dykk dypere

Hvis du ønsker å gjøre mer avanserte beregninger, kan du også bruke funksjonen `Calendar.from_date()`. Dette lar deg konvertere en dato til et kalenderobjekt, som du kan bruke til å utføre mer komplekse operasjoner. Du kan også skreddersy beregninger basert på klokkeslett, ukedag, og mye mer.

For mer informasjon om hvordan du bruker Gleam's innebygde kalenderfunksjoner, kan du sjekke ut dokumentasjonen vår [her](https://gleam.run/documentation/docs/libraries/calendar/index.html).

## Se også

- [Gleam Dokumentasjon](https://gleam.run/documentation/index.html)
- [Gleam GitHub Repo](https://github.com/gleam-lang/gleam)