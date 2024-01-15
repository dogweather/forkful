---
title:                "Beregning av en dato i fremtiden eller fortiden"
html_title:           "Elixir: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor

Å kunne beregne en dato i fremtiden eller fortiden er en viktig ferdighet for å håndtere tidsfølsomme oppgaver i programmering. Dette kan inkludere å planlegge fremtidige oppgaver, håndtere tidsbegrensede data og mye mer.

## Hvordan

Å beregne en dato i Elixir er enkelt og intuitivt. Alt du trenger å gjøre er å bruke funksjonen `:calendar.date_to_gregorian_days/1` for å konvertere en dato til dens tilsvarende dag i den gregorianske kalenderen. Deretter kan du bruke `:calendar.gregorian_days_to_date/1` for å konvertere ønsket antall dager tilbake eller fremover fra den opprinnelige datoen. La oss se på et eksempel:

```Elixir
iex> start_date = {2019, 03, 20}
iex> days_to_add = 7
iex> future_date = start_date |> :calendar.date_to_gregorian_days() |> Kernel.+(days_to_add) |> :calendar.gregorian_days_to_date()
```

I dette tilfellet vil `future_date` bli `{2019, 03, 27}`, som er 7 dager frem i tiden fra `start_date`. Du kan også beregne datoer i fortiden ved å bruke `-` operatøren på stedet for `+`.

## Dypdykk

Elixir har også innebygde funksjoner for å håndtere skuddår og å konvertere datoer til ulike kalendersystemer. Du kan også bruke `:calendar.week_start/1` for å angi hvilken ukedag som er den første i uken, og `:calendar.set_gregorian_change/1` for å endre når den gregorianske kalenderen ble innført. Dette er nyttige verktøy for å håndtere ulike behov og spesifikasjoner knyttet til datoer i programmene dine.

## Se også

- Offisiell Elixir dokumentasjon for å arbeide med datoer: https://hexdocs.pm/elixir/Calendar.html
- Elixir Cheatsheet for en rask oversikt over Elixir funksjoner: https://devhints.io/elixir
- Stack Overflow-spm. om å beregne datoer i Elixir: https://stackoverflow.com/questions/3336551/2012-1-1-to-a-date-in-elixir