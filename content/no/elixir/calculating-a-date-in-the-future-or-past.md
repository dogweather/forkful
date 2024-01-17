---
title:                "Beregning av dato i fremtid eller fortid"
html_title:           "Elixir: Beregning av dato i fremtid eller fortid"
simple_title:         "Beregning av dato i fremtid eller fortid"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hva & hvorfor?

Å beregne en dato i fremtiden eller fortiden er et vanlig problem for programmerere. Det kan være nyttig for å utvikle funksjoner som tidsstyrte tjenester eller for å vise datoer på en nettside basert på brukerens tidssone.

## Slik gjør du det:

For å beregne en dato i fremtiden eller fortiden i Elixir, kan du bruke funksjonen `Date.shift/3` og angi antall dager fra nå eller antall dager siden. For eksempel:

```Elixir
Date.shift(Date.utc_today(), 10) # Beregner datoen 10 dager frem i tid
=> ~D[2020-09-21]

Date.shift(Date.utc_today(), -5) # Beregner datoen 5 dager tilbake i tid
=> ~D[2020-09-06]
```

Hvis du vil beregne en dato basert på en annen dato, kan du bruke funksjonen `Date.add/2` og angi antall dager du vil legge til. Eksempel:

```Elixir
Date.add(~D[2020-09-15], 3) # Beregner datoen 3 dager etter 15. september 2020
=> ~D[2020-09-18]
```

## Dypdykk:

Historisk sett har beregning av datoer vært komplisert på grunn av ulike kalendere og tidssoner. Men i dag er det mange biblioteker og verktøy tilgjengelig for å enkelt håndtere forskjellige datoformater og tidssoner i Elixir.

En annen måte å beregne datoer på er å bruke funksjonen `Date.diff/2` som lar deg beregne antall dager mellom to datoer. 

## Se også:

- Elixir's offisielle dokumentasjon om [Date modulen](https://hexdocs.pm/elixir/Date.html)
- [Time](https://hexdocs.pm/ecto/Ecto.Time.html) biblioteket for håndtering av tidssoner i Elixir.
- [Calendars](https://github.com/lsm/calendars) biblioteket for å håndtere forskjellige kalendere i Elixir.