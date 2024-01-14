---
title:    "Fish Shell: Beregning av dato i fremtiden eller fortiden."
keywords: ["Fish Shell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor

Å beregne en dato i fortiden eller fremtiden kan være nyttig for å planlegge fremover eller se tilbake på fortiden. Det kan også være nyttig å ha en rasjonell måte å gjøre det på, som ved hjelp av Fish Shell.

## Hvordan

For å beregne en dato i fortiden eller fremtiden i Fish Shell, kan du bruke kommandoen `date`. Her er et eksempel på hvordan du kan bruke denne kommandoen for å beregne en dato 30 dager fra nå:

```Fish Shell
date -d "+30 days"
```

Dette vil gi deg et resultat som dette:

```shell
Tir Apr 21 11:19:12 CEST 2020
```

Du kan også beregne en dato i fortiden ved å bruke et negativt tall. For eksempel, hvis du vil finne datoen 30 dager tilbake i tid, kan du bruke følgende kommando:

```Fish Shell
date -d "-30 days"
```

Dette vil gi deg et resultat som dette:

```shell
Tor Feb 20 11:19:37 CET 2020
```

Her er noen andre nyttige eksempler på hvordan du kan bruke `date` kommandoen for å beregne ulike datoer:

- Beregne en dato 3 måneder fra nå: `date -d "+3 months"`
- Beregne en dato 1 uke fra nå: `date -d "+1 week"`
- Beregne en dato 2 timer fra nå: `date -d "+2 hours"`
- Beregne en dato 5 minutter fra nå: `date -d "+5 minutes"`

Du kan også bruke `date` sammen med andre kommandoer for å få mer spesifikke resultater. For eksempel, hvis du vil finne datoen 1 måned og 2 uker fra nå, kan du bruke denne kommandoen:

```Fish Shell
date -d "+1 month +2 weeks"
```

Dette vil gi deg et resultat som dette:

```shell
Søn Mai 03 11:21:01 CEST 2020
```

## Dypdykk

Det er viktig å forstå hvordan `date` kommandoen fungerer for å kunne beregne datoer i fortiden eller fremtiden nøyaktig. Når du bruker `-d` flagget, kan du angi datoen ved hjelp av mange forskjellige syntaks. Dette kan inkludere spesifikke kalenderdatoer, som "1st July" eller "June 30 2020", samt relative tidsenheter, som "1 day ago" eller "next week".

Det er også mulig å legge til flere tidsenheter for å få et mer spesifikt resultat. For eksempel, hvis du vil finne datoen 6 måneder, 5 uker og 3 dager fra nå, kan du bruke denne kommandoen:

```Fish Shell
date -d "+6 months +5 weeks +3 days"
```

## Se også

- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/)
- [Bash FAQ](https://mywiki.wooledge.org/BashFAQ)
- [Hvordan beregne en dato i Python](https://stackabuse.com/how-to-calculate-future-or-past-dates-in-python/)