---
title:                "Beregning av en dato i fremtiden eller fortiden"
html_title:           "Fish Shell: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å beregne en dato i fremtiden eller fortiden er en viktig funksjon for programmerere. Den lar oss å utføre komplekse matematiske beregninger for å finne ut tidspunktet for hendelser eller til å planlegge fremtidige handlinger. Programmører bruker dette ofte til å lage tidssensitive applikasjoner eller lage algoritmer for å forutsi fremtidige hendelser.

## Hvordan gjør man det?

For å beregne en dato i Fish Shell må man bruke kommandoen `date`. Denne kommandoen tar to argumenter: datoen man ønsker å beregne fra og et offset i form av dager, uker eller måneder. Her er et eksempel på hvordan man kan beregne datoen 30 dager fra i dag:

```
Fish Shell: date -d "today + 30 days"
Resultat: Ma, 23 Jul 2018 13:36:19 CEST
```

Man kan også beregne datoen for en spesifikk hendelse, som for eksempel 5 dager før julaften:

```
Fish Shell: date -d "2018-12-24 - 5 days"
Resultat: Tors, 20 Des 2018 00:00:00 CET
```

## Dypdykk

Funksjonen for å beregne en dato i Fish Shell er basert på GNU versjonen av `date` kommandoen. Dette betyr at den har flere funksjoner og muligheter som ikke er tilgjengelig i den standardversjonen som følger med Mac eller andre operativsystemer. Det finnes også alternative måter å beregne datoer på, som for eksempel å bruke Python programmeringsspråkets innebygde moduler. Det er viktig å merke seg at `date` kommandoen i Fish Shell kan variere i syntax og funksjonalitet fra andre skall som Bash eller Zsh.

## Se også

- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/cmds/date.html)
- [GNU `date` kommandodokumentasjon](https://www.gnu.org/software/coreutils/manual/html_node/Examples-of-date.html)
- [Python `datetime` modul dokumentasjon](https://docs.python.org/3/library/datetime.html)