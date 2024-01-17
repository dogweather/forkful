---
title:                "Å få dagens dato"
html_title:           "Bash: Å få dagens dato"
simple_title:         "Å få dagens dato"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å få den nåværende datoen er en vanlig oppgave for Bash-programmering. Dette er viktig for å kunne spore når en bestemt hendelse eller operasjon ble utført, og for å automatisk generere filnavn basert på datoen.

## Hvordan:
For å få den nåværende datoen, kan du bruke kommandoen `date` i Bash. Dette vil gi deg den nåværende datoen og klokkeslettet i standardformatet. Her er et eksempel på koden og resultatet:

```Bash
date
```

Output:

`Tir Sep 28 19:29:47 CEST 2021`

Dette vil variere avhengig av tidssone og systeminnstillinger.

Du kan også formatere utgangen ved å legge til en formatstreng som et argument til kommandoen `date`. For eksempel, for å få datoen i ISO-format (YYYY-MM-DD), kan du bruke følgende kode:

```Bash
date +%F
```

Output:

`2021-09-28`

For mer informasjon og mulige formateringsvalg kan du se `man date`.

## Dykk dypere:
Å få den nåværende datoen er enkelt, men det kan være nyttig å vite litt om historisk kontekst. Datoen ble først introdusert i UNIX-systemet på 1970-tallet og har utviklet seg siden da. Alternativt kan du også bruke kommandoen `cal` for å få et kalenderoversikt over en bestemt måned eller år.

Implementering av `date`-kommandoen er avhengig av systemet ditt og kan variere. På noen systemer bruker den faktisk `libcore` biblioteket for å beregne datoen.

## Se også:
- [`man date`](https://linux.die.net/man/1/date)
- [UNIX-dato og tid](https://en.wikipedia.org/wiki/Unix_time)