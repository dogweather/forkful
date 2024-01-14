---
title:                "Fish Shell: Å få gjeldende dato"
simple_title:         "Å få gjeldende dato"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor

Å få gjeldende dato kan være nyttig for å holde orden på filer og mapper, for å lage tidsstempler i logger, eller for å planlegge fremtidige oppgaver. Ved hjelp av Fish Shell, kan du enkelt få tak i dato og tid ved å bruke noen enkle kommandoer. 

## Hvordan

Bruk ```date``` kommandoen for å få gjeldende dato:

```
Fish Shell: date
Output: Man Mar 29 12:45:07 EDT 2021
```

Med ```%Y``` format flagg, kan du få dato i et mer leselig format:

```
Fish Shell: date +%Y
Output: 2021
```

Du kan også få gjeldende tid ved å bruke ```+%H:%M```:

```
Fish Shell: date +%H:%M
Output: 12:48
```

For å få både dato og tid sammen, bruk ```+%c```:

```
Fish Shell: date +%c
Output: Man Mar 29 12:50:39 EDT 2021
```

Det er også mulig å få gjeldende ukedag ved å bruke ```+%a```:

```
Fish Shell: date +%a
Output: Man
```

## Dypdykk

Fish Shell tillater også mer avansert manipulering av dato og tid ved hjelp av ```date``` kommandoen. For eksempel kan du bruke ```-d``` flagg for å få dato og tid fra en bestemt dag eller tidsperiode. Du kan også bruke ```-I``` for å få dato i ISO format, eller ```-R``` for å få dato og tid i RFC 2822 format.

## Se også

- [Offisiell Fish Shell dokumentasjon](https://fishshell.com/docs/current/index.html)
- [Guide til terminalemulering med Fish Shell](https://fishshell.com/docs/current/tutorial.html)
- [Introduksjon til shell programmering](https://fishshell.com/docs/current/scripting.html)