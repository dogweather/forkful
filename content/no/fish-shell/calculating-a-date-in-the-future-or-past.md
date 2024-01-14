---
title:    "Fish Shell: Å beregne en dato i fremtiden eller fortiden"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## Hvorfor

Å beregne en dato i fremtiden eller fortiden kan være nyttig for å organisere og planlegge aktiviteter. Med Fish Shell er dette enkelt å gjøre ved hjelp av noen enkle kommandoer.

## Hvordan

For å regne ut en dato i fremtiden eller fortiden i Fish Shell, kan du bruke kommandoen `date` sammen med noen flagg for å spesifisere ønsket dato og format.

For å regne ut en dato 7 dager frem i tid, kan du bruke følgende kommando:

```Fish Shell
date -v +7d +%d.%m.%Y
```

Her bruker vi flagget `-v` for å spesifisere antall dager vi ønsker å legge til, og flagget `+%d.%m.%Y` for å formatere datoen som dag.måned.år.

Liknende, for å regne ut en dato 2 uker tilbake i tid, kan du bruke følgende kommando:

```Fish Shell
date -v -2w +%A, %d. %B %Y
```

Her bruker vi flagget `-v` igjen, men denne gangen med negativt tall for å få en dato i fortiden. Vi bruker også flagget `+%A, %d. %B %Y` for å formatere datoen som ukedag, dag. måned år.

## Dypdykk

Ved å bruke flagget `-v` kan du også legge til eller trekke fra andre enheter enn bare dager. For eksempel kan du bruke `-v+5m` for å legge til 5 måneder, eller `-v-3y` for å trekke fra 3 år.

I tillegg kan du også bruke flagget `-f` for å spesifisere en annen dato enn dagens dato som utgangspunkt for beregning. For eksempel, for å regne ut en dato 10 dager fra og med 1. januar 2022, kan du bruke følgende kommando:

```Fish Shell
date -v +10d -f 01.01.2022 +%d.%m.%Y
```

For enda mer detaljert informasjon om hvordan man beregner datoer i Fish Shell, kan du lese dokumentasjonen på [Fish Shell sin nettside](https://fishshell.com/docs/current/cmds/date.html).

## Se også

- [Fish Shell sin nettside](https://fishshell.com/)
- [Fish Shell sin dokumentasjon](https://fishshell.com/docs/current/index.html)