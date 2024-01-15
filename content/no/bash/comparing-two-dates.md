---
title:                "Sammenligner to datoer"
html_title:           "Bash: Sammenligner to datoer"
simple_title:         "Sammenligner to datoer"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor?

Hvorfor vil man være interessert i å sammenligne to datoer? Det er flere grunner til dette, blant annet for å se om en dato kommer før eller etter en annen, eller for å avgjøre om en dato ligger i en bestemt tidsperiode.

## Hvordan?

For å sammenligne to datoer i Bash kan du bruke kommandoen `date -d` for å konvertere datoene til et tall som er enklere å sammenligne. La oss ta en titt på et eksempel:

```Bash
date1="2021-10-20"
date2="2021-11-05"

if [ $(date -d "$date1" +%s) -gt $(date -d "$date2" +%s) ]
then
    echo "$date1 er etter $date2"
else
    echo "$date1 er før $date2"
fi
```

Dette vil gi følgende output:

```Bash
2021-10-20 er før 2021-11-05
```

Her bruker vi `+%s` for å konvertere datoene til et tallformat, og deretter bruker vi `-gt` for å sammenligne dem. Du kan også bruke `-lt` for å sjekke om en dato er før en annen, eller `-eq` for å sjekke om de er like.

## Dypdykk

For å kunne sammenligne datoer på en mer avansert måte, kan du også bruke `date` kommandoen til å evaluere datoer basert på ulike formater og variabler. For eksempel kan du bruke variabelen `DATEMSK` til å sammenligne datoer basert på måned og dag:

```Bash
DATEMSK="+%m-%d"

date1="10-20"
date2="11-05"

if [ "$(date +%Y)-$date1" -gt "$(date +%Y)-$date2" ]
then
    echo "$date1 er etter $date2"
else
    echo "$date1 er før $date2"
fi
```

Her bruker vi `+%Y` for å legge til det nåværende året sammen med datoene, og sammenligner deretter basert på dette. Dette kan være nyttig hvis du prøver å sammenligne datoer som kommer tilbake hvert år, som bursdager eller årlige hendelser.

## Se også

- [GNU Bash dokumentasjon](https://www.gnu.org/software/bash/manual/)

- [Bash-programmering for nybegynnere](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)