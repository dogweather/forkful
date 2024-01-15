---
title:                "Å konvertere en dato til en streng"
html_title:           "Fish Shell: Å konvertere en dato til en streng"
simple_title:         "Å konvertere en dato til en streng"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Noen ganger må man kanskje konvertere en dato til en streng i et programmeringsprosjekt. Dette kan være for å vise datoen i et bestemt format eller å sammenligne med en annen streng. I denne artikkelen vil vi se på hvordan man gjør dette i Fish Shell.

## Hvordan gjøre det

For å konvertere en dato til en streng i Fish Shell, kan man bruke kommandoen `date`. Denne kommandoen kan ta ulike parametere for å formatere datoen på ønsket måte. La oss se på noen eksempler:

```
Fish Shell 2020-09-01
date +"%Y-%m-%d"
```
Her vil output være `2020-09-01` siden vi har bedt om å få datoen i formatet år-måned-dag.

```
Fish Shell 01/09/2020
date +"%d/%m/%Y"
```
Her vil output være `01/09/2020` siden vi har bedt om å få datoen i formatet dag/måned/år.

Man kan også legge til andre parametere som for eksempel klokkeslett eller tidsone i datoen. Dette kan være nyttig hvis man for eksempel ønsker å få datoen og tiden i et bestemt tidssone.

Det er også mulig å bruke variabler når man konverterer datoer. La oss si at vi har variablene `day`, `month` og `year` som inneholder dagens dato. Da kan man bruke disse i `date` kommandoen slik:

```
Fish Shell 1. september 2020
day=$(date +"%d")
month=$(date +"%B")
year=$(date +"%Y")
echo $day. $month $year
```

## Dypdykk

For de som er interessert i å dykke dypere inn i konvertering av datoer i Fish Shell, kan man også sjekke ut Fish Shell sin dokumentasjon. Der er det mulig å finne mer detaljert informasjon om de ulike parametere som kan brukes med `date` kommandoen.

## Se også

- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/cmds/date.html)