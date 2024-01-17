---
title:                "Å søke og erstatte tekst"
html_title:           "Fish Shell: Å søke og erstatte tekst"
simple_title:         "Å søke og erstatte tekst"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Søking og erstatning av tekst, også kjent som "search and replace", er en viktig del av programmering. Det innebærer å finne et bestemt ord eller mønster i en tekst og erstatte det med et annet. Dette kan være nyttig for å gjøre endringer i store mengder tekst eller å feilsøke kode. Det er derfor en viktig ferdighet for alle programutviklere å ha.

## Hvordan:
Koding av søk og erstatning i Fish Shell er enkelt og effektivt. Her er et eksempel på hvordan du kan bruke kommandoen i terminalen:

 ```Fish Shell
 sed -i 's/ord1/ord2/g' filnavn
 ```
Dette vil søke gjennom tekstfilen "filnavn" og erstatte alle forekomster av "ord1" med "ord2". Den globale flaggen "g" sørger for at alle forekomster blir erstattet, ikke bare den første.

## I dybden:
Søk og erstatning funksjonen finnes ikke bare i Fish Shell, men også i de fleste andre tekstredigeringsprogrammer. Selv om alternativer som "grep" og "awk" også kan brukes, er "sed" et mye mer kraftig og fleksibelt verktøy, spesielt for større tekstfiler. Fish Shell tilbyr en brukervennlig tilnærming til bruk av "sed" gjennom kommandoen som nevnt ovenfor.

## Se også:
- [Fish Shell dokumentasjon](https://fishshell.com/docs/current/cmds/sed.html)
- [Historisk bakgrunn på søk og erstatning](https://en.wikipedia.org/wiki/Search_and_replace)
- [Alternativer til søk og erstatning i Fish Shell] (https://fishshell.com/docs/current/index.html#alternative-command-line-tools)