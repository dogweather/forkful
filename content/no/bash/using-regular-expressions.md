---
title:                "Bash: Å bruke regulære uttrykk"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Regular expressions er et kraftig verktøy for å finne og manipulere tekst basert på mønstre. Ved å bruke regular expressions kan du raskt og effektivt søke gjennom store mengder tekst og utføre handlinger basert på visse kriterier. Dette gjør det til et uunnværlig verktøy for å automatisere oppgaver, forenkle dataanalyse og forbedre programmeringsferdighetene dine.

## Hvordan bruke regular expressions i Bash

For å bruke regular expressions i Bash, må du først aktivere deres støtte ved å bruke flagget `-E` i kommandolinjen eller ved å inkludere `shopt -s` `extglob` i starten av skriptet ditt. Deretter kan du begynne å lage og bruke dine egne uttrykk ved hjelp av metategn og spesielle karakterer.

For eksempel, hvis du vil finne alle ord som starter med "b" i en tekstfil, kan du bruke følgende kommando:

```Bash
grep -E '\<b\w*' tekstfil.txt
```
Dette vil hente alle linjer som inneholder et ord som starter med "b". La oss se på noen andre eksempler på bruk av regular expressions i Bash.

### Søk etter et spesifikt mønster

Du kan bruke `.` for å matche ett enkelt tegn og `*` for å matche null eller flere forekomster av det forrige tegnet. Dette gjør det mulig å finne et mønster eller et bestemt ord uansett hva som kommer før eller etter det.

```Bash
grep -E 'h.*lo' tekstfil.txt # dette vil matche både "hello" og "hallo"
```

### Søk etter flere alternativer

Du kan bruke `|` for å matche flere alternativer. Dette er nyttig når du vil finne forekomster av flere ord eller uttrykk.

```Bash
grep -E 'foo|bar|baz' tekstfil.txt # dette vil matche både "foo", "bar" og "baz"
```

### Bruk av variabler og uttrykk

Du kan også bruke variabler og uttrykk i regular expressions. For eksempel, hvis du vil finne alle linjer som inneholder et ord som starter med samme tegn som variabelen `$first_letter`, kan du bruke følgende kommando:

```Bash
first_letter="h"
grep -E "\<$first_letter\w*" tekstfil.txt # dette vil matche både "hello" og "hei"
```

## Dypdykk i regular expressions

Regular expressions kan være ganske komplekse og ha mange avanserte funksjoner og muligheter. Hvis du er interessert i å lære mer, kan du se på offisiell dokumentasjon for Bash's `grep` og `egrep` kommandoer, samt andre ressurser på nettet. Praksis gjør perfekt når det kommer til å mestre regular expressions, så ikke vær redd for å eksperimentere og prøve ut forskjellige ting.

## Se også

- [Bash Regular Expressions - Programmeringswiki](https://www.programmingwiki.com/Bash_Regular_Expressions)
- [Grep - Offisiell dokumentasjon](https://www.gnu.org/software/grep/manual/grep.html)
- [Viktige kommandoer i Bash - DigitalOcean](https://www.digitalocean.com/community/tutorials/an-introduction-to-useful-bash-commands)