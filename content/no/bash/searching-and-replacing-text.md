---
title:                "Søke og erstatte tekst"
html_title:           "Bash: Søke og erstatte tekst"
simple_title:         "Søke og erstatte tekst"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor

Å utveksle tekster i en fil kan være en tidkrevende og kjedelig oppgave. Heldigvis er det enkelt å automatisere denne prosessen ved hjelp av Bash. Ved å bruke søke- og erstatningskommandoer kan du spare tid og få et mer effektivt arbeidsflyt.

## Hvordan

Søke- og erstatningskommandoene i Bash lar deg finne og erstatte spesifikk tekst i en fil. For å søke etter et bestemt ord, kan du bruke `grep` kommandoen som følger:

```Bash
grep "ord" filnavn
```
Dette vil søke etter ordet "ord" i filen "filnavn" og returnere alle linjene der den finnes.

For å erstatte ordet med et annet ord, kan du bruke `sed` kommandoen som følger:

```Bash
sed -i 's/ord/nytt_ord/g' filnavn
```

Dette vil erstatte alle forekomster av "ord" med "nytt_ord" i filen "filnavn". Det `-i` flagget sørger for at endringene blir lagret i filen.

## Dypdykk

Bash har en rekke ulike søke- og erstatningskommandoer, som `awk` og `perl`. Disse kommandoene tilbyr mer avanserte funksjoner og muligheter for å finne og erstatte tekst i en fil.

Det er også mulig å bruke regulære uttrykk (regex) for å spesifisere hva du skal søke etter og erstatte med. Dette kan være nyttig hvis du ønsker å søke etter et mønster istedenfor et spesifikt ord.

Bash tilbyr også muligheten til å endre flere filer på en gang. Du kan bruke `sed` kommandoen sammen med `find` kommandoen for å gjøre endringer i flere filer i én kommando.

## Se også

* [Bash offisiell nettside](https://www.gnu.org/software/bash/)
* [Bash Tutorial fra Linuxize](https://linuxize.com/post/bash-search-and-replace/)
* [Bash regex tutorial fra Digital Ocean](https://www.digitalocean.com/community/tutorials/using-grep-regular-expressions-to-search-for-text-patterns-in-linux)