---
title:                "Bash: Sletting av tegn som matcher et mønster"
programming_language: "Bash"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Å slette tegn som matcher et mønster er en viktig ferdighet for å håndtere tekstbehandlingsoppgaver i Bash-programmering. Dette kan hjelpe deg med å forenkle og automatisere oppgaver som involverer å håndtere store mengder tekstfiler.

## Slik gjør du det

For å slette tegn som matcher et spesifikt mønster, kan du bruke kommandoen `sed` (stream editor). Her er et eksempel på hvordan du kan slette alle tall fra en tekstfil:

````Bash
sed 's/[0-9]//g' tekstfil.txt
````

I dette eksempelet bruker vi `sed` til å erstatte alle tall, representert av mønsteret `[0-9]`, med ingenting (altså sletter dem). Det siste argumentet "g" betyr å gjøre det på alle forekomstene i teksten. Outputet vil være den samme teksten, men nå uten tall.

## Dypdykk

Kommandoen `sed` er en kraftig verktøy for å håndtere tekstfiler i Bash-programmering. Ved å bruke regex (regular expression) mønstre, kan du spesifisere hvilke tegn du vil slette, erstatte eller beholde. Dette gjør det mulig å håndtere komplekse filer på en enkel og effektiv måte.

I tillegg til `sed` kan du også bruke andre kommandoer som `tr` (translate) og `awk` (pattern scanning and text processing) til å slette tegn basert på mønstre. Utforske disse verktøyene vil hjelpe deg å bli en mer effektiv og avansert Bash-programmerer.

## Se også

* [Bash sed kommando dokumentasjon](https://www.gnu.org/software/sed/manual/sed.html)
* [Bash tr kommando dokumentasjon](https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html)
* [Bash awk kommando dokumentasjon](https://www.gnu.org/software/gawk/manual/gawk.html)