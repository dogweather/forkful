---
title:                "Store bokstaver i en streng"
html_title:           "Bash: Store bokstaver i en streng"
simple_title:         "Store bokstaver i en streng"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Å ha riktig utforming på tekst kan være viktig av ulike årsaker, enten det er for å gjøre den mer lesbar eller for å følge et bestemt format. En vanlig formateringsmetode er å ha store bokstaver i begynnelsen av ord, også kalt "Kapitalisering". I Bash kan du enkelt endre en streng til å bli kapitalisert ved hjelp av noen kodelinjer.

## Hvordan

For å kapitalisere en streng i Bash, kan du bruke kommandoen `tr`. Her er et eksempel på hvordan du kan bruke den:

```Bash
input="dette er en test"
output=$(tr '[:lower:]' '[:upper:]' <<< "$input")
echo "$output"
```

I dette eksempelet definerer vi en variabel `input` som inneholder en streng. Ved å bruke `tr` og spesifisere både små og store bokstaver, vil kommandoen kapitalisere alle bokstavene i strengen vår. Outputet blir deretter lagret i variabelen `output` og deretter skrevet ut ved hjelp av `echo` kommandoen.

## Dypdykk

Det er viktig å merke seg at `tr` kommandoen i Bash ikke bare kan kapitalisere en hel streng, men også bytte ut en bestemt del av en streng med en annen. Ved å bruke den riktige syntaksen, kan du endre en tekst til å oppfylle dine spesifikke behov.

En annen nyttig kommando du kan bruke for å kapitalisere strenger er `sed`. Denne kommandoen er spesielt nyttig hvis du vil kapitalisere bare første bokstav i hvert ord i en streng. Her er et eksempel:

```Bash
input="dette er en test"
output=$(sed -e 's/\b\(.\)/\u\1/g' <<< "$input")
echo "$output"
```

Med denne kommandoen vil `sed` kapitalisere første bokstav i hvert ord i strengen vår, og returnere "Dette Er En Test".

## Se også

- [Bash-siden på WikiBooks](https://en.wikibooks.org/wiki/Bash_Shell_Scripting)
- [Offisiell dokumentasjon for Bash](https://www.gnu.org/software/bash/manual/bash.pdf)