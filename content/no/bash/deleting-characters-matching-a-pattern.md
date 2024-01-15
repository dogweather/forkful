---
title:                "Slette tegn som matcher et mønster"
html_title:           "Bash: Slette tegn som matcher et mønster"
simple_title:         "Slette tegn som matcher et mønster"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Det er flere grunner til hvorfor du kan ønske å slette karakterer som matcher et visst mønster i Bash. Noen ganger kan dette være nødvendig for å rydde opp i en fil eller en tekststreng, eller for å filtrere ut uønsket informasjon.

## Hvordan

For å slette karakterer som matcher et mønster i Bash, kan du bruke kommandoen "sed" (stream editor). Denne kommandoen kan brukes til å manipulere og endre tekstfiler.

```Bash
sed 's/[mønster]/[tomt]/g' filnavn.txt
```

La oss si at du har en fil med navnet "liste.txt" som inneholder følgende informasjon:

```
1. Kari
2. Petter
3. Line
```

Hvis du ønsker å slette alle tallene i denne filen, kan du bruke følgende kommando:

```Bash
sed 's/[0-9]//g' liste.txt
```

Dette vil resultere i at filen kun inneholder navnene, uten tallene foran.

```
Kari
Petter
Line
```

## Deep Dive

Når du bruker "sed" kommandoen til å slette karakterer som matcher et mønster, er det viktig å forstå hvordan det fungerer. Kommandoen tar i bruk regulære uttrykk, som brukes til å beskrive et mønster av karakterer.

I eksempelet over brukte vi [0-9] for å matche alle tall. Dette uttrykket betyr "match alle tall mellom 0 og 9". Hvis du ønsker å lære mer om regulære uttrykk, kan du se på dette nettstedet: https://www.regular-expressions.info/

## Se Også

- [Linux Bash Command Line Cheat Sheet](https://www.guru99.com/linux-bash-command-cheat-sheet.html)
- [RegExr - Online Regular Expression Testing Tool](https://regexr.com/)