---
title:                "Bash: Å finne lengden av en streng"
simple_title:         "Å finne lengden av en streng"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å finne lengden på en tekststreng er en vanlig oppgave innen Bash-programmering. Det kan være nyttig når vi ønsker å behandle forskjellige tekststrenger på en effektiv måte, for eksempel ved å sortere dem eller søke gjennom dem. Ved å lære å finne lengden på en tekststreng i Bash, vil du ha et viktig verktøy i verktøykassen din for å håndtere tekstbehandling.

## Hvordan

For å finne lengden på en tekststreng i Bash, kan vi bruke kommandoen `expr length`, etterfulgt av selve tekststrengen innenfor gåseøyne. La oss se på et eksempel:

```Bash
str="Hei, verden!"
echo "Lengden på tekststrengen er: `expr length "$str"`"
```

Dette vil gi følgende output: `Lengden på tekststrengen er: 12`. Denne kommandoen teller også mellomrom og spesialtegn, så vær oppmerksom på dette når du bruker den.

## Dypdykk

Det er viktig å merke seg at `expr length` kun fungerer for å finne lengden på en enkelt tekststreng. Hvis du ønsker å finne lengden på en liste av tekststrenger, kan du bruke `wc` kommandoen i kombinasjon med en `here string` som inneholder alle tekststrengene. En `here string` er en spesiell type input hvor tekststrenger er separert av linjeskift. La oss se på et eksempel:

```Bash
str1="En"
str2="to"
str3="tre"
echo -e "$str1\n$str2\n$str3" | wc -c
```

Dette vil gi følgende output: `10`. `wc -c` teller antall tegn i inputen, som i dette tilfellet er `1`, `2`, `3` og to ekstra linjeskift. Ved å trekke fra disse to ekstra tegnene fra resultatet, vil vi få den totale lengden på de tre tekststrengene.

## Se også

- [Bash - Hvordan bruke variabelverdi i en annen variabel](https://www.davidkonsumerer.no/bash-hvordan-bruke-variabelverdi-i-en-annen-variabel/)
- [Bash User Guide - Text Processing](https://tiswww.case.edu/php/chet/bash/bashref.html#Text-Processing)
- [Bash man-siden for `expr`](https://www.man7.org/linux/man-pages/man1/expr.1.html)