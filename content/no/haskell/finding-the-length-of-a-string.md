---
title:                "Finne lengden på en streng"
html_title:           "Arduino: Finne lengden på en streng"
simple_title:         "Finne lengden på en streng"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/haskell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å finne lengden på en streng (tekst) betyr å telle antall tegn. Dette er særlig nyttig for feltvalidering, tekstbehandling og programmering av brukergrensesnitt.

## Hvordan gjøre det:

I Haskell finnes det en innebygget funksjon kalt `length` for å finne lengden på en streng. Her er et eksempel:

```Haskell
lengdenAvStrengen = length "Hallo, verden"
```

Når du kjører koden over, vil `lengdenAvStrengen` lagre verdien 13, som er antall tegn i strengen "Hallo, verden".

## Dype detaljer

Opprinnelsen til funksjonen `length` i Haskell går tilbake til det funksjonelle programmeringsprinsippet om å behandle data i det høyeste abstraksjonsnivået. Hvis vi hadde behov for å finne lengden på en streng på en lavere abstraksjon, kunne vi ha brukt en rekursiv funksjon eller en teller i en iterasjon. Men Haskell gir oss denne innebygde funksjonen for å øke kodelesbarheten og effektiviteten.

Alternative metoder for å finne en strenglengde kan inkludere å bruke en teller i en iterativ prosess, men `length`-funksjonen er vanligvis mer tids- og ressurseffektiv. I koden er 'length' implementert ved hjelp av en 'fold' operasjon, som effektivt itererer over listen (eller strengen) en gang, akkumulerer et resultat (lengden, i dette tilfellet).

## Se også

For mer informasjon om Haskell og strengbehandling, besøk følgende lenker:

1. Haskell Wiki: [Text Processing](https://wiki.haskell.org/Text_Processing)
2. Real world Haskell: [Coping with Strings](http://book.realworldhaskell.org/read/using-strings.html)
3. Stack Overflow thread: [How to count number of characters in a string in Haskell?](https://stackoverflow.com/questions/3963269/split-a-string-in-haskell)