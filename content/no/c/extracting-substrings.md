---
title:                "Utvinning av substringer"
html_title:           "C: Utvinning av substringer"
simple_title:         "Utvinning av substringer"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Extracting substrings, eller å trekke ut delstrenger, er en metode som lar programmerere hente ut en del av en større tekststreng. Dette kan være nyttig når man ønsker å manipulere eller analysere en bestemt del av teksten, for eksempel for å filtrere ut data eller lage en ny tekst med kun relevante deler. Dette er et vanlig verktøy for å jobbe med tekst og gjør det enklere å utføre ulike oppgaver i en programmeringskontekst.

## Slik gjør du det:

Det finnes flere måter å ekstrahere delstrenger på i C, avhengig av hvilke funksjoner og biblioteker som er tilgjengelige. Her er et eksempel på hvordan man kan bruke `strncpy()`-funksjonen for å ekstrahere en del av en tekststreng og lagre den i en ny strengvariabel:

```C
char text[] = "Hei, dette er en tekststreng!";
char substring[10];
strncpy(substring, text + 5, 9);
printf("%s", substring);
```

Denne koden vil skrive ut "dette er ", altså delen av teksten som er fra og med det sjette tegnet og ut ni tegn framover. Merk at `strncpy()` fungerer ved å kopiere de første `n` tegnene fra en kildestreng til en målstreng, og kan derfor brukes til å ekstrahere delstrenger.

## Dypdykk:

Ekstrahering av delstrenger er en viktig del av tekstbehandling i programmeringsverdenen, og har vært brukt i lang tid. Tidligere, da datamaskiner var mindre og ressurser var begrensede, var det vanlig å bruke indekserte variabler for å lagre deler av teksten. Dette er fortsatt en mulighet, men funksjoner som `strncpy()` gjør det enklere å håndtere komplekse tekstoppgaver.

Det finnes også andre måter å ekstrahere delstrenger på i C, for eksempel ved å bruke `strtok()`-funksjonen fra `string.h` biblioteket. Denne funksjonen lar deg splitte en tekststreng ved hjelp av et bestemt tegn, og hente ut delene som separate strenger.

Det er viktig å være oppmerksom på at ekstrahering av delstrenger kan være litt utfordrende hvis man ikke er forsiktig med indeksering og håndtering av minne. Det er derfor viktig å alltid sjekke dokumentasjonen for de ulike funksjonene man bruker, og å teste koden grundig for å unngå feil.

## Se også:

* [Official C documentation for strncpy()](https://www.cplusplus.com/reference/cstring/strncpy/)
* [C string functions](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
* [C tutorial on string manipulation](https://www.tutorialspoint.com/cprogramming/c_strings.htm)