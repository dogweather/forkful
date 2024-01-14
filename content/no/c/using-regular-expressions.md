---
title:                "C: Å bruke regulære uttrykk"
programming_language: "C"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor 

Regular expressions, eller i norsk, regulære uttrykk, er et kraftig verktøy for å arbeide med tekststrenger i C-programmering. Det tillater enkelt å finne og manipulere spesifikke deler av en tekststreng basert på mønstre. Dette kan være svært nyttig når du må behandle store mengder data og ønsker å automatisere prosessen. 

## Hvordan 

For å bruke regulære uttrykk i C-programmering, må du inkludere `regex` biblioteket ved å skrive `#include <regex.h>` i begynnelsen av koden din. Deretter kan du bruke funksjonen `regcomp` for å kompilere uttrykket ditt og lagre det i en `regex_t` variabel. For eksempel, hvis du ønsker å finne alle tall i en tekststreng, kan du bruke følgende kode:

```C
regex_t regex;
char * pattern = "[0-9]+";
regcomp(&regex, pattern, REG_EXTENDED);
```

Deretter kan du bruke funksjonen `regexec` for å søke etter dette mønsteret i en tekststreng. Det vil returnere verdien `0` hvis det finner et samsvar, og `-1` hvis det ikke gjør det. For å få tilgang til den faktiske teksten som matcher mønsteret, kan du bruke `regmatch_t` strukturen og `regexec` funksjonen. Her er et eksempel på hvordan du kan skrive ut matchene dine på skjermen:

```C
char * input = "Det er 2021, og vi programmerer i C!";
regmatch_t matches;
if(regexec(&regex, input, 1, &matches, 0) == 0){
    printf("Fant match på indeks %d, med lengde %d\n", matches.rm_so, matches.rm_eo);
    printf("Matchet tekst: %.*s\n", (int)(matches.rm_eo - matches.rm_so), input + matches.rm_so);
}
```

Dette vil skrive ut følgende på skjermen:

```C
Fant match på indeks 6, med lengde 4
Matchet tekst: 2021
```

Du kan også bruke regulære uttrykk for å erstatte matcher i en tekststreng. Dette gjøres ved hjelp av `regreplace` funksjonen. Du kan se en fullstendig oversikt over funksjonene og deres parametere på [denne siden](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html#Regular-Expressions). 

## Dypdykk 

Regular expressions kan virke komplisert ved første øyekast, men de er utrolig nyttige når du blir mer kjent med dem. En av styrkene er at de tillater deg å søke etter mer komplekse mønstre enn bare en streng med tekst. For eksempel kan du bruke parenteser `()` for å gruppere deler av uttrykket ditt og bruke pipes `|` for å matche flere forskjellige alternativer. 

En annen viktig ting å merke seg er at det finnes forskjellige varianter av regulære uttrykk som for eksempel POSIX og Perl. Disse har litt forskjellig syntaks, så det er viktig å dobbeltsjekke hvilken versjon du bruker når du ser på dokumentasjonen for regulære uttrykk. Du kan lese mer om forskjellene mellom disse variantene på [denne siden](https://www.regular-expressions.info/posix.html). 

## Se også 

For å lære mer om regulære uttrykk i C-programmering, kan du se på følgende ressurser: 

- [Dokumentasjon for `regex.h`](https://www.gnu.org/software/libc/manual/html_node/Regular-Expressions.html) 
- [En interaktiv tutorial om regulære uttrykk](https://regexone.com/) 
- [En praktisk guide for å lage effektive regulære uttrykk](https://www.rexegg.com/regex-quickstart.html)