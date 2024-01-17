---
title:                "Interpolering av en streng"
html_title:           "C: Interpolering av en streng"
simple_title:         "Interpolering av en streng"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/c/interpolating-a-string.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Interpolering av strenger er en metode for å sette inn dynamiske verdier i ulike tekststrenger. Dette gjøres ved å kombinere statiske tekststrenger med variabler som inneholder informasjon som trengs i teksten. Dette er en nyttig teknikk som gjør programmering mer fleksibelt og effektivt.

## Hvordan:

Interpolering av strenger i C gjøres ved å bruke spesielle formatteringsmellomrom, kalt "placeholders". Disse mellomrommene markeres med et prosenttegn (%) og en spesifisert angivelse av datatypen som skal plasseres der.

```C
int alder = 25;
char *navn = "Per";

printf("%s er %d år gammel.", navn, alder);
```

Dette vil gi følgende utput:

```C
Per er 25 år gammel.
```

## Dypdykk:

Interpolering av strenger i C ble introdusert i versjon C99 av programmeringsspråket. Tidligere måtte man bruke funksjoner som `sprintf` eller `strcat` for å oppnå samme resultat, noe som var mer komplisert og ineffektivt.

Det finnes også alternative metoder for å sette inn variabler i tekststrenger, for eksempel konkatenering ved hjelp av `+`-operatoren. Men interpolering er å foretrekke da det gir en mer ryddig og oversiktlig kode.

Implementeringen av interpolering av strenger i C er basert på en del av språket som heter variadiske funksjoner, som tillater bruk av et variabelt antall argumenter.

## Se også:

For mer informasjon og eksempler på interpolering av strenger i C, se:

- [C99 Standard at ISO.org](https://www.iso.org/standard/29237.html)
- [C99 Standard at ISO.org (PDF)](https://www.iso.org/standard/29237.html?browse=tc)
- [C String Formatting at TutorialsPoint](https://www.tutorialspoint.com/c_standard_library/c_function_sprintf.htm)