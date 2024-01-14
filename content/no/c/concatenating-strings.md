---
title:    "C: Sammenføyning av strenger"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/c/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor?
String-konkatenering er en viktig del av programmering, spesielt når man jobber med å manipulere tekststrenger. Det lar deg kombinere flere strenger til en enkelt streng, som er nyttig for å lage komplekse utdata eller manipulere inputdata. Så hvis du jobber med tekstbehandling, er det viktig å vite hvordan man konkatenerer strenger i C.

## Hvordan?
Kodingseksempler viser den enkleste måten å konkatenerer strenger i C, ved å bruke funksjonen `strcat()`. Denne funksjonen tar to argumenter: strengen som skal utvides, og strengen som skal legges til.

Et eksempel på bruk av `strcat()` ville være:

```C
char string1[] = "Hei, ";
char string2[] = "verden!";
strcat(string1, string2);
```

Dette vil føre til at `string2` blir lagt til i `string1`, og resultatet vil være `Hei, verden!` Når disse to strengene blir konkateneret, vil det skape en ny string med alle tegnene fra begge strengene.

Dette er bare en enkel måte å konkatenerer strenger i C på. Det finnes også flere andre funksjoner, som `sprintf()` og `strncpy()`, som kan bli brukt til å konkatenerer strenger på forskjellige måter.

## Dypdykk
Når du konkatenerer strenger, er det viktig å sørge for at strengene har riktig størrelse og at det finnes nok plass til å kombinere dem. Hvis ikke, kan det føre til feil i programmet eller til og med krasj.

En annen viktig ting å huske på er at du må være forsiktig når du konkatenerer brukergenerert input, for å unngå potensielle sikkerhetsrisikoer som bufferoverflow. Det er derfor viktig å alltid validere og filtrere brukerinput før det konkateneres til andre strenger.

## Se også
- [C String Functions](https://www.tutorialspoint.com/c_standard_library/string_h.htm)
- [How to concatenate two strings in C](https://www.programiz.com/c-programming/library-function/string.h/strcat)
- [Secure coding in C](https://www.securecoding.cert.org/confluence/display/c/Secure+Coding+Standards)