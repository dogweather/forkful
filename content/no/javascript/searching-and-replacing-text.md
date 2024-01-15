---
title:                "Søking og erstatning av tekst"
html_title:           "Javascript: Søking og erstatning av tekst"
simple_title:         "Søking og erstatning av tekst"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hvorfor
Å søke og erstatte tekst er en vanlig oppgave når man jobber med programmering. Det lar deg enkelt gjøre endringer i en større mengde tekst på en gang, noe som kan spare deg for mye tid og krefter.

## Slik gjør du det
Søke og erstatte funksjonen i Javascript gjør det mulig å finne et bestemt uttrykk i en tekststreng og erstatte det med et annet uttrykk. Dette kan gjøres ved hjelp av den innebygde metoden `replace()` som tar to parametere - uttrykket du ønsker å bytte ut, og det nye uttrykket du ønsker å erstatte det med. La oss se på et enkelt eksempel:

```Javascript
let tekst = "Jeg elsker å programmere i Javascript!";
let nyTekst = tekst.replace("Javascript", "Python");
console.log(nyTekst);
```

I dette eksempelet har vi definert en variabel `tekst` med en tekststreng. Deretter bruker vi `replace()` metoden til å erstatte "Javascript" med "Python". Det nye uttrykket lagres i variabelen `nyTekst` og blir deretter logget ut, noe som vil gi følgende output:

```
Jeg elsker å programmere i Python!
```

Søke og erstatte funksjonen i Javascript tar også i bruk såkalte regulære uttrykk, som gjør det mulig å søke etter mer komplekse mønstre i en tekststreng. Her er et eksempel som viser hvordan du kan bytte ut alle tall i en tekststreng med "X":

```Javascript
let tall = "123 er et tall og 456 er et annet tall.";
let nyTekst = tall.replace(/[0-9]/g, "X");
console.log(nyTekst);
```

I dette eksempelet bruker vi regulære uttrykk og `g` flagget (global) for å erstatte alle tall fra 0 til 9 med "X". Outputen vil bli:

```
XXX er et tall og XXX er et annet tall.
```

## Dypdykk
Nå som vi har sett på noen enkle eksempler på hvordan søke og erstatte funksjonen fungerer, la oss dykke dypere inn i dette emnet. `replace()` metoden i Javascript tar også i bruk to spesielle flagg - `i` for å gjøre søket case-insensitive (ikke skille mellom store og små bokstaver) og `m` for å gjøre søket flerlinjet. Du kan også gi `replace()` metoden en funksjon som argument i stedet for et statisk uttrykk. Dette kan være nyttig når du ønsker å lage et mer fleksibelt søk og erstatte system. Her er et eksempel på en slik implementering:

```Javascript
let tekst = "Javascript er et fantastisk programmeringsspråk!";
let nyTekst = tekst.replace(/javascr[ıı]pt/gi, match => match.toUpperCase());
console.log(nyTekst);
```

I dette eksempelet bruker vi et regulært uttrykk som gjør søket case-insensitive, noe som betyr at både "Javascript" og "javascript" vil bli erstattet. Vi bruker også `.toUpperCase()` metoden i funksjonen som argument for å gjøre det erstattede uttrykket til store bokstaver. Outputen vil bli:

```
JAVASCRIPT er et fantastisk programmeringsspråk!
```

## Se også
- [MDN - String replace() method](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [W3Schools - JavaScript String replace() Method](https://www.w3schools.com/jsref/jsref_replace.asp)