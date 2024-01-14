---
title:                "Javascript: Ekstrahering av delstrenger"
simple_title:         "Ekstrahering av delstrenger"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/extracting-substrings.md"
---

{{< edit_this_page >}}

# Hvorfor

Et av de mest nyttige konseptene i Javascript-programmering er å kunne håndtere tekststrenger på en effektiv måte. Ofte trenger vi bare en del av en tekststreng, for eksempel for å hente ut et navn eller et tall fra en lengre streng. For å gjøre dette, er det viktig å kunne utvinne substrings, eller deler av strenger.

# Slik gjør du det

Å utvinne substrings i Javascript er enkelt og kan gjøres ved hjelp av innebygde funksjoner. Vi kan bruke `.substring()`-metoden som tar to indekser som parametere og returnerer en ny substring basert på disse indeksene. For eksempel:

```Javascript
let tekst = "Hei, mitt navn er Johannes";
console.log(tekst.substring(12, 20));
```

Dette vil gi følgende utskrift:

```
Johannes
```

Vi kan også bruke `.slice()`-metoden, som fungerer på samme måte som `.substring()`, men gir også mulighet for å bruke negative indekser. Dette er nyttig hvis vi vil utvinne substrings fra slutten av en tekststreng. For eksempel:

```Javascript
let tall = "123456789";
console.log(tall.slice(-3));
```

Dette vil gi følgende utskrift:

```
789
```

Vi kan også bruke `.substr()`-metoden, som tar en startindeks og en lengde som parametere og returnerer en substring basert på disse. For eksempel:

```Javascript
let tekst = "Programmering er gøy!";
console.log(tekst.substr(14, 3));
```

Dette vil gi følgende utskrift:

```
gøy
```

# Dypdykk

Når vi utvinner substrings, er det viktig å huske på hvordan indekser fungerer i Javascript. Det første tegnet i en tekststreng har indeks 0, og den siste indeksen vil alltid være lengden på strengen minus 1. For å utvinne en substring fra en tekststreng som inneholder spesielle tegn som mellomrom, må vi være ekstra nøye med hvilke indekser vi bruker.

Det er også viktig å merke seg at alle disse metodene returnerer en ny tekststreng, og påvirker ikke den opprinnelige strengen. For å endre den opprinnelige strengen, kan vi bruke metoden `.replace()`.

# Se også

- [Offisiell dokumentasjon for substring-funksjoner i Javascript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/substring)
- [Tutorial på utvinning av substrings i Javascript](https://www.w3schools.com/jsref/jsref_substring.asp)
- [Praktiske eksempler på utvinning av substrings](https://www.javascripttutorial.net/javascript-string-substring/)