---
title:                "Javascript: Kapitalisering av en streng"
simple_title:         "Kapitalisering av en streng"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Enten du er nybegynner eller erfaren programmerer, så har du sannsynligvis støtt på en situasjon hvor du trenger å endre en tekststreng til store bokstaver. Kanskje du ønsker å lage en tittel til en nettside eller formatere et brukernavn. Uansett hva årsaken er, så er det viktig å vite hvordan du kan gjøre dette på en enkel og effektiv måte. Derfor skal vi i dag ta en titt på hvordan du kan kapitalisere en tekststreng ved hjelp av Javascript.

## Slik gjør du det

Før vi begynner å kode, la oss først se på hva vi ønsker å oppnå. La oss si at vi har en tekststreng som sier "javascript er gøy". Vi ønsker å endre denne til "Javascript er gøy". 

For å kunne gjøre dette, må vi først konvertere teksten til en array ved hjelp av "split" funksjonen. Dette vil gjøre det enklere for oss å manipulere teksten. 

```Javascript
let tekst = "javascript er gøy";
let tekstArray = tekst.split("");
```

Nå som vi har teksten som en array, kan vi begynne å endre de ulike elementene. Vi ønsker å endre det første elementet, altså "j", til stor bokstav. Dette kan vi gjøre ved å bruke "toUpperCase" funksjonen.

```Javascript
tekstArray[0] = tekstArray[0].toUpperCase();
```

Etter dette ønsker vi å sette sammen teksten igjen til en tekststreng ved hjelp av "join" funksjonen og legge til mellomrom mellom hvert ord.

```Javascript
let nyTekst = tekstArray.join(" ");
console.log(nyTekst);
// Output: Javascript er gøy
```

Med disse få linjene med kode har vi lykkes med å kapitalisere tekststrengen vår.

## Dykk dypere

Nå lurer du kanskje på hvorfor vi konverterte teksten til en array før vi endret den? Det er fordi arrays i Javascript er modifiserbare, noe som gjør det lettere å endre enkeltkomponenter. Dette trikset kan også brukes til mange andre situasjoner der du ønsker å endre deler av en tekststreng.

Det er også verdt å nevne at det finnes en innebygd funksjon i Javascript for å kapitalisere en tekststreng, nemlig "toUpperCase" funksjonen. Men hvis du ønsker å gjøre dette på en dynamisk måte, for eksempel ved å bruke variabler, så kan trikset med å konvertere teksten til en array være nyttig.

## Se også

- [Javascript String Methods](https://www.w3schools.com/js/js_string_methods.asp)
- [Stack Overflow - How to capitalize a word in Javascript?](https://stackoverflow.com/questions/1026069/how-to-capitalize-the-first-letter-of-each-word-in-a-string)
- [MDN Web Docs - String.prototype.toUpperCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)