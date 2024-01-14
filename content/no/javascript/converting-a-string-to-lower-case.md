---
title:    "Javascript: Konvertere en streng til små bokstaver"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Hvorfor

Det å konvertere en string til lower case er en viktig del av JavaScript programmering fordi det lar deg gjøre strenger sammenlignbare med hverandre. I tillegg, mange funksjoner og metoder fungerer kun med lower case strenger, så å konvertere et string til lower case er ofte nødvendig for å få koden din til å fungere.

## Hvordan gjøre det

For å konvertere en string til lower case i JavaScript, kan du bruke `toLowerCase()` funksjonen. Denne funksjonen tar en string som parameter og returnerer den samme stringen, men med alle bokstavene omgjort til lower case.

```Javascript
let name = "Marie";
let lowerCaseName = name.toLowerCase();
console.log(lowerCaseName);
```
Output: marie

Her har vi definert en variabel `name` med verdien "Marie" og brukt `toLowerCase()` funksjonen for å konvertere den til lower case. Deretter har vi lagret resultatet i en ny variabel `lowerCaseName` og brukt `console.log()` for å skrive ut den nye strengen til konsollen.

Det er også mulig å bruke metoden `toLowerCase()` direkte på en string uten å måtte lagre det i en ny variabel.

```Javascript
let name = "MARIE";
console.log(name.toLowerCase());
```
Output: marie

## Fordypning

Det er viktig å merke seg at `toLowerCase()` funksjonen vil bare konvertere bokstaver som er en del av det engelske alfabetet. Bokstaver fra andre språk vil ikke bli omgjort til lower case, så hvis du jobber med flere språk, bør du være forsiktig med hvordan du håndterer strenger og konverteringer.

I tillegg vil `toLowerCase()` funksjonen bare endre bokstaver til lower case. Hvis strengen din inneholder tall, symbolske tegn eller mellomrom, vil disse bli igjen uendret.

En annen ting å merke seg er at `toLowerCase()` funksjonen ikke endrer den opprinnelige strengen, den returnerer bare en ny lower case versjon av den. Hvis du vil endre den originale strengen, må du lagre den nye lower case strengen i samme variabel.

## Se også

- [W3Schools - JavaScript String toLowerCase() Method](https://www.w3schools.com/jsref/jsref_tolowercase.asp)
- [MDN Web Docs - String.prototype.toLowerCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [JavaScript.info - Strings and arrays](https://javascript.info/string)
- [JavaScript.com - String Methods](https://www.javascript.com/learn/strings/methods)