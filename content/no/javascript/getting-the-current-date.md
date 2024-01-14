---
title:    "Javascript: Å få nåværende dato"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

##Hvorfor
I de fleste programmeringsspråk er det en innebygd funksjon for å hente ut dagens dato og klokkeslett. Dette er nyttig fordi det lar oss spesifisere nøyaktig når et program skal kjøre eller når en hendelse skal skje. I denne bloggposten vil vi se nærmere på hvordan man kan bruke Javascript for å få tak i den nåværende datoen.

##Hvordan bruke
For å få den nåværende datoen i Javascript, kan du bruke Date-objektet. Dette objektet inneholder en rekke metoder for å få tak i dato og klokkeslett. Her er et eksempel på hvordan du kan hente ut den nåværende datoen og formatere den til en lesbar form:
```Javascript
let today = new Date();
let day = today.getDate();
let month = today.getMonth()+1;
let year = today.getFullYear();

console.log(`Dagens dato er ${day}.${month}.${year}.`);
// Output: Dagens dato er 5.10.2021.
```
Her bruker vi metoder som `getDate()`, `getMonth()` og `getFullYear()` for å få tak i den nødvendige informasjonen. Vi legger også til 1 til `getMonth()` siden det returnerer en verdi fra 0-11, der 0 er januar.

##Dypdykk
Det finnes flere metoder for å formatere datoen og klokkeslettet i Javascript. Her er noen eksempler:
- `getDay()`: returnerer en verdi fra 0-6, der 0 er søndag og 6 er lørdag.
- `getHours()`: returnerer timen i 24-timers format.
- `getMinutes()`: returnerer antall minutter.
- `getSeconds()`: returnerer antall sekunder.
- `getMilliseconds()`: returnerer antall millisekunder.

Det finnes også en rekke metoder for å sette en spesifikk dato eller klokkeslett ved hjelp av `set()`-metodene. Du kan utforske disse nærmere i dokumentasjonen for Javascript.

##Se også
- [Date-objektet i Javascript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [MDN Dokumentasjon for Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)