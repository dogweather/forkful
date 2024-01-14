---
title:                "Javascript: Å få dagens dato"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hvorfor
Det å få den nåværende datoen er en viktig del av programmering. Det kan være nyttig for å holde oversikt over tidsstempel, oppdatere innhold på nettsider og mye mer. Uansett hva grunnen er, er det viktig å vite hvordan man får tak i den nåværende datoen i Javascript for å kunne bruke den i programmene våre.

## Hvordan
Det er flere måter å få den nåværende datoen i Javascript på, avhengig av hva slags format du trenger den i. Her er noen eksempler med kode og eksempelutgang som viser deg hvordan du kan få tak i datoen på forskjellige måter:

```Javascript
// Få tak i den nåværende datoen i formatet "dd/mm/yyyy"
let d = new Date();
console.log(`${d.getDate()}/${d.getMonth() + 1}/${d.getFullYear()}`); // Output: 23/10/2021

// Få tak i den nåværende datoen som et ISO-format
let d = new Date();
console.log(d.toISOString()); // Output: 2021-10-23T16:34:09.607Z

// Få tak i datoen i en bestemt tidsone
let d = new Date();
console.log(d.toLocaleString("nb-NO", {timeZone: "Europe/Oslo"})); // Output: 23.10.2021, 18:34:09
```

Som du kan se, kan du justere formatet og tidszonen for å få den nåværende datoen akkurat slik du trenger den.

## Deep Dive
For å få en dypere forståelse av hvordan datoen fungerer i Javascript, er det viktig å vite at den blir lagret som en tallverdi som representerer antall millisekunder siden midnatt, 1. januar 1970. Dette kalles "epoch tiden". Så når du får tak i den nåværende datoen, blir det i bunn og grunn konvertert fra denne tallverdien til et mer leselig format.

Det er også verdt å nevne at spesielle tilfeller, som skuddår og tidssoner, kan påvirke den nåværende datoen og må tas hensyn til når du jobber med den.

## Se også
- [MDN - Date object](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools - Get Current Date](https://www.w3schools.com/js/js_date_methods.asp)
- [JavaScript.info - Date and Time](https://javascript.info/date)