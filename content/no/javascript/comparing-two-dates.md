---
title:    "Javascript: Sammenligne to datoer"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Hvorfor

Sammenligning av to datoer er en vanlig oppgave når man jobber med datoer i Javascript. Dette kan være nyttig for å sjekke om en dato kommer før eller etter en annen, eller om de er like. Det kan også være nyttig for å bestemme hvor lang tid det er mellom to datoer.

## Hvordan

For å sammenligne to datoer i Javascript, kan du bruke de innebygde funksjonene `new Date()` og `getTime()`. La oss se på et eksempel:

```Javascript
// Opprett to datoer
let date1 = new Date(2019, 6, 12); // 12 juli 2019
let date2 = new Date(2020, 6, 12); // 12 juli 2020

// Bruk getTime() for å få tiden i millisekunder
let time1 = date1.getTime();
let time2 = date2.getTime();

// Sammenlign tidene og få ut en tallverdi
let difference = time2 - time1;
```

I dette eksempelet oppretter vi to datoer og bruker `getTime()` for å få tiden i millisekunder. Vi trekker deretter fra den første datoen fra den andre for å få ut en positiv eller negativ tallverdi, som indikerer hvor lang tid det er mellom de to datoene i millisekunder.

```Javascript
// Kode for å sammenligne år
if (date1.getFullYear() < date2.getFullYear()) {
    console.log("Første dato kommer før andre dato.");
} else if (date1.getFullYear() > date2.getFullYear()) {
    console.log("Første dato kommer etter andre dato.");
} else {
    console.log("Datoene er like.");
}

// Kode for å sammenligne måneder
if (date1.getMonth() < date2.getMonth()) {
    console.log("Første dato kommer før andre dato.");
} else if (date1.getMonth() > date2.getMonth()) {
    console.log("Første dato kommer etter andre dato.");
} else {
    console.log("Datoene er like.");
}

// Kode for å sammenligne dager
if (date1.getDate() < date2.getDate()) {
    console.log("Første dato kommer før andre dato.");
} else if (date1.getDate() > date2.getDate()) {
    console.log("Første dato kommer etter andre dato.");
} else {
    console.log("Datoene er like.");
}
```

I disse kodestykkene bruker vi de innebygde funksjonene `getFullYear()`, `getMonth()` og `getDate()` for å sammenligne år, måneder og dager på en mer nøyaktig måte.

## Dykk dypere

Du kan også bruke andre metoder som `getHours()`, `getMinutes()` og `getSeconds()` for å sammenligne tidspunkt på en mer presis måte. Det er også viktig å merke seg at Javascript bruker en innebygd metode for å håndtere datoen "1. januar 1970" som en referanse for `getTime()`. Dette betyr at hvis du sammenligner en dato som kommer før dette, vil du få en negativ tallverdi og hvis du sammenligner en dato etter dette, vil du få en positiv tallverdi.

## Se også
- [MDN Web Docs: Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools: Date Objects](https://www.w3schools.com/js/js_dates.asp)
- [JavaScript.info: Date and time](https://javascript.info/date)