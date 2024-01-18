---
title:                "Oversetting av en dato fra en streng"
html_title:           "Javascript: Oversetting av en dato fra en streng"
simple_title:         "Oversetting av en dato fra en streng"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

**

## Hva og Hvorfor?
Parsing av dato fra en streng er en vanlig oppgave for programmører. Det innebærer å konvertere en tekstrepresentasjon av en dato til et objekt som kan behandles av dataprogrammer. Dette er nyttig for å kunne manipulere datoer og utføre operasjoner som å sortere eller filtrere data basert på datoer.

## Hvordan:
Det finnes flere måter å parse datoer fra en streng på i Javascript. En av de vanligste metodene er ved hjelp av `Date()` objektet og `Date.parse()` metoden. Her er et eksempel som viser hvordan du kan parse en dato fra en streng og lagre den i en variabel:

```Javascript
var datoString = "2021-10-14";
var datoObjekt = new Date(Date.parse(datoString));

console.log(datoObjekt); // Output: Thu Oct 14 2021 00:00:00
```

En annen måte å parse en dato på er ved hjelp av `Date()` konstruktøren og å spesifisere år, måned og dag som argumenter. Dette gir også mulighet for å angi klokkeslett og tidsone. Her er et eksempel på hvordan du kan opprette et `Date()` objekt ved å parse en dato fra en streng på denne måten:

```Javascript
var datoString = "2021-10-14T14:30:45";
var datoObjekt = new Date(datoString);

console.log(datoObjekt); // Output: Thu Oct 14 2021 14:30:45 GMT+0200 (CEST)
```

## Dypdykk:
Parsing av datoer fra en streng har eksistert siden de tidligste versjonene av Javascript. Første utgave av språket, som ble utgitt i 1995, inkluderte funksjoner for å konvertere datoer fra tekst til dataverdier. Siden da har det blitt lagt til flere alternativer for å utføre denne oppgaven, som `moment.js` biblioteket.

En annen alternativ metode for å parse datoer fra en streng i Javascript er ved hjelp av `Intl` objektet, som gir mulighet for å parse datoer basert på ulike kulturinnstillinger som språk og region.

Implementasjonen av parsing av datoer fra en streng i Javascript kan variere mellom ulike nettlesere og enheter, noe som kan føre til uventede resultater. Det er derfor viktig å teste koden din for å sikre at den fungerer som forventet på alle enheter.

## Se også:
For mer informasjon om parsing av datoer fra en streng i Javascript, kan du besøke følgende nettsider:

- [W3Schools: Javascript Date Objects](https://www.w3schools.com/jsref/jsref_obj_date.asp)
- [MDN web docs: Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [moment.js dokumentasjon](https://momentjs.com/docs/)