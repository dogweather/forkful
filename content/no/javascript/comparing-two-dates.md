---
title:                "Sammenligning av to datoer"
html_title:           "Javascript: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å sammenligne to datoer er en vanlig oppgave i webutvikling, spesielt når man jobber med tidssensitive data som for eksempel datoer for arrangementer eller viktige hendelser. Ved å sammenligne to datoer kan man enkelt finne ut om de er like, eller hvilken dato som kommer først eller sist. Dette er nyttig for å organisere og presentere data på en mer forståelig måte for brukeren.

## Hvordan

For å sammenligne to datoer i Javascript, bruker vi ofte det innebygde Date-objektet. La oss se på et eksempel:

```javascript
const dato1 = new Date('2021-08-20');
const dato2 = new Date('2021-08-15');
if (dato1 > dato2) {
  console.log('Dato 1 er senere enn dato 2');
} else if (dato2 > dato1) {
  console.log('Dato 2 er senere enn dato 1');
} else {
  console.log('Dato 1 og dato 2 er like');
}
```

I dette eksempelet har vi opprettet to Date-objekter og sammenlignet dem ved hjelp av sammenligningsoperatøren `>`. Denne sammenligningen vil returnere en boolsk verdi (true eller false) basert på hvilken dato som kommer først. Merk at vi kan også bruke andre sammenligningsoperatører som `<` og `===` for å sammenligne datoer.

I tillegg til sammenligningsoperatører, har Date-objektet også flere metoder som kan være nyttige for å sammenligne datoer. For eksempel kan du bruke `getDate()`-metoden for å få dagnummeret av en dato og sammenligne det med et annet datoobjekts dagnummer.

## Dypdykk

Det er viktig å merke seg at sammenligning av datoer kan være litt utfordrende på grunn av måten Javascript håndterer datatyper på. Forventer du kanskje at `new Date('2021-08-20') === new Date('2021-08-20')` skal returnere true? I så fall tar du feil. På grunn av måten Javascript sammenligner objekter på, vil disse to datoobjektene bli betraktet som forskjellige og sammenligningen vil returnere falsk. For å løse dette, kan du bruke `getTime()`-metoden som returnerer et tall basert på antall millisekunder siden 1. januar 1970. Ved å sammenligne disse tallene kan du få riktig resultat.

Et annet viktig punkt å huske på er at datoer kan være forskjellige på tvers av tidssoner. Dette kan føre til uventede resultater hvis du ikke tar hensyn til tidssoner når du sammenligner datoer. Det kan derfor være lurt å ha et system for å håndtere tidssoner når du jobber med datoer i Javascript.

## Se også

- [MDN - Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [W3Schools - Date Comparison](https://www.w3schools.com/js/js_date_methods.asp)
- [Stack Overflow - Comparing Dates in Javascript](https://stackoverflow.com/questions/497790/how-do-you-compare-two-dates)