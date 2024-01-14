---
title:                "Javascript: Sammenligning av to datoer"
simple_title:         "Sammenligning av to datoer"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

Når man jobber med datoer i Javascript, kan det ofte være behov for å sammenligne to datoer. Dette kan være nyttig for å finne ut om en dato er tidligere, senere eller lik en annen dato. Å sammenligne datoer er også viktig for å sortere dem i riktig rekkefølge eller for å filtrere ut bestemte datoer i en liste.

## Hvordan

For å sammenligne to datoer i Javascript, bruker vi de innebygde metoder for `Date`-objektet. Vi begynner med å opprette to datoer som vi vil sammenligne:

```Javascript
let dato1 = new Date(2020, 5, 1);
let dato2 = new Date(2020, 3, 15);
```

Vi kan så sammenligne disse to datoene ved å bruke følgende operatorer:

- `>` står for større enn
- `<` står for mindre enn
- `>=` står for større enn eller lik
- `<=` står for mindre enn eller lik
- `===` står for lik

La oss se på noen eksempler på hvordan vi kan bruke disse operatorene:

```Javascript
// Sjekker om dato1 er senere enn dato2
if (dato1 > dato2) {
  console.log("Dato1 er senere enn dato2");
}

// Sjekker om dato2 er tidligere enn dato1
if (dato2 < dato1) {
  console.log("Dato2 er tidligere enn dato1");
}

// Sjekker om dato1 er lik dato2
if (dato1 === dato2) {
  console.log("Dato1 er lik dato2");
}
```

Vi kan også bruke disse operatorene i kombinasjon med `if-else`-setninger for å utføre ulike handlinger avhengig av resultatet av sammenligningen. Vi kan også bruke metoden `getTime()` for å konvertere datoen til millisekunder, noe som gjør det enklere å sammenligne datoen med andre verdier.

## Dypdykk

Når vi sammenligner datoer i Javascript, er det viktig å være klar over at det kan være noen fallgruver. På grunn av hvordan datoer er lagret og tolket i Javascript, kan to tilsynelatende like datoer likevel bli ansett som ulike.

For eksempel vil følgende kode returnere `false`:

```Javascript
let dato1 = new Date(2020, 5, 1);
let dato2 = new Date(2020, 5, 1);
console.log(dato1 === dato2); //false
```

Dette skjer fordi to `Date`-objekter vil kun bli ansett som like hvis de refererer til samme objekt i hukommelsen. Hvis vi heller sammenligner verdien av `getTime()`-metoden, vil koden returnere `true`:

```Javascript
let dato1 = new Date(2020, 5, 1);
let dato2 = new Date(2020, 5, 1);
console.log(dato1.getTime() === dato2.getTime()); //true
```

Det er også viktig å være klar over at datoer ofte tolkes forskjellig i ulike deler av verden. For eksempel vil `new Date(2020, 0, 1)` representere 1. januar 2020 i mange land, mens det i USA vil representere 31. desember 2019 på grunn av tidsforskjellen.

## Se også

- [Date object - MDN web docs](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Comparing dates in JavaScript - Stack Overflow](https://stackoverflow.com/questions/49682382/comparing-dates-in-node-js-in-different-timezones)