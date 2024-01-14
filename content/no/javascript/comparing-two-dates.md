---
title:                "Javascript: Sammenligning av to datoer"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Hvorfor

Sammenligning av datoer i Javascript er nyttig for å filtrere og sortere data som inneholder datoer, for eksempel en liste over arrangementer eller transaksjonsdata.

## Hvordan

Det er flere metoder for å sammenligne to datoer i Javascript. En av de vanligste inkluderer å bruke ```Date``` objektet og metoden ```getTime()``` for å konvertere datoene til millisekunder og deretter sammenligne dem.

```Javascript
let date1 = new Date('2020-10-15');
let date2 = new Date('2020-10-20');

console.log(date1.getTime()); // 1602724800000
console.log(date2.getTime()); // 1603156800000

if (date1.getTime() > date2.getTime()) {
    console.log('date1 er etter date2');
} else if (date1.getTime() === date2.getTime()) {
    console.log('date1 er lik date2');
} else {
    console.log('date1 er før date2');
}
```

Dette vil resultere i følgende output:

```
date1 er før date2
```

En annen metode er å bruke ```Date``` objektets metoder som ```getFullYear()```, ```getMonth()```, ```getDate()``` og sammenligne individuelle deler av datoen.

```Javascript
let date1 = new Date('2020-10-15');
let date2 = new Date('2020-10-20');

if (date1.getFullYear() > date2.getFullYear()) {
    console.log('date1 er etter date2');
} else if (date1.getFullYear() === date2.getFullYear()) {
    if (date1.getMonth() > date2.getMonth()) {
        console.log('date1 er etter date2');
    } else if (date1.getMonth() === date2.getMonth()) {
        if (date1.getDate() > date2.getDate()) {
            console.log('date1 er etter date2');
        } else if (date1.getDate() === date2.getDate()) {
            console.log('date1 er lik date2');
        } else {
            console.log('date1 er før date2');
        }
    } else {
        console.log('date1 er før date2');
    }
} else {
    console.log('date1 er før date2');
}
```

Dette vil også resultere i følgende output:

```
date1 er før date2
```

## Dypdykk

Det er viktig å være oppmerksom på at sammenligning av datoer kan være utfordrende på grunn av tidssoner og ulike formater for datoobjekter i forskjellige miljøer. Det er også viktig å merke seg at to like datoer kan være forskjellige på millisekunderivå, og derfor bør man være forsiktig med å bruke streng sammenligning (```===```) i slike tilfeller.

En annen ting å huske på er at Javascript støtter datoer tilbake til år 0, men noen eldre nettlesere kan ikke støtte dette og kan gi uforutsette resultater ved sammenligning. Derfor kan det være hensiktsmessig å bruke tredjepartsbiblioteker som Moment.js for å håndtere datoer og unngå slike problemer.

## Se også

- [Moment.js dokumentasjon](https://momentjs.com/docs/)
- [W3Schools tutorial om å sammenligne datoer i Javascript](https://www.w3schools.com/js/js_dates.asp)
- [Medium artikkel om sammenligning av datoer i Javascript](https://medium.com/better-programming/how-to-compare-2-dates-in-javascript-ca5bfc0e31b6)