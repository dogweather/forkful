---
title:                "TypeScript: Utskrift av feilrettingsutdata"
simple_title:         "Utskrift av feilrettingsutdata"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hvorfor

Å skrive kode kan til tider være en utfordring, spesielt når det kommer til feilsøking. En av de beste måtene å finne og løse problemer i koden er å bruke debug utskrift. Dette hjelper oss med å se hva som foregår under kjøring av koden og gir et mer detaljert bilde av hva som skjer. Ved å skrive ut viktige verdier og meldinger kan vi forstå koden bedre og dermed kunne fikse eventuelle feil raskere.

## Slik gjør du det

For å skrive debug utskrift i TypeScript, kan vi bruke funksjonen `console.log()`. Denne funksjonen tar inn en parameter som er det vi ønsker å skrive ut, for eksempel en variabel eller en melding. Her er et eksempel på hvordan det ser ut i kode:

```TypeScript
let navn = 'Marie';
let alder = 25;
console.log('Hei, mitt navn er ' + navn + ' og jeg er ' + alder + ' år gammel.');
```

Dette vil gi følgende utskrift i konsollen når programmet kjører:

`Hei, mitt navn er Marie og jeg er 25 år gammel.`

Som du kan se, kan vi kombinere variabler og tekst for å få mer detaljert informasjon i utskriften.

En annen nyttig funksjon er `console.table()`, som lar oss skrive ut informasjon i en tabellform. Dette er spesielt nyttig når vi har komplekse objekter eller arrayer, og vi ønsker å se dem strukturert.

```TypeScript
let bruker = {
    navn: 'Erik',
    alder: 28,
    yrke: 'Utvikler'
};

let prosjekter = ['Nettbutikk', 'Bloggsiden', 'Kundedatabase'];

console.log(bruker);
console.log(prosjekter);
console.table(bruker);
console.table(prosjekter);
```

Dette vil resultere i følgende utskrift i konsollen:

```
{ navn: 'Erik', alder: 28, yrke: 'Utvikler' }
['Nettbutikk', 'Bloggsiden', 'Kundedatabase']
┌─────────┬─────┬─────────────┐
│ (Index) │ Key │    Value    │
├─────────┼─────┼─────────────┤
│    0    │'navn'│   'Erik'   │
│    1    │'alder'│     28     │
│    2    │'yrke'│'Utvikler'  │
└─────────┴─────┴─────────────┘
┌─────────┬───────────────┐
│ (Index) │     Value     │
├─────────┼───────────────┤
│    0    │  'Nettbutikk' │
│    1    │  'Bloggsiden' │
│    2    │'Kundedatabase'│
└─────────┴───────────────┘
```

Dette gjør det enklere å se strukturen av objektet og arrayet, og gjør feilsøkingen mer effektiv.

## Dypdykk

I tillegg til `console.log()` og `console.table()`, finnes det også andre nyttige funksjoner for debug utskrift i TypeScript. `console.error()` lar oss skrive ut feilmeldinger i rød tekst, mens `console.warn()` lar oss skrive ut advarsler i gult. Dette kan være nyttig for å skille mellom forskjellige typer meldinger i utskriften.

Vi kan også bruke `console.assert()` for å sjekke om en vilkårlig uttalelse er sann. Hvis uttalelsen er falsk, vil det bli skrevet ut en feilmelding. Dette er nyttig for å bekrefte at visse forhold er oppfylt under kjøring av koden.

## Se også

- [https://developer.mozilla.org/en-US/docs/Web/API/console](https://developer.mozilla.org/en-US/docs/Web/API/console)
- [https://www