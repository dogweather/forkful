---
title:    "TypeScript: Sammenligning av to datoer"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Hvorfor

Når du arbeider med programmering i TypeScript, kan du enkelt støte på situasjoner der du må sammenligne to forskjellige datoer. Dette kan være nyttig for å finne ut om en dato kommer før eller etter en annen, eller for å beregne tiden mellom to datoer. Ved å lære hvordan du kan sammenligne datoer i TypeScript, vil du kunne håndtere disse situasjonene mer effektivt og nøyaktig.

## Hvordan

For å sammenligne to datoer i TypeScript, kan du bruke innebygde funksjoner og metoder som er tilgjengelige i JavaScript. Her er et eksempel på hvordan du kan sammenligne to datoer og få ut en boolean-verdi som svar:

```TypeScript
let dato1 = new Date('2020-01-01');
let dato2 = new Date('2020-02-02');

let erDato1FørDato2 = dato1 < dato2;

console.log(erDato1FørDato2); // Output: true
```

I dette eksempelet oppretter vi to forskjellige datoobjekter og sammenligner dem ved å bruke `<` operatøren. Hvis dato1 er før dato2, vil boolean-verdien bli satt til `true`, ellers vil den være `false`.

En annen måte å sammenligne datoer på er å bruke deres numeriske verdi. Datoer i JavaScript er lagret som millisekunder siden 1. januar 1970. Derfor kan vi konvertere en dato til denne numeriske verdien og sammenligne den med en annen ved å bruke `getTime()` -metoden. Her er et eksempel:

```TypeScript
let dato1 = new Date('2020-01-01');
let dato2 = new Date('2020-02-02');

let dato1NumeriskVerdi = dato1.getTime();
let dato2NumeriskVerdi = dato2.getTime();

let erDato1FørDato2 = dato1NumeriskVerdi < dato2NumeriskVerdi;

console.log(erDato1FørDato2); // Output: true
```

Du kan også bruke andre innebygde metoder som `getFullYear()`, `getMonth()` og `getDate()` for å få ut spesifikke deler av datoene og sammenligne dem.

## Deep Dive

Når du sammenligner to datoer i TypeScript, er det viktig å være oppmerksom på tidssoner. Datoer kan være forskjellige avhengig av hvilken tidssone du er i, og dette kan påvirke resultatene dine når du sammenligner dem. For å unngå feil, kan det være lurt å konvertere alle datoer til UTC før du sammenligner dem.

En annen ting å være oppmerksom på er at JavaScript (og derfor også TypeScript) har noen begrensninger når det kommer til å håndtere datoer. For eksempel vil datoer før 1. januar 1970 ikke bli håndtert riktig av de innebygde metodene. Dette kan føre til uventede resultater når du sammenligner datoer som ligger før denne datoen.

## Se også

- [MDN Web Docs: Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScript Docs: Date](https://www.typescriptlang.org/docs/handbook/datetime.html)
- [W3Schools: JavaScript Dates](https://www.w3schools.com/js/js_dates.asp)