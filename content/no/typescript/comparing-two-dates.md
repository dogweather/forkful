---
title:                "TypeScript: Sammenligning av to datoer"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

### Hvorfor

Sammenligning av to datoer er en vanlig oppgave i programmering, spesielt når du jobber med datoer og tidspunkter. Ved å kunne sammenligne to datoer kan du enkelt sammenligne ulike tidspunkter og foreta beslutninger basert på resultatet. Dette er spesielt nyttig når du jobber med tidssensitive data eller funksjonaliteter.

### Slik gjør du det

Å sammenligne to datoer i TypeScript er enkelt og kan gjøres ved å bruke innebygde funksjoner og operatører. Her er et enkelt eksempel som viser hvordan du kan sammenligne to datoer for å sjekke om den ene er senere enn den andre:

```typescript
let dato1 = new Date('2021-01-01');
let dato2 = new Date('2021-01-02');

if (dato1 < dato2) {
  console.log('dato1 er tidligere enn dato2');
} else if (dato1 === dato2) {
  console.log('dato1 er lik dato2');
} else {
  console.log('dato1 er senere enn dato2');
}
```
Dette eksempelet vil skrive ut "dato1 er tidligere enn dato2" siden dato1 er satt til 1. januar 2021 og dato2 er satt til 2. januar 2021.

I tillegg til å bruke de vanlige sammenligningsoperatorene (<, <=, >, >=), kan man også bruke innebygde funksjoner som getTime() som returnerer datoer som millisekunder siden 1970. Dette kan være nyttig når man trenger å sammenligne svært nøyaktige tidspunkter.

### Dypdykk

Når du sammenligner to datoer, må du være klar over at det kan være forskjellige tidszoner i spill. Datoer og tidspunkter lagres vanligvis som UTC (Universal Time Coordinated) og konverteres til lokal tid ved behov. Dette kan føre til uventede resultater når du sammenligner datoer, spesielt hvis brukeren din befinner seg i en annen tidsone enn serveren din.

En måte å unngå dette på er å bruke funkcjonen Date.UTC() som tar inn en dato og returnerer den som et millisekund siden 1970 i UTC-format. På denne måten kan du sikre at datoene dine sammenlignes riktig helt uavhengig av tidszoner.

### Se også

- [MDN Web Docs - Date](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScript Handbook - Date](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-2-4.html#map--set-weakmap--weakset-and-setters)
- [Comparing Dates in JavaScript](https://dmitripavlutin.com/compare-dates-using-javascript/)