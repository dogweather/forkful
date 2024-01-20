---
title:                "Få den gjeldende datoen"
html_title:           "Haskell: Få den gjeldende datoen"
simple_title:         "Få den gjeldende datoen"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å hente den nåværende datoen betyr å få tak i datoen som den er akkurat nå, nøyaktig når koden kjøres. Programmerere bruker dette til å spore tid, legge til tidsstempler, måle ytelse og mer.

## Hvordan:

Her er hvordan du henter den nåværende datoen i JavaScript:

```Javascript
let dato = new Date();
console.log(dato);
```

Dette vil skrive ut noe som dette (avhengig av når og hvor du kjører koden):

```Javascript
2021-12-20T08:33:18.847Z
```

## Fordypning

Historisk kontekst: Å hente den nåværende datoen har vært en del av JavaScript siden begynnelsen. Date-objektet ble lagt til i JavaScript i 1997 som del av ECMAScript 1 standarden.

Alternativer: Du kan også bruke biblioteker som Moment.js for mer komplekse dato- og tidsoperasjoner. For eksempel:

```Javascript
let moment = require('moment'); 
console.log(moment().format());
```

Implementeringsdetaljer: Når du kaller new Date() i JavaScript, returnerer den et Date objekt som representerer den gjeldende datoen og tid ned til millisekund. Dette skjer på klientens side, så det vil alltid være innstilt på brukerens lokale tidssone, med mindre du angir det spesifikt.

## Se også 

Her er noe relatert lesning for å lære mer:

2. Moment.js [documentation](https://momentjs.com/)
3. [ECMAScript 1 standard](http://www.ecma-international.org/ecma-262/10.0/index.html#Title) som først introduserte Date objektet.