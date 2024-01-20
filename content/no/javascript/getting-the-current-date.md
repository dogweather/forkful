---
title:                "Slik får du tak i dagens dato"
date:                  2024-01-20T15:15:01.485906-07:00
html_title:           "C: Slik får du tak i dagens dato"
simple_title:         "Slik får du tak i dagens dato"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å hente dagens dato i JavaScript betyr å få nøyaktig tidspunkt for når koden kjøres. Det er nyttig for alt fra tidssporing i applikasjoner til å stemple brukeraktivitet.

## Hvordan gjøre det:

Hente nåværende dato og klokkeslett:

```javascript
let naa = new Date();
console.log(naa);
```

Utskrift kan se slik ut, avhengig av når og hvor koden kjøres:

```
2023-03-29T12:34:56.789Z
```

Kun datoen, uten tid:

```javascript
let dato = new Date().toDateString();
console.log(dato);
```

Eksempel output:

```
Wed Mar 29 2023
```

## Dypdykk

Historisk sett har JavaScript alltid hatt objektet `Date` for å jobbe med datoer og tider. Metoder som `now()`, `parse()`, og `UTC()` gir flere måter å skape og manipulere datoobjekter på.

Alternativt kan biblioteker som `Moment.js` eller det nyere `date-fns` brukes for mer komplekse operasjoner.

Implementasjonen av `new Date()` tar brukerens tidssone og sommertid inn i beregningen, så resultatet er lokaltid på brukerens maskin.

## Se også

- MDN Web Docs on Date: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date
- Moment.js dokumentasjon: https://momentjs.com/docs/
- date-fns biblioteket: https://date-fns.org/