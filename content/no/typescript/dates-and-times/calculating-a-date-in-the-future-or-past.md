---
title:                "Beregning av en dato i fremtiden eller fortiden"
aliases:
- no/typescript/calculating-a-date-in-the-future-or-past.md
date:                  2024-01-20T17:32:07.595558-07:00
model:                 gpt-4-1106-preview
simple_title:         "Beregning av en dato i fremtiden eller fortiden"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Beregning av en dato i fremtiden eller fortiden innebærer å legge til eller trekke fra tid fra en nåværende dato. Programmerere bruker dette for å håndtere funksjoner som utløpsdatoer, påminnelser, eller tidsbaserte begivenheter i applikasjonene deres.

## Hvordan gjøre det:
```TypeScript
const today: Date = new Date();
const tenDaysLater: Date = new Date(today);
tenDaysLater.setDate(tenDaysLater.getDate() + 10); // Legger til 10 dager

console.log(today.toDateString()); // Eks.: "Wed Mar 15 2023"
console.log(tenDaysLater.toDateString()); // "Sat Mar 25 2023"

const thirtyDaysAgo: Date = new Date(today);
thirtyDaysAgo.setDate(thirtyDaysAgo.getDate() - 30); // Trekker fra 30 dager

console.log(thirtyDaysAgo.toDateString()); // "Sun Feb 13 2023"
```

## Dypdykk
Datohåndtering i programmering har historie tilbake til begynnelsen av datatiden. Tidligere var det ofte avhengig av plattforms-spesifikke funksjoner, men med moderne språk som JavaScript og TypeScript har vi innebygde klasser som `Date`. Implementasjonsdetaljer inkluderer å huske tidszoner og skuddår når man legger til og trekker fra dager. Alternativt kan biblioteker som Moment.js eller Date-fns benyttes for mer kompleks dato-håndtering.

## Se også
- MDN Web Docs: [Date reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- Moment.js: [Documentation](https://momentjs.com/docs/)
- Date-fns: [Documentation](https://date-fns.org/v2.28.0/docs/Getting-Started)
