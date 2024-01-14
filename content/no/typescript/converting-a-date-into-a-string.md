---
title:    "TypeScript: Konvertere en dato til en streng"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å konvertere datoer til strenger er en vanlig oppgave i programmering, spesielt når man jobber med brukergrensesnitt og ønsker å vise datoer på en lesbar måte. Ved å forstå hvordan dette gjøres, kan man lage mer brukervennlige og informative applikasjoner.

## Hvordan gjøre det

For å konvertere en dato til en streng i TypeScript kan man bruke metoden `toLocaleString()` på et `Date` objekt. Denne metoden tar inn et språk som parameter og returnerer en lesbar tekststreng i henhold til det språket som er angitt.

```TypeScript
let date = new Date();
let dateString = date.toLocaleString("no-NO"); // "08.04.2021 14:30:00"
```

I dette eksempelet har vi angitt språket som `no-NO` for norsk bokmål. Dette vil gi oss en tekststreng med datoen og klokkeslettet på norsk format.

Man kan også formatere strengen ved å legge til ekstra parametere i `toLocaleString()` metoden, for eksempel for å få et mer spesifikt format eller kun vise deler av datoen.

```TypeScript
// Viser kun dato
let dateString = date.toLocaleString("no-NO", { dateStyle: "short" }); // "08.04.21"

// Viser dato og måned på bokmål, og kun år på engelsk
let dateString = date.toLocaleString("no-NO", { dateStyle: "medium", year: "numeric" }); // "8. april 2021"

// Viser klokkeslettet i 24-timers format
let dateString = date.toLocaleString("no-NO", { timeStyle: "short", hour12: false }); // "14:30"
```

Det finnes mange ulike formateringsmuligheter, så det er viktig å lese dokumentasjonen for mer detaljert informasjon.

## Dypdykk

Bak kullissene bruker `toLocaleString()` metoden `Intl` objektet i JavaScript som lar oss konvertere tall og datoer til lokale formater basert på det angitte språket. Dette gjør det enkelt å vise datoer i en brukervennlig form.

Det finnes også alternative måter å lage tekststrenger fra datoobjekter i JavaScript, for eksempel ved å bruke biblioteker som strftime og moment.js. Disse bibliotekene kan være mer fleksible og kraftige, men kommer med en ekstra innlæringskurve.

## Se også

- [MDN: `Date` objektet i JavaScript (engelsk)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [MDN: `Intl` objektet i JavaScript (engelsk)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Intl)
- [Moment.js dokumentasjon (engelsk)](https://momentjs.com/docs/)