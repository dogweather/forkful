---
title:                "Å beregne en dato i fremtiden eller fortiden"
html_title:           "TypeScript: Å beregne en dato i fremtiden eller fortiden"
simple_title:         "Å beregne en dato i fremtiden eller fortiden"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å beregne en dag i fremtiden eller fortiden betyr å finne ut hvilken dato som vil være et gitt antall dager etter eller før en gitt dato. Dette er en vanlig oppgave for programmerere når de må håndtere datoer og tidsstempel i programmering.

## Hvordan:

Beregne en dato i fremtiden eller fortiden kan gjøres på en enkel måte ved å bruke Date-objektet i TypeScript. Her er et eksempel på å finne datoen 10 dager frem i tid og skrive den ut i konsollen:

```TypeScript
const today = new Date();
today.setDate(today.getDate() + 10);
console.log(today);
```

Dette vil produsere følgende resultat:

```
2000-01-20T15:55:17.869Z
```

Vi kan også beregne en dato i fortiden ved å bruke samme metode, men med negativt antall dager. Her er et eksempel på å finne datoen 5 dager tilbake i tid:

```TypeScript
const today = new Date();
today.setDate(today.getDate() - 5);
console.log(today);
```

Dette vil produsere følgende resultat:

```
2000-01-05T15:55:17.869Z
```

## Dypdykk:

Beregning av datoer i fremtiden eller fortiden er en viktig oppgave i programmering, spesielt når man håndterer tidssoner og forskjellige formater. Alternativer til å bruke Date-objektet inkluderer moment.js biblioteket som gir mer fleksibilitet og funksjoner for å håndtere datoer.

Når man beregner en dato i fremtiden eller fortiden, er det viktig å huske å også ta hensyn til skuddår og måneder med forskjellig antall dager. Dette kan gjøres ved å bruke innebygde funksjoner i språket eller ved å bruke eksterne biblioteker.

## Se også:

- [moment.js dokumentasjon](https://momentjs.com/docs/)
- [MDN Web Docs - Date object](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)