---
title:                "Beregning av en dato i fremtiden eller fortiden"
html_title:           "Javascript: Beregning av en dato i fremtiden eller fortiden"
simple_title:         "Beregning av en dato i fremtiden eller fortiden"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Hvorfor

Tenk deg at du skal planlegge en fremtidig begivenhet eller sjekke når en historisk hendelse fant sted. Da kan det være nyttig å kunne regne ut en dato i fortiden eller fremtiden. Med JavaScript kan du enkelt gjøre dette ved hjelp av innebygde funksjoner og metoder. I denne artikkelen skal vi se nærmere på hvordan du kan beregne datoer i fortiden eller fremtiden, og hvordan det kan være nyttig.

## Hvordan

For å kunne beregne en dato i fortiden eller fremtiden i JavaScript, må du først forstå hvordan datoer blir representert og håndtert i språket. JavaScript bruker en spesiell datoobjekttype som inneholder informasjon om år, måned, dag og klokkeslett. For å beregne en dato i fortiden eller fremtiden, må vi først opprette en datoobjekt og deretter bruke funksjoner og metoder for å endre datoen.

Her er et eksempel på hvordan du kan beregne en dato i fremtiden ved hjelp av JavaScript:

```javascript
let nå = new Date(); // Oppretter et datoobjekt med dagens dato og klokkeslett
nå.setDate(nå.getDate() + 7); // Legger til 7 dager for å få datoen i fremtiden
console.log(nå); // Output: Sun Jul 19 2020 14:36:54 GMT+0200 (Central European Summer Time)
```

Her legger vi en uke til dagens dato og får dermed datoen for en uke frem i tid. På samme måte kan du også trekke fra dager, måneder eller år ved å bruke `setDate`, `setMonth` og `setFullYear` metoder.

For å beregne en dato i fortiden kan du følge samme logikk og trekke fra ønsket antall dager, måneder eller år. Det er også mulig å kombinere flere funksjoner og metoder for å få mer nøyaktige og komplekse datoer.

## Dypdykk

Som nevnt tidligere, bruker JavaScript en spesiell datoobjekttype for å håndtere datoer. Denne objekttypen har også en rekke innebygde funksjoner og metoder som kan gjøre beregning av datoer enkle og effektive.

En viktig ting å huske på er at datoer i JavaScript er basert på UTC (Coordinated Universal Time) og kan variere i forskjellige tidssoner. Det er derfor viktig å justere datoen for ønsket tidssone før du gjør beregninger.

Det finnes også flere tredjeparts biblioteker som kan være nyttige for mer avanserte beregninger av datoer og håndtering av tidssoner. Det kan være lurt å utforske disse for å finne den beste løsningen for ditt prosjekt.

## Se også

- [Date objektet i JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [Timezone-js biblioteket](https://github.com/mde/timezone-js)
- [Luxon biblioteket](https://moment.github.io/luxon/)