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

## Hva & Hvorfor?

Beregning av en dato i fremtiden eller fortiden er prosessen hvor en programvareutvikler manipulerer datoer for å flytte dem frem eller tilbake i tid. Dette er nyttig i mange scenarioer, for eksempel for å planlegge påminnelser, beregne forfallsdatoer og tidslinjer.

## Hvordan gjør man det:

Her er en grunnleggende kode som viser hvordan dette kan gjøres i JavaScript. Vi tar utgangspunkt i dagens dato, og legger til syv dager for å få en dato i fremtiden, og trekker deretter syv dager for å få en dato i fortiden:

```Javascript
let today = new Date();
let futureDate = new Date();
let pastDate = new Date();

futureDate.setDate(today.getDate() + 7);
pastDate.setDate(today.getDate() - 7);

console.log(`Dagens dato er: ${today}`);
console.log(`Datoen om syv dager er: ${futureDate}`);
console.log(`Datoen for syv dager siden var: ${pastDate}`);
```

Merk at `new Date()` gir oss dagens dato, og `setDate()` metoden lar oss manipulere denne datoen.

## Dypdykk:

Det å beregne en dato i fremtiden eller fortiden er noe som har vært gjort så lenge vi har hatt datamaskiner. Før JavaScript kom på banen, brukte utviklere språk som C++ og Java for å gjøre dette. Alternativt til `Date()` finnes også mer kraftfulle biblioteker, som for eksempel `moment.js` eller `date-fns`, som gir en mer fullstendig løsning for dato- og tidsmanipulering.

Utførelsen av dette i JavaScript er basert på ECMAScript `- Date`-objektet, som gjør det mulig for oss å jobbe med datoer og tider. Vi bruker `new Date()` for å lage et nytt datoobjekt, og så kan vi bruke forskjellige metoder, som `setDate()`, for å manipulere det.

## Se Også:

For mer informasjon om håndtering av datoer og tider i Javascript, kan disse ressursene være nyttige:

1. [JavaScript Date Objects (W3Schools)](https://www.w3schools.com/js/js_date_methods.asp)
2. [Working with JavaScript Date (MDN)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Numbers_and_dates#date_object)
3. [moment.js](https://momentjs.com/)
4. [date-fns](https://date-fns.org/)