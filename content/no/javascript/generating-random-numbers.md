---
title:    "Javascript: Generering av tilfeldige tall"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Hvorfor
Random tall eller tilfeldige tall er en viktig del av mange programmeringsprosjekter. Å kunne generere tilfeldige tall er nyttig for å lage spill, simuleringer, og mange andre typer applikasjoner. Det er også en vanlig metode for å sikre at krypteringsnøkler og passord er unike og vanskelige å gjette.

## Hvordan
Det finnes flere måter å generere tilfeldige tall på i Javascript. Den enkleste metoden er å bruke Math.random() funksjonen. Denne funksjonen returnerer et tilfeldig tall mellom 0 og 1. For å få et tilfeldig tall mellom et bestemt intervall, kan du bruke følgende kode:

```javascript
let tilfeldigTall = Math.floor(Math.random() * (max - min + 1)) + min;
```

I dette tilfellet vil tilfeldigTall variere mellom min og max (inkludert begge tallene). For eksempel, med min = 1 og max = 10, vil tilfeldigTall variere mellom 1 og 10.

Det er også mulig å bruke en løkke for å generere et visst antall tilfeldige tall. For eksempel:

```javascript
for (let i = 0; i < 10; i++) {
  let tilfeldigTall = Math.floor(Math.random() * (max - min + 1)) + min;
  console.log(tilfeldigTall);
}
```

Dette vil generere 10 tilfeldige tall innenfor det gitte intervallet.

## Deep Dive
Det er viktig å merke seg at selv om Math.random() funksjonen genererer tilfeldige tall, er de ikke helt tilfeldige. Funksjonen bruker en algoritme som produserer tall basert på en startverdi kalt en "seed". Dette betyr at dersom du bruker samme "seed" vil du få samme rekkefølge av tilfeldige tall hver gang du kjører koden. Dette kan være en ulempe hvis du trenger helt tilfeldige tall.

For å løse dette problemet, kan du bruke en tredjeparts bibliotek som "random-js" som gir mye større muligheter for å generere tilfeldige tall. Dette vil sikre at tallene er mer tilfeldige uten å måtte bekymre seg for å sette en "seed".

## Se også
- [The Math Object in JavaScript (MDN)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Math)
- [Generating Random Numbers in JavaScript (freeCodeCamp)](https://www.freecodecamp.org/news/how-to-generate-random-numbers-in-javascript/)
- [Using the random-js library (npm)](https://www.npmjs.com/package/random-js)