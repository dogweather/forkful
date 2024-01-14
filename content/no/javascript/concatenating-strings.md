---
title:                "Javascript: Kombinering av strenger"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Hvorfor

Hvorfor skulle noen bry seg om å sette sammen strenger (concatenate strings)? Vel, tenk deg at du har to strenger med tekst og du vil kombinere dem til en ny og lengre tekst. Dette er når concatenation kommer til nytte! Det kan også være nyttig når du vil legge sammen tall og tekst, for eksempel for å lage en dynamisk melding eller utskrift.

## Hvordan

For å sammenslå strenger i Javascript bruker vi "+" operatøren. La oss si at vi har en streng med navnet "Ingrid" og en annen streng med "er en programmerer". For å få dem til å bli en setning, kan vi bruke følgende kode:

```Javascript
let navn = "Ingrid";
let yrke = "er en programmerer";
let setning = navn + " " + yrke;

console.log(setning); // Output: Ingrid er en programmerer
```

Merk at vi bruker et mellomrom (" ") mellom navn og yrke slik at det blir mellomrom i resulterende setning.

Det er også mulig å concatenere flere strenger samtidig:

```Javascript
let hilsen = "Hei";
let navn = "Ingrid";
let alder = "32";
let utskrift = hilsen + " " + navn + ", du er " + alder + " år gammel.";

console.log(utskrift); // Output: Hei Ingrid, du er 32 år gammel.
```

Vi kan også concatenere tall og tekst. Javascript vil automatisk konvertere tall til strenger når de blir kombinert med tekst:

```Javascript
let tall = 5;
let resultat = "Det dobbelte av " + tall + " er " + (tall * 2);

console.log(resultat); // Output: Det dobbelte av 5 er 10
```

## Deep Dive

Når du concateneterer strenger i Javascript, kan du også bruke `+=` (tillegg-likhet) operatøren. Dette er nyttig når du vil bygge opp en lang streng med flere deler. For eksempel:

```Javascript
let bekreftelse = "Takk for at du meldte deg på!";
bekreftelse += " Vi gleder oss til å se deg på arrangementet.";
bekreftelse += " Husk å ta med deg en venn!";

console.log(bekreftelse); // Output: Takk for at du meldte deg på! Vi gleder oss til å se deg på arrangementet. Husk å ta med deg en venn!
```

Det er også verdt å merke seg at vi kan concatenere strenger med andre datatyper, som for eksempel arrays og objekter. I dette tilfellet vil alt ikke bli kombinert til en streng, men i stedet bli omgjort til en streng og satt sammen med resten av teksten.

## Se Også

- [W3Schools: Concatenation](https://www.w3schools.com/js/js_string_concat.asp)
- [MDN Web Docs: String concatenation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Addition_assignment)
- [Codecademy: Concatenation](https://www.codecademy.com/learn/introduction-to-javascript/modules/learn-javascript-strings/cheatsheet)