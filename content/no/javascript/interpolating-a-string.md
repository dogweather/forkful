---
title:                "Interpolering av en streng"
html_title:           "Javascript: Interpolering av en streng"
simple_title:         "Interpolering av en streng"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/interpolating-a-string.md"
---

{{< edit_this_page >}}

Hva & Hvorfor?

Interpolering av strenger er en måte å sette sammen forskjellige deler av tekster på i JavaScript, ved å kombinere variabler og fast tekst. Det brukes ofte når vi vil bygge en tekststreng dynamisk basert på brukerinput eller variabler. Dette gjør koden mer fleksibel og modulerbar.

Hvordan:

Vi kan interpolere strenger ved å bruke backticks (``) i stedet for anførselstegn eller enkeltfnutter. Deretter kan vi bruke ${variabelnavn} for å inkludere variabler i strengen. La oss se et eksempel:

```javascript
let navn = "Ola";
console.log(`Hei ${navn}, velkommen til nettsiden min!`);
```

Dette vil produsere følgende utskrift:

Hei Ola, velkommen til nettsiden min!

En annen måte å interpolere på er å bruke metoden .concat() for å kombinere strenger og variabler, for eksempel:

```javascript
let alder = 25;
console.log("Jeg er " + alder + " år gammel.");
```

Dette vil produsere følgende utskrift:

Jeg er 25 år gammel.

Deep Dive:

Interpolering av strenger ble introdusert i ES6 (EcmaScript 2015) som en ny syntaks for å sette sammen tekststrenger. Tidligere brukte man ofte en kombinasjon av anførselstegn og plussoperatøren for å bygge strenger. Interpolering ble lagt til for å gjøre det enklere å kombinere tekst og variabler.

En annen måte å interpolere strenger på er å bruke replace() metoden, men dette kan bli mer tungvint når man har flere variabler eller deler av tekst som skal settes sammen.

Se også:

For mer informasjon om interpolering av strenger i JavaScript, sjekk ut følgende ressurser:

- Web development tutorial om string interpolation: https://www.w3schools.com/js/js_string_interpolation.asp
- Offisiell dokumentasjon om template literals fra MDN: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals