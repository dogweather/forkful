---
title:                "Utskrift av feilsøkingsresultat"
html_title:           "Arduino: Utskrift av feilsøkingsresultat"
simple_title:         "Utskrift av feilsøkingsresultat"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/printing-debug-output.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å trykke ut feilsøkingsdata ('debug output' på engelsk) er prosessen med å vise kodeinformasjon på skjermen for testing og feilsøking. Programmerere gjør dette for lettere å merke feil, se inneværende verdier på variable, og oppnå mer innsikt i hvordan koden kjører.

## Hvordan gjøre det:

Her er en enkel måte å trykke ut feilsøkingsdata på i Javascript:

```Javascript
console.log("Hello, World!"); // Resultat: Hello, World!
let x = 5;
console.log("Verdien av x er: ", x); // Resultat: Verdien av x er: 5
```
Å bruke `console.log()` er den mest brukte måten å feilsøke i Javascript, Linjen vil bli printet til Javascript konsollen i din nettside's utviklerverktøy.

## Dypdykk

I historisk sammenheng, begynte bruk av feilsøkingsutskrift i de mest primitive datamaskinene, der man faktisk skrev ut resultater på papir. Nå er det en integrert del av moderne programmeringspraksis, spesielt i dynamiske språk som Javascript.

Alternativer til `console.log()` inkluderer `console.error()` og `console.warn()`, som også skriver til konsollen, men blir merket som feil eller advarsler, som kan være nyttig for å skille mellom ulike typer debug output.

På implementeringsnivå, går `console.log()`-meldinger direkte til webleserens Javascript engine, hvor de blir skrevet ut til konsollen.

## Se Også

For mer detaljert informasjon om feilsøkingsutskrift i Javascript, se disse kildene:
- [Mozilla Developer Network - console](https://developer.mozilla.org/en-US/docs/Web/API/Console)
- [Google Developers - Get Started with Debugging JavaScript in Chrome DevTools](https://developer.chrome.com/docs/devtools/javascript/)