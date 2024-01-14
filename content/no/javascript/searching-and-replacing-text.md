---
title:    "Javascript: Søking og erstatting av tekst"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Hvorfor
Å søke og erstatte tekst er en nyttig ferdighet for enhver Javascript-programmerer. Det gjør det enkelt å endre flere deler av en tekst på en gang, og sparer deg for tid og arbeid.

## Slik gjør du det
Å søke og erstatte tekst i Javascript kan gjøres ved hjelp av enkle string-metoder og regulære uttrykk. Ta en titt på eksempelet nedenfor som viser hvordan du kan erstatte alle forekomster av et bestemt ord i en tekststreng:

```Javascript
let tekst = "Dette er en tekst som inneholder ordet Javascript";
let erstattetTekst = tekst.replace("Javascript", "HTML");
console.log(erstattetTekst); // output: Dette er en tekst som inneholder ordet HTML
```

I dette eksemplet bruker vi metoden `replace()` som tar inn to argumenter - det første er teksten du vil bytte ut, og det andre er teksten du vil erstatte det med.

Du kan også bruke regulære uttrykk i kombinasjon med `replace()` for å søke og erstatte på en mer avansert måte. Regulære uttrykk er nyttige når du ønsker å erstatte en hel del av en tekststreng som følger et bestemt mønster. La oss se på et eksempel:

```Javascript
let tekst = "Dette er en tekst som inneholder tall 123 og ordet Javascript";
let erstattetTekst = tekst.replace(/\d+/g, "ett tall");
console.log(erstattetTekst); // output: Dette er en tekst som inneholder tall ett tall og ordet Javascript
```

I dette eksemplet bruker vi et regulært uttrykk `/\d+/g` som leter etter alle tall i teksten og erstatter dem med teksten "ett tall".

## Dypdykk
Det finnes flere string-metoder og regulære uttrykk som kan brukes til å søke og erstatte tekst i Javascript. Her er noen andre nyttige ressurser som kan hjelpe deg å lære mer:

- [String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace) - offisiell dokumentasjon fra MDN med flere eksempler
- [RegExr](https://regexr.com/) - et nettsted som lar deg eksperimentere med regulære uttrykk og se hvordan de fungerer
- [Javascript RegEx Cheat Sheet](https://websitesetup.org/javascript-regex-cheat-sheet/) - en nyttig ressurs for å lære mer om regulære uttrykk

## Se også
- [Hvordan bruke string-metoder i Javascript](https://blogg.no/string-metoder-javascript)
- [En grundig guide til regulære uttrykk i Javascript](https://blogg.no/regulare-uttrykk-javascript)