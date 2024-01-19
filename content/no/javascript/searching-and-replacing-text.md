---
title:                "Søking og erstatning av tekst"
html_title:           "Lua: Søking og erstatning av tekst"
simple_title:         "Søking og erstatning av tekst"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Søke og erstatte tekst refererer til å finne et bestemt mønster i en tekststreng og å erstatte det med en annen tekststreng. Programmerere gjør dette hovedsakelig for data manipulasjon og automatisering.

## Hvordan:
Her er noen kodingseksempler: 

1. Bruke `String.prototype.replace()` metoden.

```Javascript
// Eksisterende tekststreng.
let txt = "Hello, world!";

// Erstatt 'world' med 'Norge'.
let replacedTxt = txt.replace("world", "Norge");

console.log(replacedTxt);
// Output: "Hello, Norge!"
```

2. Søke og erstatte flere tilfeller med RegExp (+ 'g' flag).

```Javascript
// Eksisterende tekststreng. 
let txt = "Hei, verden! Verden, hei!";

// Erstatt alle tilfeller av 'verden' med 'Norge'.
let replacedTxt = txt.replace(/verden/g, "Norge");

console.log(replacedTxt);
// Output: "Hei, Norge! Norge, hei!"
```

## Dyp Dykk:
Historisk sett har søking og erstatting av tekst vært en viktig funksjon fra de tidligste dagene av programmering, med det første kjente bruk fra 1956-tallets strengmanipulasjon algoritmer.

Alternativt kan du søke etter og erstatte tekst ved hjelp av avanserte algoritmer som KMP (Knuth-Morris-Pratt), Boyer-Moore, og mange flere, men JavaScripts innebygde metoder er mer enn tilstrekkelige for de fleste bruk.

Når det gjelder implementering, bruker Javascripts `String.prototype.replace()` metoden en grei tilnærming - det leter gjennom teksten streng etter streng, og erstatter tekstbasert på parametrene du angir.

## Se Også:
1. [MDN Web Docs: String.prototype.replace()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
2. [JavaScript RegExp Objects](https://www.w3schools.com/js/js_regexp.asp)
3. [Advanced string search and replace methods](https://stackoverflow.com/questions/441018/replacing-strin)