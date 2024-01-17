---
title:                "Søk og erstatt tekst"
html_title:           "Javascript: Søk og erstatt tekst"
simple_title:         "Søk og erstatt tekst"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Hva & hvorfor?
Søking og erstatting av tekst er en viktig del av programmering. Det handler om å finne bestemte deler av tekst og erstatte dem med noe annet. Dette gjøres for å effektivisere kode, rette feil eller gjøre tekst mer lesbar.

## Hvordan:
```Javascript
// Eksempel på søk og erstatting:
let tekst = "Hei alle sammen, velkommen til vår nettside!";
let nyTekst = tekst.replace("velkommen", "kom inn");
console.log(nyTekst);
// Output: Hei alle sammen, kom inn til vår nettside!
```

## Dypdykk:
Søk og erstatting av tekst har vært en del av programmering siden de tidligste dager. Det finnes flere alternative metoder for å utføre dette, som for eksempel regular expressions og string handling functions. Det er viktig å være nøye med hvilke deler av teksten man ønsker å erstatte, da det kan føre til uforutsette endringer i koden om man ikke er presis.

## Se også:
- [MDN Web Docs - Searching and replacing text using JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions#Substituting_Substrings)