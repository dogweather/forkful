---
title:                "Finne lengden på en streng"
html_title:           "Go: Finne lengden på en streng"
simple_title:         "Finne lengden på en streng"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# Finne Lengden på en Streng i TypeScript

## Hva & Hvorfor?

Å finne lengden på en streng innebærer å telle antall karakterer den har. Dette gjøres av programmører for å manipulere eller validere tekstdata.

## Hvordan gjøre det:

For å finne lengden på en streng i TypeScript, bruker vi .length metoden. Her er et eksempel: 

```TypeScript
let tekst: string = "Hei, verden!";
console.log(tekst.length);
```

Kjører du koden ovenfor, vil du få utfallet: `12`. Fordi "Hei, verden!" strengen består av 12 tegn.

## Dyp Dykk

Helt siden de tidlige dagene for programmering, har muligheten til å finne lengden på en streng vært en grunnleggende funksjon. I TypeScript, er .length metoden gitt i grunnpakken, så det er ingen nødvendighet for eksterne bibliotek.

Som alternativer kan du bruke en "for" løkke til å telle antall tegn, men .length er mer effektiv og mindre utsatt for feil.

Internt, opererer .length metoden ved å returnere antall kodeenheter i strengen. TypeScript behandler strenger som en sekvens av UTF-16 kodeenheter, noe som gir en pålitelig måte å håndtere tekstdata på i mange språk.

## Se også

For mer informasjon om strengmanipulasjoner og TypeScript, sjekk ut følgende ressurser:

1. [TypeScript offisielle dokumentasjon om strenger](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
2. [En guide om forskjellige måter for strengmanipulasjoner i TypeScript](https://www.tutorialsteacher.com/typescript/typescript-string)
3. [Et dypt dykk inn i UTF-16 og hvordan TypeScript behandler strenger](https://stackoverflow.com/questions/4878756/how-to-capitalize-the-first-letter-of-each-word-in-a-string-in-javascript)