---
title:                "Å bruke regelmessige uttrykk"
html_title:           "TypeScript: Å bruke regelmessige uttrykk"
simple_title:         "Å bruke regelmessige uttrykk"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Hvorfor

Det finnes mange gode grunner til å bruke regulære uttrykk (regular expressions) i TypeScript koden din. Først og fremst kan de hjelpe deg med å finne, manipulere og validere tekstbaserte data på en effektiv måte. Dette kan være svært nyttig når du jobber med ting som validering av skjemaer, parsing av filer eller søk i lange dokumenter.

## Hvordan

Bruken av regulære uttrykk i TypeScript er ganske likt som i andre programmeringsspråk. La oss se på noen enkle eksempler og tilhørende output.

```TypeScript
// Matcher en streng som starter med en stor bokstav og inneholder minst ett tall
const regex = /^[A-Z].*\d+$/;

console.log(regex.test('Hei123')); // Output: true
console.log(regex.test('Hei')); // Output: false
```

Dette uttrykket starter med `/` og slutter med `/`, som er vanlig syntaks for regulære uttrykk. `^` betyr "start of line" mens `$` betyr "end of line". Strengen må altså starte med en stor bokstav og inneholde minst ett tall for å være en match.

```TypeScript
// Erstatter alle tall i en streng med *
const regex = /\d/g;

console.log('Hei123'.replace(regex, '*')); // Output: Hei***
```

I dette eksempelet bruker vi funksjonen `replace` til å erstatte alle tall i en streng med `*`. `\d` matcher alle tall og `g` betyr "global", altså alle forekomster i strengen.

## Dypdykk

Det finnes mange forskjellige metoder og funksjoner for å jobbe med regulære uttrykk i TypeScript. Det kan også være lurt å være klar over at noen spesielle tegn, som `.` og `*`, må escapes med `\` for å tolkes som vanlige tegn og ikke som del av selve uttrykket.

En annen ting å være klar over er at regulære uttrykk er case-sensitive, med mindre du bruker flagget `i` for "case-insensitive". Dette kan være spesielt viktig å tenke på ved validering av brukerinput.

## Se også

- [Regulære uttrykk i TypeScript dokumentasjonen](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [Regular Expression Tester](https://regex101.com/) for å teste og eksperimentere med uttrykk.