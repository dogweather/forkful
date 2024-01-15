---
title:                "Sletting av tegn som samsvarer med et mønster"
html_title:           "TypeScript: Sletting av tegn som samsvarer med et mønster"
simple_title:         "Sletting av tegn som samsvarer med et mønster"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor

Det er mange grunner til at noen ville ønske å slette tegn som matcher et mønster i koden sin. Det kan være for å fjerne uønskede tegn som påvirker funksjonaliteten til programmet, eller for å strømlinjeforme og rydde opp i koden for enklere lesbarhet og vedlikehold.

## Hvordan slette tegn som matcher et mønster i TypeScript

Å slette tegn som matcher et mønster i TypeScript er enkelt med bruk av regulære uttrykk (regex). Her er et eksempel på hvordan man kan fjerne alle tall fra en streng:

```TypeScript
let string = "Hello 123 world";
string = string.replace(/\d/g, "");
console.log(string); // Output: Hello world
```

I dette eksempelet bruker vi `.replace()` metoden og et regex-uttrykk for å erstatte alle tall (`\d`) med en tom streng. Det er viktig å merke seg at regex er følsomt for store og små bokstaver, så bruk `i` etter `/` hvis man ønsker at uttrykket skal ignorere dette.

Man kan også bruke regex for å fjerne spesifikke tegn eller bokstaver. For eksempel, hvis man vil fjerne alle forekomster av bokstaven "a" i en streng, kan man gjøre følgende:

```TypeScript
let string = "Apples and oranges";
string = string.replace(/a/gi, "");
console.log(string); // Output: pples nd ornges
```

I dette tilfellet erstatter vi alle bokstaver `a` (uavhengig av om de er store eller små) med en tom streng.

## Dypdykk i regex

Regex, eller regulære uttrykk, er et kraftig verktøy for å søke etter og manipulere tekst i en streng. Det er basert på en serie med symboler og spesielle uttrykk som beskriver mønstrene man ønsker å finne og endre. Støtte for regex er en del av standarden ES2015, og det finnes mange ressurser på nettet for å lære mer om hvordan man bruker det i koding. 

En av de vanligste bruksområdene for regex er å søke etter og manipulere tekst i en streng, som vi har vist i eksemplene over. Men det er også nyttig for å validere inndata, formatere tekst og rydde opp i koden. Å forstå hvordan regex fungerer kan være veldig nyttig for å forbedre effektiviteten og nøyaktigheten i koden din. 

## Se også

- [MDN Regex tutorial](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions) 
- [TypeScript Regex tutorial](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [Regex tester](https://regex101.com/) (nyttig for å teste og øve på regex-uttrykk)