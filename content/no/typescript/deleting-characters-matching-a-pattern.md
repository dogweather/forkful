---
title:                "Slette tegn som matcher et mønster"
html_title:           "TypeScript: Slette tegn som matcher et mønster"
simple_title:         "Slette tegn som matcher et mønster"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Sletting av tegn som matcher et mønster er en programmeringsteknikk som innebærer å fjerne alle forekomster av et spesifisert tegnmønster fra en streng. Dette gjøres av forskjellige årsaker, for eksempel å formatere tekst eller fjerne uønskede tegn fra en streng før videre behandling.

## Hvordan:

```TypeScript
const tekst = "Hei, hvor rart!";

// For å fjerne alle utropstegn fra teksten:
const formatertTekst = tekst.replace(/!/g, "");

console.log(formatertTekst); // Output: "Hei, hvor rart"

```

```TypeScript
let tall = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

// For å slette alle partall fra listen:
tall = tall.filter(tall => tall % 2 !== 0);

console.log(tall); // Output: [1, 3, 5, 7, 9]
```

## Deep Dive:

Sletting av tegn som matcher et mønster er en vanlig teknikk i mange programmeringsspråk, og det er også støttet av TypeScript. Mønsteret som skal matches kan være både et enkelt tegn eller en mer kompleks kombinasjon av tegn. Alternativene til å bruke denne teknikken inkluderer å bruke en løkke og sjekke hvert tegn i strengen individuelt, men dette kan være mer omstendelig og mindre effektivt.

Når man sletter tegn som matcher et mønster, erstattes de fjernede tegnene med tomme tegn. Dette kan gi uønskede mellomrom i teksten, som kan løses ved å bruke metoden `trim()` som fjerner mellomrom og linjeskift fra begynnelsen og slutten av en streng.

## Se også:

- [MDN's guide for regular expressions in JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)
- [TypeScript's official documentation on string operations](https://www.typescriptlang.org/docs/handbook/strings.html)