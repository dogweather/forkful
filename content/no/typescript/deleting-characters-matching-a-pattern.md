---
title:    "TypeScript: Sletting av tegn som matcher et mønster"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/no/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Hvorfor
Det kan være mange grunner til å engasjere seg i å slette tegn som matcher et mønster i TypeScript-kode. Dette kan være for å rengjøre og organisere koden din, forbedre lesbarheten eller forbedre ytelsen.

## Hvordan man gjør det
Det første trinnet for å slette tegn som matcher et mønster er å bruke String-replace-funksjonen i TypeScript, som lar deg erstatte tegn med et annet tegn eller en tom streng. Her er et eksempel på bruk av denne funksjonen:

```TypeScript
let sentence: string = "Dette er en tekst!";
let newSentence: string = sentence.replace(/e/g, "");
console.log(newSentence);
```

I dette eksemplet bruker vi `/e/g` som mønster, som betyr at vi vil erstatte alle forekomster av "e" i strengen med en tom streng. Dette vil gi følgende utdata:

```TypeScript
Dtt i n txtst!
```

Vi kan også bruke denne funksjonen til å erstatte deler av tegn med en annen streng, for eksempel:

```TypeScript
let sentence: string = "Dette er en tekst!";
let newSentence: string = sentence.replace(/en/g, "ø");
console.log(newSentence);
```

Dette vil gi utdata:

```TypeScript
Døttø er ø økst!
```

Det er også verdt å merke seg at String-replace-funksjonen returnerer en ny streng og ikke endrer den originale strengen.

## Dypdykk
Det er mange forskjellige måter å bruke String-replace-funksjonen på for å slette tegn som matcher et mønster i TypeScript. Du kan for eksempel bruke mer avanserte regulære uttrykk for å matche et bredere spekter av tegn eller bruke løkker og betingelser for å håndtere spesielle tilfeller.

Det kan også være nyttig å se på andre String-funksjoner som kan bidra til å slette tegn, som for eksempel String-slice-funksjonen for å fjerne deler av en streng, eller String-trim-funksjonen for å fjerne ekstra mellomrom rundt en streng.

## Se også
- [String-replace-funksjonen i TypeScript](https://www.typescriptlang.org/docs/handbook/utilities.html#replace-extract)
- [Regulære uttrykk i TypeScript](https://www.typescriptlang.org/docs/handbook/regular-expressions.html)
- [Andre nyttige String-funksjoner i TypeScript](https://www.typescripttutorial.net/typescript-string-functions/)