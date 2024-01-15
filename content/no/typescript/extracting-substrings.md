---
title:                "Utvinning av substrings"
html_title:           "TypeScript: Utvinning av substrings"
simple_title:         "Utvinning av substrings"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## Hvorfor

Å hente ut substringer fra en streng er en nyttig teknikk som kan hjelpe deg med å håndtere og manipulere data på en mer effektiv måte. Dette kan være spesielt nyttig når du jobber med større datasett eller når du trenger å filtrere ut spesifikke deler av en streng.

## Hvordan

For å hente ut substringer i TypeScript, kan du bruke `slice()` metoden. Denne metoden tar to parametere, start og slutt indeks, og returnerer en ny streng bestående av delen av den opprinnelige strengen du ønsker å hente ut. Her er et eksempel:

```TypeScript
let navn: string = "Per Hansen";
let etternavn: string = navn.slice(4, 10);

console.log(etternavn); // Hansen
```

I dette eksemplet brukes `slice()` metoden til å hente ut etternavnet til personen fra strengen "Per Hansen" ved å angi startindeks som representerer "H" og sluttpunktet som representerer "n". Du kan også bruke negative indekser, som starter fra slutten av strengen. For eksempel kan du bruke `-1` som sluttpunkt for å hente ut den siste bokstaven i strengen.

## Dypdykk

Det er viktig å merke seg at `slice()` metoden ikke endrer den opprinnelige strengen, men returnerer en ny. Du kan også bruke `substring()` og `substr()` metodene til å hente ut substringer, men de har noen små forskjeller i hvordan de håndterer parametrene. For mer informasjon om disse metodene og videre lesing om å hente ut substringer i TypeScript, kan du sjekke ut dokumentasjonen [her](https://www.typescriptlang.org/docs/handbook/basic-types.html#string).

## Se også

- [Dokumentasjon for strenger i TypeScript](https://www.typescriptlang.org/docs/handbook/basic-types.html#string)
- [Hva er forskjellen mellom slice, substring og substr i JavaScript?](https://stackoverflow.com/questions/2243824/what-is-the-difference-between-slice-substring-and-substr-in-javascript)