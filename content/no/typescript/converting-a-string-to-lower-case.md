---
title:                "Oversette en streng til små bokstaver"
html_title:           "TypeScript: Oversette en streng til små bokstaver"
simple_title:         "Oversette en streng til små bokstaver"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å konvertere en streng til små bokstaver betyr å endre alle bokstavene i en tekst til deres tilsvarende små bokstaver. Programmerere gjør dette for å sikre konsistens i koden sin og for å lettere kunne sammenligne strenger.

## Hvordan:
TypeScript har en innebygd funksjon som heter ```toLowerCase()```, som gjør det mulig å konvertere en streng til små bokstaver. Her er et eksempel på hvordan man kan bruke denne funksjonen i koden:

```TypeScript
let navn: string = "John Doe";
console.log(navn.toLowerCase());
```

Outputen vil være:
```
john doe
```

## Dypdykk:
Å konvertere strenger til små bokstaver har vært vanlig praksis i programmering i lang tid, og har sin opprinnelse fra ASCII-standarden som ble utviklet tidlig på 1960-tallet. Det finnes også alternative måter å gjøre dette på, som for eksempel å bruke regex-uttrykk. Implementasjonen av ```toLowerCase()```-funksjonen i TypeScript følger Unicode-standarden, som betyr at språk som ikke bare bruker latinske tegn også blir konvertert til små bokstaver.

## Se også:
Hvis du ønsker å lære mer om strenger og hvordan man manipulerer dem i TypeScript, kan du sjekke ut disse kildene:

- Offisiell dokumentasjon for ```toLowerCase()```-funksjonen: https://www.typescriptlang.org/docs/handbook/reference/functions.html#strings-and-regular-expressions
- En tutorial på YouTube om strenger i TypeScript: https://www.youtube.com/watch?v=9tIom3xv8-k
- En bloggpost som diskuterer forskjellige metoder for å konvertere strenger til små bokstaver: https://www.thoughtco.com/javascript-to-case-of-a-string-2037702