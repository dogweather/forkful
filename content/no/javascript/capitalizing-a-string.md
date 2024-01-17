---
title:                "Stor bokstaver i en streng"
html_title:           "Javascript: Stor bokstaver i en streng"
simple_title:         "Stor bokstaver i en streng"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

# Hva Og Hvorfor?

Å kapitalisere en streng betyr å gjøre den første bokstaven i hvert ord til en stor bokstav. Dette gjøres for å gjøre teksten mer lesbar og forståelig, spesielt når man jobber med store mengder tekst. Programmere gjør dette for å sikre konsekvens og organisering i koden deres.

# Hvordan:
```Javascript
let navn = "martin luther king jr.";

console.log(navn.charAt(0).toUpperCase() + navn.slice(1));
// Resultat: Martin Luther King Jr.
```

Her bruker vi metoden charAt() for å få tak i den første bokstaven i strengen, og så bruker vi metoden toUpperCase() for å gjøre denne om til en stor bokstav. Deretter bruker vi metoden slice() for å få tak i resten av strengen, og til slutt legger vi disse to sammen for å få det endelige resultatet.

# Dypdykk:
Å kapitalisere en streng har vært en vanlig praksis innenfor programmering i mange år. Det er også alternative metoder for å gjøre dette, som for eksempel å bruke CSS eller HTML for å formatere teksten. Dette kan være mer effektivt for å style en nettside, men for å sikre konsekvent formatering i koden er det vanlig å gjøre dette i selve koden.

# Se Også:
- https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase
- https://www.w3schools.com/jsref/jsref_toUpperCase.asp
- https://www.codingdojo.com/blog/why-coding-is-important