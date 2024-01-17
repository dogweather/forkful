---
title:                "Lese en tekstfil"
html_title:           "TypeScript: Lese en tekstfil"
simple_title:         "Lese en tekstfil"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?
Å lese en tekstfil i programmering betyr å lese data fra en tekstfil og lagre det i variabler eller bruke det til å utføre andre handlinger. Programmere gjør dette for å hente informasjon fra en ekstern kilde, som for eksempel en database eller en annen fil, og bruke den i deres programmering.

## Hvordan:
Å lese en tekstfil i TypeScript er enkelt med innebygde funksjoner som er tilgjengelig i språket. Her er en kodeeksempel som viser hvordan du kan lese en tekstfil og lagre linjene som en liste av strenger:

```TypeScript
// Leser inn en tekstfil
const fs = require('fs');
let data = fs.readFileSync('/path/til/tekstfil.txt', 'utf8');

// Deler teksten opp i linjer og lagrer det i en liste
let linjer = data.split('\n');
```

Etter å ha kjørt denne koden, vil `linjer` variabelen inneholde alle linjene fra tekstfilen som separate strenger.

## En Dypere Dykk:
Å lese en tekstfil er en viktig del av programmering og har blitt gjort i utallige år. Før innebygde funksjoner som i dag, måtte programmerere bruke flere kodelinjer for å lese og tolke en tekstfil. Men i dag, takket være utviklingen av programmeringsspråk, gjøres denne oppgaven enkelt og effektivt med få linjer med kode. Man kan også bruke eksterne biblioteker for å gjøre oppgaven enda enklere, for eksempel `readline` biblioteket i Node.js.

## Se Også:
- [Node.js fs modul](https://nodejs.org/api/fs.html)
- [GitHub Repo for readline biblioteket](https://github.com/nodejs/node/tree/master/lib/readline.js)