---
title:                "Javascript: Lesing av en tekstfil."
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor
Lurer du på hvorfor du bør engasjere deg i å lese en tekstfil? Vel, tekstfiler er en av de vanligste måtene å lagre og dele data på i et programmeringsmiljø. De kan inneholde alt fra tekst til tall og er en viktig del av å lære programmering.

## Hvordan
Det første du må gjøre er å bruke Javascript til å åpne tekstfilen. Dette kan gjøres med `fs` -modulen og `fs.readFile()` -funksjonen. Du kan da lese filen ved å bruke `toString()` -metoden. For eksempel:

```Javascript
const fs = require('fs');

fs.readFile('tekstfil.txt', function(err, data) {
  if (err) throw err;
  console.log(data.toString());
});
```

Dette vil skrive ut innholdet i tekstfilen til terminalen. Hvis du vil lagre dataene i en variabel for senere bruk, kan du gjøre det ved å bruke en `var` -erklæring:

```Javascript
const fs = require('fs');

var tekst = fs.readFile('tekstfil.txt', function(err, data) {
  if (err) throw err;
  return data.toString();
});

console.log(tekst);
```

Dette vil lagre innholdet i tekstfilen i variabelen `tekst` som du kan bruke videre i koden din.

## Dypdykk
Det er viktig å merke seg at `fs.readFile()` -funksjonen bruker en såkalt "callback" -funksjon for å håndtere filen. Dette betyr at den vil kjøre i bakgrunnen mens resten av koden din fortsetter å kjøre. Dette er grunnen til at vi brukte `return` -erklæringen i eksemplet over for å få tak i innholdet i tekstfilen.

En annen ting å huske på er at `fs.readFile()` -funksjonen vil lese filen i sin helhet og deretter returnere dataene. Hvis du har en veldig stor fil, kan dette føre til problemer med minnebruk i programmet ditt. En løsning på dette kan være å bruke `fs.createReadStream()` -funksjonen som vil lese filen i mindre biter og dermed redusere mengden av minne som blir brukt.

## Se også
- [fs Modulen i Node.js](https://nodejs.org/api/fs.html)
- [JavaScript File System: La oss lese flere filer!](https://medium.com/@osanseviero/javascript-file-system-la-oss-lese-flere-filer-ae018dddeec4)
- [Using Node.js to Read File Using FileReader & WebSocket API](https://www.patlau.com/using-node-js-to-read-file-using-filereader-websocket-api/)