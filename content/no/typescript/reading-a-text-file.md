---
title:                "TypeScript: Lese en tekstdokument"
simple_title:         "Lese en tekstdokument"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Det å lese tekstfiler er en viktig del av programmering, og kan være nyttig for å hente informasjon fra ulike kilder eller lagre data for senere bruk. For Norwegian readers, å forstå hvordan man kan lese tekstfiler ved hjelp av TypeScript kan være nyttig for å øke kunnskapen og ferdighetene innenfor dette programmeringsspråket.

## Hvordan du kan lese en tekstfil med TypeScript

For å lese en tekstfil med TypeScript, må man først importere innbygde moduler som "fs" og "path". Deretter må man bruke fs.readFile() metoden for å lese innholdet i filen. Her er et eksempel på hvordan man kan lese en tekstfil som heter "tekstfil.txt":

```TypeScript
import * as fs from 'fs';
import * as path from 'path';

const tekstfilSti = path.resolve(__dirname, 'tekstfil.txt');

fs.readFile(tekstfilSti, 'utf-8', (err, data) => {
    if (err) {
        console.log(err);
    } else {
        console.log(data);
    }
});
```

I dette tilfellet bruker vi fs.readFile() metoden med tre parametere: filstien, ønsket tegnsett (i dette tilfellet "utf-8"), og en tilbakekallfunksjon som vil skrive ut innholdet i filen eller eventuelle feilmeldinger.

## Dykke dypere ned i lesing av tekstfiler

Når man leser en tekstfil, er det viktig å være oppmerksom på tegnsettet til filen. Hvis tegnsettet ikke er riktig, kan det føre til feil eller uforståelig data. Man bør også være forsiktig med å lese store filer, da det kan føre til at programmet går tom for minne. Det kan være lurt å bruke en streaming-tilnærming i stedet for å lese hele filen samtidig.

En annen viktig del av lesing av tekstfiler er å kunne håndtere feil. I eksempelet over brukte vi en if-else setning for å skrive ut eventuelle feilmeldinger. Det kan også være lurt å bruke try-catch blokker for å håndtere feil på en mer detaljert måte.

## Se også

- [Offisiell dokumentasjon for fs modulen](https://nodejs.org/api/fs.html)
- [Offisiell dokumentasjon for path modulen](https://nodejs.org/api/path.html)