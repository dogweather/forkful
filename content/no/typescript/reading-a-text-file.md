---
title:                "TypeScript: Lesing av tekstfil"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hvorfor

Å lese en tekstfil er en vanlig og viktig del av å jobbe med programmering. Det kan være nyttig for å få tilgang til data eller for å bearbeide større mengder informasjon. I denne bloggposten skal vi se på hvordan man kan lese tekstfiler ved hjelp av TypeScript.

## Hvordan

For å lese en tekstfil i TypeScript, må vi bruke Node.js sin innebygde funksjon "fs". Først må vi importere denne funksjonen ved å bruke "require" - uttrykket.

```TypeScript
const fs = require('fs');
```

Neste steg er å opprette en variabel som vil inneholde tekstfilen vi ønsker å lese. Vi kan gjøre dette ved å bruke "readFileSync" funksjonen og angi filnavnet som parameter.

```TypeScript
const data = fs.readFileSync('example.txt');
```

For å få ut dataen som er lagret i filen, må vi bruke "toString" funksjonen.

```TypeScript
console.log(data.toString());
```

Outputten vil da være teksten som ligger inne i filen. For eksempel, hvis teksten i "example.txt" er "Hei på deg!", vil outputten være "Hei på deg!".

## Dypdykk

Det finnes også andre måter å lese tekstfiler på i TypeScript, som for eksempel ved å bruke "readFile" funksjonen i stedet for "readFileSync". Forskjellen er at "readFile" er en asynkron funksjon, mens "readFileSync" er en synkron funksjon. Det betyr at "readFile" funksjonen vil ikke blokkere koden mens filen leses, mens "readFileSync" vil gjøre det.

En annen metode er å lese filen linje for linje ved hjelp av "createReadStream" funksjonen. Dette kan være nyttig hvis man trenger å behandle større filer, da man ikke trenger å laste hele filen inn i minnet før man kan starte behandlingen.

## Se Også

- https://codeburst.io/read-write-json-files-with-node-js-92d03cc82824
- https://nodejs.org/api/fs.html#fs_fs_readfilesync_path_options
- https://medium.com/@osiolabs/read-large-text-files-using-nodejs-a60bc58dae5e