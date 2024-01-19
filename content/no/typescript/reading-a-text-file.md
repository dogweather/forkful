---
title:                "Lese en tekstfil"
html_title:           "C#: Lese en tekstfil"
simple_title:         "Lese en tekstfil"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Hva & Hvorfor?

Å lese en tekstfil er prosessen med å få informasjon fra en fil i en format som er forståelig for oss mennesker, nemlig tekst. Programmerere gjør det for å få tilgang til, manipulere og analysere data som lagres i filer.

## Hvordan:

Her er et eksempel på hvordan du leser en tekstfil i TypeScript ved bruk av Node.js `fs`-modulen.

```TypeScript
import * as fs from "fs";
fs.readFile("/path/til/din/tekstfil.txt", 'utf8', function (err, data) {
   if (err) {
       return console.log(err);
   }
   console.log(data);
});
```

Når du kjører denne koden, vil utgangen være innholdet i din tekstfil, hvis det ikke finnes noen feil i koden eller filstien.

## Deep Dive:

- Historisk Kontekst: Tekstfiler har lenge vært et middel for å lagre og dele data. De er lette å lage, lese, og modifisere, og krever ingen spesiell programvare utover en enkel teksteditor.
- Alternativer: Selv om lesing av tekstfiler er enkel og direkte, er det andre metoder for datahåndtering. Databaser, for eksempel, tilbyr kraftige måter å lagre, hente, og manipulere data på. Json og XML er også populært for datautveksling.
- Implementasjon Detaljer: Når du leser en tekstfil, bruker du I/O operasjoner. Disse operasjonene kan være blokkerende (synchronous) eller ikke-blokkerende (asynchronous). I vårt tilfelle, bruker vi ikke-blokkerende I/O som betyr at andre operasjoner kan fortsette mens filen blir lest.

## Se Også:

- [Node.js fs Dokumentasjon](https://nodejs.org/api/fs.html): Kilde til grundig forståelse av hvordan `fs`-modulen fungerer.
- [TypeScript Dokumentasjon](https://www.typescriptlang.org/docs/): Utforsk hva mer du kan gjøre med TypeScript.
- [MDN Web Docs om Fil og Direktør-APIer](https://developer.mozilla.org/en-US/docs/Web/API/File_and_Directory_Entries_API): Et alternativ til Node.js for håndtering av filer i nettleseren.