---
title:                "Opprette en midlertidig fil"
html_title:           "C#: Opprette en midlertidig fil"
simple_title:         "Opprette en midlertidig fil"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/no/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# Lage midlertidige filer i Javascript

## Hva & Hvorfor?

Laging av midlertidige filer handler om å skape filer som ikke varer for alltid, men blir fjernet etter bruk. Dette er verdifullt for programmerere for å minimere ressursbruk og lagring, spesielt når det håndteres store datamengder.

## Hvordan gjør man det:

Her er et grunnleggende eksempel på hvordan du oppretter en midlertidig fil i Javascript ved hjelp av `temp-file` modulen.

```Javascript
const tempFile = require('temp-file');
  
// Lager en midlertidig fil
let tempfile = tempFile(); 
console.log(tempfile); 
// Output: '/tmp/1234567890abcdef'
```

I dette eksempelet vil stien til den midlertidige filen bli skrevet ut. Filen vil da bli fjernet automatisk når prosessen avsluttes.

## Dypdykk 

Historisk sett har midlertidige filer vært en varig del av databehandling, selv før Javascripts dominans. Lignende funksjonalitet finnes i mange programmeringsspråk, som Python og Java.

Et alternativ til å bruke midlertidige filer er å bruke midlertidig hukommelse (RAM), selv om dette kan være dyrt med store datamengder. Javascript tillater nå bruk av blobs, som kan brukes til midlertidig lagring i klientprogrammer.

Implementeringen av midlertidige filer i `temp-file` modulen er ganske rett frem. Den bruker Node.js's `fs` modul til å lage filen, og prosessens 'exit' event til å slette den.

## Se også 

For mer informasjon om hvordan bruke 'temp-file' modulen i Javascript, se [dokumentasjonen her](https://www.npmjs.com/package/temp-file).

For mer bakgrunn på midlertidige filer generelt, se [denne StackOverflow-tråden](https://stackoverflow.com/questions/1132941/what-are-the-best-practices-for-using-temporary-files-in-a-program). 

For hvordan å håndtere blobs i Javascript, se [MDN dokumentasjon](https://developer.mozilla.org/en-US/docs/Web/API/Blob).