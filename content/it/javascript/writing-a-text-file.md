---
title:                "Scrivere un file di testo"
html_title:           "Arduino: Scrivere un file di testo"
simple_title:         "Scrivere un file di testo"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Scrivere un file di testo significa salvare dati in un formato leggibile, come `.txt` o `.csv`. I programmatori lo fanno per persistenza dei dati, logging, o per esportare dati in formati maneggevoli.

## How to:
In Node.js, per scrivere in un file:

```Javascript
const fs = require('fs');

fs.writeFile('example.txt', 'Ciao Mondo!', function(err) {
    if(err) return console.log(err);
    console.log('File salvato!');
});
```
Output:
```
File salvato!
```

Per scrivere in modo sincrono:
```Javascript
const fs = require('fs');

try {
  fs.writeFileSync('example.txt', 'Ciao Mondo!', 'utf8');
  console.log('File salvato!');
} catch(err) {
  console.error(err);
}
```
Output:
```
File salvato!
```

## Deep Dive
Prima di Node.js e dei browser moderni con API di file, si faceva affidamento su tecnologie backend come PHP o Perl per scrivere su file. Oggi JavaScript in ambiente Node.js consente di manipolare il filesystem direttamente. Tuttavia, nel browser, scrivere su file locali è restrittivo per ragioni di sicurezza, e opera attraverso API specializzate come la `FileSystem` API.

Alternative includono database come MongoDB per salvare grandi quantità di dati e localStorage o IndexedDB nel browser.

Dettagli di implementazione: `fs.writeFile()` è asincrono per non bloccare l'esecuzione del codice, mentre `fs.writeFileSync()` è bloccante.

## See Also
- Documentazione di Node.js `fs` Modulo: [Node.js fs module](https://nodejs.org/api/fs.html)
- Informazioni dettagliate sulla `FileSystem` API: [MDN Web Docs - FileSystem API](https://developer.mozilla.org/en-US/docs/Web/API/FileSystem)
- Introduzione a `localStorage` e `IndexedDB`: [MDN Web Docs - Web Storage API](https://developer.mozilla.org/en-US/docs/Web/API/Web_Storage_API)
