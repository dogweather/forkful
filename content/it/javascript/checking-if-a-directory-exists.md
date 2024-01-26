---
title:                "Verifica dell'esistenza di una directory"
date:                  2024-01-20T14:57:10.740982-07:00
html_title:           "Gleam: Verifica dell'esistenza di una directory"
simple_title:         "Verifica dell'esistenza di una directory"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
Verificare l'esistenza di una directory significa controllare se un certo percorso su un filesystem porta a una cartella. Programmatori lo fanno per evitare errori durante la lettura/scrittura di file, o prima di creare o accedere a file in quella directory.

## How to:
In Node.js, usiamo il modulo `fs` per lavorare con il filesystem. Ecco un esempio su come verificare l'esistenza di una directory:

```javascript
const fs = require('fs');
const path = './path/to/directory';

// Versione asincrona con fs.access
fs.access(path, fs.constants.F_OK, (err) => {
  if (err) {
    console.error(`La directory non esiste: ${err}`);
  } else {
    console.log('La directory esiste.');
  }
});

// Versione sincrona con fs.existsSync
if (fs.existsSync(path)) {
  console.log('La directory esiste.');
} else {
  console.error('La directory non esiste.');
}
```

Output:
```
La directory esiste.
```
or
```
La directory non esiste: [Error: ENOENT: no such file or directory, access './path/to/directory']
```

## Deep Dive:
Storicamente, era comune usare `fs.exists` per controllare l'esistenza di file e directory, ma è stato deprecato perché introduceva ambiguità (non distingueva tra problemi di permessi e la non esistenza). Alternativamente, si può usare `fs.stat()` o `fs.readdir()` per ottenere informazioni su file/directory. Tuttavia, per semplicità e chiarezza del codice, `fs.access()` o `fs.existsSync()` sono preferiti per controllare l'esistenza di directory. Entrambi consigliano di gestire le operazioni sul filesystem in modo proattivo con try-catch per prevenire eccezioni invece di fare controlli preventivi.

## See Also:
- Node.js File System documentation: [File System | Node.js v16.13.2 Documentation](https://nodejs.org/api/fs.html)
- Discussione su `fs.exists` vs `fs.access`: [Should I use fs.existsSync or fs.access?](https://stackoverflow.com/questions/31788990/should-i-use-fs-existssync-or-fs-access)
- Guida ai moduli Node.js: [Node.js Modules: An Introduction](https://www.nodesource.com/blog/an-absolute-beginners-guide-to-using-npm/)
