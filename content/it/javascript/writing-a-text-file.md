---
title:                "Scrivere un file di testo"
html_title:           "Javascript: Scrivere un file di testo"
simple_title:         "Scrivere un file di testo"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Scrivere un file di testo è una delle attività più comuni per un programmatore in Javascript. È un modo semplice e veloce per memorizzare informazioni e dati importanti all'interno di un programma.

## Come fare

Per scrivere un file di testo in Javascript, possiamo utilizzare il modulo `fs` (file system) incluso nella libreria standard Node.js.

```Javascript
// Importiamo il modulo fs
const fs = require('fs');

// Definiamo il contenuto del nostro file di testo
const content = "Questo è un file di testo scritto in Javascript!";

// Utilizziamo il metodo writeFile per scrivere il nostro file
fs.writeFile('mioFile.txt', content, (err) => {
  if (err) throw err;
  console.log('Il file è stato scritto correttamente!');
});

```

## Approfondimento

Ecco alcune opzioni che possiamo utilizzare con il metodo `writeFile`:

- `encoding`: specifica il formato di codifica del file di testo (di default è `utf8`)
- `mode`: imposta i permessi per il file (di default è `0666`)
- `flag`: consente di specificare in che modo il file deve essere aperto (di default è `w`, che indica la modalità di scrittura)

Inoltre, possiamo utilizzare il metodo `appendFile` per aggiungere testo a un file di testo esistente, invece di sovrascriverlo completamente.

```Javascript
fs.appendFile('mioFile.txt', 'Questo testo verrà aggiunto all\'inizio del file', (err) => {
  if (err) throw err;
  console.log('Il file è stato aggiornato correttamente!');
});
```

## Vedi anche

- [MDN - File System API](https://developer.mozilla.org/it/docs/Web/API/File_System_API)
- [Node.js - File System](https://nodejs.org/api/fs.html)