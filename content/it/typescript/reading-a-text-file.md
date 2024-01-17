---
title:                "Lettura di un file di testo."
html_title:           "TypeScript: Lettura di un file di testo."
simple_title:         "Lettura di un file di testo."
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Cosa & Perché?
Leggere un file di testo è un'operazione comune e vitale per i programmatori. Consiste nel leggere il contenuto di un file di testo e utilizzarlo all'interno del programma. Ciò può consentire di ottenere input dall'utente, leggere dati da file di configurazione o salvare i risultati delle elaborazioni.

## Come fare:
In TypeScript, è possibile leggere un file di testo utilizzando la funzione `readFileSync()` del modulo `fs`, che fa parte delle librerie standard di Node.js. Di seguito è riportato un semplice esempio di come leggere il contenuto di un file di testo e stamparlo sulla console:

```
// require fs module
const fs = require('fs');
// readFileSync() function takes two arguments: path to file and encoding method
const content = fs.readFileSync('test.txt', 'utf-8');
// print content to console
console.log(content);
```

Il risultato del codice sopra mostrerà il contenuto del file `test.txt` nella console.

## Approfondimento:
La lettura di un file di testo è stata un'operazione essenziale fin dai primi giorni della programmazione. In passato, i programmatori dovevano utilizzare linguaggi come C o C++ insieme alle loro librerie per leggere e scrivere file. Tuttavia, con l'avvento dei linguaggi di scripting come JavaScript e TypeScript, la lettura dei file è diventata più semplice grazie all'utilizzo di librerie e moduli specifici come `fs`.

Come alternativa al metodo `readFileSync()`, è possibile utilizzare anche la funzione `createReadStream()` del modulo `fs` per la lettura di file di grandi dimensioni. Questo metodo legge il file in modo asincrono, il che significa che il programma non si bloccherà durante la lettura.

Per quanto riguarda l'implementazione della lettura di un file di testo, il modulo `fs` sfrutta la gestione dei file del sistema operativo per accedere ai dati del file. Ciò significa che i permessi del file devono essere correttamente impostati affinché il programma possa accedere al suo contenuto.

## Vedi anche:
- Documentazione ufficiale del modulo `fs`: https://nodejs.org/dist/latest-v16.x/docs/api/fs.html
- Tutorial su come leggere un file di testo in TypeScript: https://www.typescriptlang.org/docs/handbook/fs.html