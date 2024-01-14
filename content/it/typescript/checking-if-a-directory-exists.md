---
title:                "TypeScript: Verifica se una directory esiste"
simple_title:         "Verifica se una directory esiste"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché controllare se una cartella esiste

Quando si lavora con programmi di coding, spesso c'è la necessità di gestire le cartelle e i file. In alcune situazioni, potrebbe essere utile verificare se una determinata cartella esiste prima di eseguire un'azione specifica. Ciò può risparmiare tempo e aiutare a prevenire errori durante l'esecuzione del codice. In questo articolo parleremo di come fare questo utilizzando TypeScript.

## Come fare

Per controllare se una cartella esiste in TypeScript, possiamo utilizzare la funzione `fs.existsSync()` che fa parte del modulo `fs`. Questo modulo è incluso nella libreria standard di Node.js e ci permette di interagire con il file system del computer.

```TypeScript
import * as fs from 'fs';

// Verifica se la cartella "documents" esiste
if (fs.existsSync('./documents')) {
  console.log('La cartella "documents" esiste!');
} else {
  console.log('La cartella "documents" non esiste!');
}
```

Il codice sopra userà la funzione `fs.existsSync()` per verificare la presenza della cartella "documents" nella directory corrente. Se la cartella esiste, stamperà "La cartella 'documents' esiste!" nella console. Altrimenti, stamperà "La cartella 'documents' non esiste!".

## Deep Dive

La funzione `fs.existsSync()` accetta un parametro che rappresenta il percorso della cartella da controllare. Se non viene specificato alcun percorso, la funzione controllerà la directory corrente. La funzione restituisce un valore booleano che indica se la cartella esiste o meno.

Questa funzione è particolarmente utile quando si deve gestire il salvataggio o il caricamento di file all'interno di una specifica cartella. Prima di salvare un file, si può verificare se la cartella in cui si intende salvarlo esiste. In caso contrario, la si può creare con la funzione `fs.mkdirSync()`.

## Vedi anche

- [Documentazione ufficiale di fs.existsSync()] (https://nodejs.org/api/fs.html#fs_fs_existssync_path)
- [Come gestire le cartelle con TypeScript] (https://blog.logrocket.com/working-with-directories-in-typescript/)