---
title:                "Verificare se una directory esiste"
html_title:           "TypeScript: Verificare se una directory esiste"
simple_title:         "Verificare se una directory esiste"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Cosa e perché?

Verificare se una directory esiste è un'operazione comune nella programmazione che consente di assicurarsi che un percorso esista prima di eseguire operazioni su di esso. Questo è importante perché in caso contrario, il programma potrebbe generare errori o comportarsi in modo imprevisto.

## Come fare:

```TypeScript
import * as fs from 'fs';

const directoryPath = './path/to/directory';

// utilizzando la funzione statSync
const exists = fs.existsSync(directoryPath);

// utilizzando la funzione lstatSync
const stats = fs.lstatSync(directoryPath);
const exists = stats.isDirectory();
```

L'output sarà un booleano, true se la directory esiste e false altrimenti.

## Approfondimento:

In passato, per verificare l'esistenza di una directory si utilizzava la funzione `existsSync` del modulo `fs` di Node.js. Tuttavia, a partire dalla versione 10 di Node.js, questa funzione è stata deprecata e sostituita dalle funzioni `existsSync` e `lstatSync`. La differenza tra le due è che `existsSync` controlla solo se il percorso esiste, mentre `lstatSync` restituisce informazioni più dettagliate sul percorso, come il fatto che sia una directory.

In alternativa, si può utilizzare il modulo `path` per formattare il percorso e verificare l'esistenza utilizzando la funzione `existsSync` del modulo `fs`.

## Vedi anche:

- Documentazione sul modulo `fs` di Node.js: https://nodejs.org/dist/latest-v14.x/docs/api/fs.html
- Documentazione sul modulo `path` di Node.js: https://nodejs.org/dist/latest-v14.x/docs/api/path.html