---
title:                "Lettura di un file di testo"
date:                  2024-01-20T17:55:07.584036-07:00
model:                 gpt-4-1106-preview
simple_title:         "Lettura di un file di testo"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why?
Leggere un file di testo significa accedere al suo contenuto tramite codice. I programmatori lo fanno per processare dati, configurazioni o per leggere risorse lato server.

## How to:
Per leggere un file di testo in Node.js con TypeScript, uso la libreria `fs`. Ecco come fare:

```typescript
import { readFile } from 'fs/promises';

async function leggiFile(testo: string) {
  try {
    const contenuto = await readFile(testo, { encoding: 'utf8' });
    console.log(contenuto);
  } catch (errore) {
    console.error(errore);
  }
}

leggiFile('esempio.txt');
```
Se `esempio.txt` contiene `Ciao mondo!`, l'output sarà:
```
Ciao mondo!
```

## Deep Dive
Leggere file di testo è basilare ma cruciale. Nata nei primi giorni della programmazione, questa funzionalità permetteva di conservare e accedere ai dati. TypeScript, essendo un superset di JavaScript, sfrutta le API di Node.js per tale operazione. C'è l'opzione sincrona, `readFileSync`, ma quella asincrona, `readFile`, previene il blocco dell'I/O. Alternativamente, potresti usare `fs.createReadStream` per file grandi, evitando di caricare tutto in memoria.

## See Also
- Documentazione ufficiale Node.js per la lettura dei file: [Node.js fs.readFile](https://nodejs.org/api/fs.html#fs_fs_readfile_path_options_callback)
