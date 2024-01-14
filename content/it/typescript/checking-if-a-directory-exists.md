---
title:    "TypeScript: Verifica se una directory esiste"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Perché

Spesso quando si programma in TypeScript, è necessario accedere a specifiche cartelle o file all'interno di un'applicazione. Ma cosa succede se quella cartella o file non esiste? Questa è la ragione principale per cui è importante conoscere come verificare l'esistenza di una directory.

## Come fare

Per verificare se una directory esiste o meno in TypeScript, possiamo utilizzare la funzione `existsSync()` del modulo `fs` (file system). Questa funzione accetta il percorso della directory come argomento e restituisce un valore booleano, `true` se la directory esiste e `false` se non esiste.

```TypeScript
import * as fs from 'fs';

const directoryPath = '/percorso/della/directory';

if (fs.existsSync(directoryPath)) {
  console.log('La directory esiste!');
} else {
  console.log('La directory non esiste.');
}
```

L'output di questo codice varierà a seconda di se la directory esiste o no. Proviamo con un esempio!

Input:

```TypeScript
import * as fs from 'fs';

const directoryPath = '/users/documents';

if (fs.existsSync(directoryPath)) {
  console.log('La directory esiste!');
} else {
  console.log('La directory non esiste.');
}
```

Output:

```bash
La directory esiste!
```

Input:

```TypeScript
import * as fs from 'fs';

const directoryPath = '/users/desktop';

if (fs.existsSync(directoryPath)) {
  console.log('La directory esiste!');
} else {
  console.log('La directory non esiste.');
}
```

Output:

```bash
La directory non esiste.
```

## Approfondimento

La funzione `existsSync()` utilizza il modulo `fs`, che rappresenta il file system di un computer. Con `existsSync()` stiamo essenzialmente controllando se il percorso specificato corrisponde a una cartella o a un file esistente nel sistema operativo. È anche possibile utilizzare questa funzione per verificare l'esistenza di file specifici.

Inoltre, esiste anche la versione asincrona della funzione, `exists()`, che accetta una callback come argomento e restituisce un valore booleano al suo interno. Questo è utile se si desidera gestire l'operazione in modo asincrono.

## Vedi anche
- [Documentazione ufficiale di fs.existsSync()](https://nodejs.org/api/fs.html#fs_fs_existssync_path)
- [Articolo su come utilizzare il modulo fs in TypeScript](https://coderrocketfuel.com/article/using-the-node-js-fs-module-in-typescript)

Grazie per aver letto questo articolo su come verificare l'esistenza di una directory in TypeScript! Spero che sia stato utile per te. Ciao!