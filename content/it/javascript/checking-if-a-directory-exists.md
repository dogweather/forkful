---
title:    "Javascript: Verifica se esiste una directory"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché

Programmare in Javascript può essere un processo impegnativo e spesso ci si trova di fronte a problemi che richiedono di essere risolti. Uno di questi problemi può essere quello di controllare se una directory esiste. Questa operazione può essere utile per una serie di motivi, come ad esempio accedere a file o cartelle specifiche, o verificare se un utente ha i permessi necessari per accedere a quelle risorse.

## Come fare

Per controllare se una directory esiste, possiamo utilizzare la funzione `fs.existsSync()` del modulo File System di Node.js.

```
const fs = require('fs'); 

if (fs.existsSync('/percorso/della/directory')) { 
    console.log('La directory esiste!'); 
} else { 
    console.log('La directory non esiste!'); 
}
```

Questo semplice codice utilizza la funzione `fs.existsSync()` per verificare se la directory specificata esiste. Se la directory esiste, viene stampato un messaggio di conferma; altrimenti, viene stampato un messaggio di errore.

Un'altra opzione è utilizzare la funzione `fs.stat()` per ottenere informazioni sulla directory e verificare se esiste o meno.

```
const fs = require('fs'); 

fs.stat('/percorso/della/directory', (err, stats) => { 
    if (err) { 
        console.log('La directory non esiste!'); 
    } else { 
        console.log('La directory esiste!'); 
        console.log(`Dimensioni: ${stats.size} bytes`); 
        console.log(`Ultima modifica: ${stats.mtime}`); 
    } 
});
```

La funzione `fs.stat()` restituisce un oggetto `stats` contenente diverse informazioni sulla directory, come la sua dimensione e l'ultima data di modifica.

## Approfondimento

In alcuni casi, potrebbe essere necessario controllare se una directory specifica esiste all'interno di una directory principale. In questo caso, possiamo utilizzare la funzione `fs.readdir()` per elencare tutti i file e le directory presenti in una determinata directory.

```
const fs = require('fs');

fs.readdir('/percorso/della/directory', (err, files) => {
    if (err) {
        console.log('Errore nella lettura dei file!');
    } else {
        if (files.includes('directory-cercata')) {
            console.log('La directory esiste!');
        } else { 
            console.log('La directory non esiste!');
        }
    }
});
```

La funzione `fs.readdir()` restituisce un array contenente tutti i nomi dei file e delle directory presenti nella directory specificata. Utilizzando la funzione `includes()`, possiamo verificare se tra questi nomi è presente la directory che stiamo cercando.

## Vedi anche

- [Documentazione su `fs.existsSync()`](https://nodejs.org/api/fs.html#fs_fs_existssync_path)
- [Documentazione su `fs.stat()`](https://nodejs.org/api/fs.html#fs_fs_stat_path_options_callback)
- [Documentazione su `fs.readdir()`](https://nodejs.org/api/fs.html#fs_fs_readdir_path_options_callback)