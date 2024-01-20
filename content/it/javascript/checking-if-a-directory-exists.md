---
title:                "Verifica dell'esistenza di una directory"
html_title:           "Javascript: Verifica dell'esistenza di una directory"
simple_title:         "Verifica dell'esistenza di una directory"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Verifica dell'esistenza di una directory in JavaScript
**Sommario**: Verificare l'esistenza di una directory è un compito comune quando si lavora con filesystem. In questo tutorial, scopriremo come fare proprio questo utilizzando JavaScript.

## Cos'è e perché?
Verificare l'esistenza di una directory significa controllare se una specifica directory esiste o no nel filesystem. Alcuni processi, come la lettura di file, il salvataggio di dati o l'installazione di programmi potrebbero richiederla. Questa verifica è un passaggio cruciale per prevenire errori durante l'esecuzione di questi processi.

## Come fare:
Ecco come verificare l'esistenza di una directory utilizzando Node.js, una runtime di JavaScript molto popolare.

```Javascript
const fs = require('fs');

fs.access('/percorso/della/directory', (errore) => {
  if (errore){
    console.log("La directory non esiste.");
  } else {
    console.log("La directory esiste.");
  }
});
```
Dove '/percorso/della/directory' è il percorso della directory che si vuole verificare. 

## Approfondimento:
La funzione `fs.access()` esiste sin dalla versione v0.11.15 di Node.js. Ma c'è un'alternativa; prima di `fs.access()`, si usava `fs.exists()`, ma questa è stata deprecata nella documentazione perché portava a confusione.
 
Ecco un esempio di come utilizzarla:

```Javascript
const fs = require('fs');

fs.exists('/percorso/della/directory', (esiste) => {
  if(esiste){
    console.log("La directory esiste.");
  } else {
    console.log("La directory non esiste.");
  }
});
```
Con `fs.exists()`, tuttavia, bisogna fare attenzione quando si lancia l'asincronicità del codice. 

## Vedi anche:
1. Documentazione Node.js: [fs.access()](https://nodejs.org/api/fs.html#fs_fs_access_path_mode_callback), [fs.exists()](https://nodejs.org/api/fs.html#fs_fs_exists_path_callback)
3. StackOverflow: [How to check if directory exists in Node.js?](https://stackoverflow.com/questions/4482686/check-synchronously-if-file-directory-exists-in-node-js)