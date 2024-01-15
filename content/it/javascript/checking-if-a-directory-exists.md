---
title:                "Controllare se una directory esiste"
html_title:           "Javascript: Controllare se una directory esiste"
simple_title:         "Controllare se una directory esiste"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Perché 

Spesso, quando si lavora con un linguaggio di programmazione come JavaScript, si potrebbe dover controllare l'esistenza di una directory all'interno del sistema di file. Ciò potrebbe essere necessario per gestire i file che saranno creati o acceduti dal proprio codice. In questo articolo, vedremo come verificare se una directory esiste utilizzando JavaScript.

## Come fare 

Per verificare se una directory esiste utilizzando JavaScript, possiamo utilizzare la funzione `existsSync` del modulo `fs`. Questa funzione ci permette di verificare in modo sincrono se una directory esiste o meno. Di seguito è riportato un codice di esempio:

```Javascript
var fs = require('fs');

if (fs.existsSync('/usr/local')) {
    console.log('La directory esiste!');
} else {
    console.log('La directory non esiste!');
}
```

In questo codice, stiamo utilizzando la funzione `existsSync` per verificare se la directory `/usr/local` esiste o meno. Se la directory esiste, verrà stampato un messaggio a schermo. In caso contrario, verrà stampato un altro messaggio.

## Approfondimento 

Oltre alla funzione `existsSync`, il modulo `fs` di Node.js offre anche altre opzioni per verificare l'esistenza di una directory. In particolare, è possibile utilizzare la funzione `accessSync` per verificare se è possibile accedere alla directory oppure no. Ci sono anche altre librerie di terze parti disponibili per il controllo dell'esistenza di directory, come ad esempio [fs-extra](https://www.npmjs.com/package/fs-extra).

## Vedi anche

- [Documentazione ufficiale di Node.js su fs](https://nodejs.org/api/fs.html)
- [Tutorial su come utilizzare il modulo fs di Node.js](https://www.digitalocean.com/community/tutorials/how-to-use-the-fs-module-in-node-js)
- [fs-extra - una libreria di terze parti per il sistema di file in Node.js](https://www.npmjs.com/package/fs-extra)