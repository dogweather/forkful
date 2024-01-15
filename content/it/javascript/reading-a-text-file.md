---
title:                "Leggere un file di testo"
html_title:           "Javascript: Leggere un file di testo"
simple_title:         "Leggere un file di testo"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché
Ci sono molte ragioni per cui si potrebbe voler leggere un file di testo in Javascript. Potresti aver bisogno di accedere ai dati contenuti nel file, di modificarli o di utilizzarli per creare una nuova funzionalità nel tuo progetto.

## Come fare
Per leggere un file di testo in Javascript, è necessario utilizzare la funzione `readFile()` del modulo `fs` (file system). Ecco un esempio di codice che mostra come leggere un file di testo e stampare il suo contenuto sulla console:

```javascript
const fs = require('fs');

fs.readFile('nome_file.txt', 'utf8', (err, data) => {
  if (err) throw err;
  console.log(data);
});
```

Il primo parametro della funzione `readFile()` è il nome del file che si desidera leggere, mentre il secondo parametro indica il formato di codifica del file. In questo caso, stiamo utilizzando `utf8` per i file di testo. Il terzo parametro è una funzione di callback che viene eseguita una volta che il file è stato letto con successo. All'interno della funzione di callback, è possibile accedere ai dati del file tramite il parametro `data`.

Di seguito puoi trovare un esempio di output ottenuto utilizzando il codice sopra descritto con un file di testo di esempio chiamato `nome_file.txt`:

```javascript
Questo è il contenuto del mio file di testo.
```

## Approfondimento
Esistono diverse operazioni che è possibile effettuare dopo aver letto un file di testo in Javascript. Ad esempio, è possibile utilizzare la funzione `writeFile()` per scrivere i dati letti da un file su un nuovo file o su uno già esistente. Inoltre, è possibile anche utilizzare la funzione `split()` per suddividere i dati del file in array, utilizzando un carattere specifico come separatore.

Per ulteriori informazioni su come utilizzare il modulo `fs` e sulle varie funzioni disponibili per leggere e scrivere file in Javascript, si consiglia di consultare la documentazione ufficiale: [https://nodejs.org/api/fs.html](https://nodejs.org/api/fs.html).

## Vedi anche
- [Tutorial: Come leggere un file di testo in Javascript](https://www.html.it/pag/66363/javascript-leggere-file/)
- [Funzioni di file system in Node.js](https://www.tutorialspoint.com/nodejs/nodejs_file_system.htm)
- [Documentazione ufficiale sul modulo fs di Node.js](https://nodejs.org/api/fs.html)