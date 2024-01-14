---
title:    "Javascript: Lettura di un file di testo"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Perché

Leggere un file di testo tramite codice JavaScript può essere una parte fondamentale del processo di sviluppo di un'applicazione web. Ciò consente di accedere ed elaborare i dati presenti in un file di testo, che può essere utilizzato per una varietà di scopi come la memorizzazione di configurazioni, il salvataggio di dati utente e altro ancora.

## Come

Per iniziare a leggere un file di testo utilizzando JavaScript, è necessario innanzitutto accedere al modulo "fs" built-in di Node.js. Questo modulo fornisce le funzionalità per lavorare con file di sistema, inclusa la lettura di file di testo. Ecco un esempio di codice che utilizza la funzione "readFile" per leggere un file di testo:

```Javascript
const fs = require('fs');

fs.readFile('test.txt', 'utf8', (err, data) => {
  if (err) throw err;
  console.log(data);
});
```

In questo esempio, stiamo leggendo un file chiamato "test.txt" e passando la codifica "utf8" per assicurarci che venga interpretato correttamente come testo. La funzione di callback, che viene eseguita quando il file è stato letto con successo, stampa il contenuto del file nella console.

## Deep Dive

Oltre alla semplice lettura del contenuto di un file di testo, è possibile effettuare diverse elaborazioni utilizzando le funzioni fornite dal modulo "fs". Ad esempio, è possibile utilizzare la funzione "readFileSync" per leggere un file in modo sincrono, cioè senza la necessità di un callback. Inoltre, è possibile passare un oggetto di opzioni come secondo argomento per specificare la codifica e altre opzioni di lettura.

Inoltre, il modulo "fs" fornisce anche la possibilità di scrivere e creare file di testo tramite le funzioni "writeFile" e "writeFileSync". Questo può essere utile per salvare dati di output o creare file di configurazione durante l'esecuzione del programma.

## Vedi anche

- [Documentazione del modulo "fs" di Node.js (in inglese)](https://nodejs.org/dist/latest-v15.x/docs/api/fs.html)
- [Esempi di codice per leggere e scrivere file di testo con Node.js (in inglese)](https://stackabuse.com/reading-and-writing-json-files-with-node-js/)