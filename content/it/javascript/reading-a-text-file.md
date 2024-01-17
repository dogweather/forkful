---
title:                "Lettura di un file di testo"
html_title:           "Javascript: Lettura di un file di testo"
simple_title:         "Lettura di un file di testo"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

L'esecuzione di un file di testo è un'azione comune per i programmatori JavaScript. Ciò significa che il computer legge il contenuto di un file di testo e lo utilizza per eseguire un'azione, come stampare un messaggio sullo schermo o memorizzarlo in una variabile.

I programmatori leggono i file di testo per accedere a dati importanti o per integrare informazioni nei loro programmi. Ciò consente loro di creare programmi più dinamici e personalizzati per gli utenti.

## Come fare:

Per leggere un file di testo in JavaScript, è necessario utilizzare la funzione integrata `fetch()`. Questa funzione consente di recuperare il contenuto di un file dal suo URL e di salvarlo in una variabile per l'utilizzo nel programma.

```
javascript
fetch('testo.txt')
  .then(response => response.text())
  .then(text => console.log(text));
```

L'esempio sopra recupera il contenuto di un file di testo chiamato `testo.txt` e lo stampa nella console.

## Approfondimenti:

L'azione di leggere file di testo è stata introdotta in JavaScript attraverso il metodo obsoleto `readFile()` e ora viene utilizzata principalmente con la funzione `fetch()`. Tuttavia, ci sono anche alcune alternative in JavaScript, come l'uso della libreria `fs` o del framework `Node.js` per gestire file di testo più complessi.

Quando si leggono file di testo, è importante prendere in considerazione la codifica del file, in modo da poter leggere il contenuto correttamente. Inoltre, se il file di testo è molto grande, è consigliabile utilizzare il metodo `readStream()` invece di `readFile()`, per evitare di occupare troppa memoria.

Per ulteriori informazioni su come leggere file di testo in JavaScript, puoi consultare la documentazione ufficiale di `fetch()` o cercare tutorial online su come utilizzare le librerie `fs` o `Node.js`.

## Vedi anche:

- [Digging into the JavaScript FileReader API](https://www.digitalocean.com/community/tutorials/how-to-handle-reading-files-in-javascript-with-the-filereader-api)
- [Using Files API in Node.js](https://stackabuse.com/reading-and-writing-files-in-node-js/)
- [MDN Web Docs on Reading Files in JavaScript](https://developer.mozilla.org/en-US/docs/Web/API/File/Using_files_from_web_applications)