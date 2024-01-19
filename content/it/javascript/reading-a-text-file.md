---
title:                "Lettura di un file di testo"
html_title:           "C: Lettura di un file di testo"
simple_title:         "Lettura di un file di testo"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Lettura di un file di testo in Javascript: Un Guida Concisa

## Che cosa e perché?
Lettura di un file di testo è il processo di recupero dei dati da un file di testo. I programmatori lo fanno per ottenere i dati in un formato utilizzabile all'interno del loro codice.

## Come fare:
```Javascript
const fs = require('fs');

fs.readFile('/percorso/del/tuo/file.txt', 'utf8', (err, data) => {
  if (err) {
    console.error(err)
    return
  }
  console.log(data)
})
```
Questo frammento di codice mostra come leggere un file di testo in un percorso specificato. In caso di errore, verrà stampato un messaggio di errore.

Usando il metodo `readFile`, otterrete un output simile al seguente se il contenuto del file è "Ciao, Mondo!":
```
Ciao, Mondo!
```

## Approfondimenti

La lettura di file di testo è una pratica comune in programmazione sin dalle origini del settore. Inizialmente i file di testo erano il principale mezzo di archiviazione di dati, sebbene questa pratica sia in declino con l'introduzione di database più efficienti e veloci. Tuttavia, leggere file di testo è ancora una abilità essenziale per vari compiti, come il parsing di log o la lettura di configurazioni.

Esistono diverse alternative a `fs.readFile`, come `fs.readFileSync` per una lettura sincrona o l'utilizzo di stream per gestire file di grandi dimensioni.

L'implementazione di `fs.readFile` in Node.js utilizza le API di basso livello di libuv, una libreria C multipiattaforma. Questo rende la lettura dei file in Node.js estremamente veloce ed efficiente.

## Vedi Anche
- [Node.js fs.readFile documentation](https://nodejs.org/api/fs.html#fs_fs_readfile_path_options_callback)
- [libuv documentation](http://docs.libuv.org/)
- [JavaScript Promises: An Introduction](https://developers.google.com/web/fundamentals/primers/promises)
- [A Brief History of JavaScript](https://www.w3.org/community/webed/wiki/A_Short_History_of_JavaScript)