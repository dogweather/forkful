---
title:    "Javascript: Creare un file temporaneo"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Perché
Creare un file temporaneo è una pratica comune nella programmazione in linguaggio Javascript. Questo strumento ti permette di gestire in modo efficiente dati temporanei o di supporto durante l'esecuzione di un programma.

## Come Fare
Per creare un file temporaneo in Javascript, puoi utilizzare la funzione `fs.mkdtempSync()` del modulo `fs`.
```Javascript
const fs = require('fs');

// Definisci un prefisso per il nome del file temporaneo
const prefix = 'temp-';

// Specifica la directory in cui verrà creato il file temporaneo
const dir = '/tmp/';

// Utilizza la funzione fs.mkdtempSync() per creare il file temporaneo
const tempFile = fs.mkdtempSync(dir + prefix);

// Stampa il nome del file temporaneo creato
console.log(tempFile); // Output: /tmp/temp-8xytv3b7
```

## Approfondimento
Questa funzione crea un file temporaneo in modo sicuro, garantendo che il nome del file sia univoco e che non vi siano collisioni tra più processi che lo utilizzano contemporaneamente. Inoltre, il file verrà automaticamente eliminato dal sistema al termine dell'esecuzione del programma.

È possibile specificare un prefisso personalizzato per il nome del file temporaneo e la directory in cui verrà creato. Inoltre, la funzione restituisce il percorso completo del file temporaneo appena creato.

## Vedi Anche
- [Documentazione per la funzione fs.mkdtempSync()](https://nodejs.org/api/fs.html#fs_fs_mkdtempsync_prefix_options)
- [Esempio di utilizzo della funzione fs.mkdtempSync()](https://www.w3schools.com/nodejs/met_fs_mkdtempsync.asp)