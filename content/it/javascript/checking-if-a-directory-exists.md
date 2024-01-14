---
title:    "Javascript: Verifica se una directory esiste."
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Perché
Spesso durante la programmazione, potresti trovarti nella necessità di verificare se una directory esiste o meno. Questa operazione è particolarmente utile nel caso in cui tu debba manipolare file all'interno di una directory o creare una nuova directory solo se quella desiderata non esiste già.

## Come fare
Nella programmazione Javascript, è possibile verificare l'esistenza di una directory utilizzando la funzione `fs.existsSync()` del modulo `fs`. Questa funzione prende come argomento il percorso della directory che si vuole verificare e restituisce un valore booleano (`true` se la directory esiste, `false` se non esiste). Vediamo un esempio pratico di come utilizzare questa funzione:

```Javascript
const fs = require('fs');

// Verifica l'esistenza della directory "documents"
if (fs.existsSync("./documents")) {
  console.log("La directory 'documents' esiste già");
} else {
  console.log("La directory 'documents' non esiste");
}
```

Nell'esempio sopra, stiamo utilizzando la funzione `fs.existsSync()` per verificare se la directory "documents" esiste nella stessa cartella del file Javascript in cui stiamo lavorando. Per utilizzarla in un progetto reale, dovremmo cambiare il percorso del file in base alla nostra specifica esigenza.

## Approfondimento
Facciamo un passo indietro e analizziamo meglio la funzione `fs.existsSync()`. Come accennato in precedenza, questa funzione restituisce un valore booleano, ma come fa esattamente a determinare se la directory esiste o meno? Innanzitutto, è importante sottolineare che questa funzione fa parte del modulo `fs`, che fornisce un'interfaccia per interagire con il sistema di file del computer. Per poter utilizzare questa funzione, dobbiamo quindi prima importare il modulo tramite la keyword `require()`. Una volta importato, possiamo invocare la funzione e passare come argomento il percorso della directory che desideriamo verificare.

```Javascript
const fs = require('fs');
const path = require('path');

function checkDirExists(directory) {
  return fs.existsSync(path.join(__dirname, directory));
}
```

Nell'esempio sopra, stiamo definendo una funzione che prende come argomento il nome di una directory e utilizza la funzione `path.join()` per unire il percorso della directory passato come argomento con il percorso corrente della cartella in cui si trova il file Javascript. Questo ci permette di utilizzare la funzione indipendentemente dal percorso in cui ci troviamo. Infine, stiamo restituendo il valore booleano di `fs.existsSync()` utilizzando `path.join()`.

## Vedi anche
- [Documentazione ufficiale di `fs.existsSync()`](https://nodejs.dev/learn/the-nodejs-fs-module#fs-existsync)
- [Documentazione ufficiale di `path.join()`](https://nodejs.dev/learn/nodejs-path-module#nodejs-path-join)
- [Guida all'utilizzo dei moduli in Node.js](https://nodejs.dev/learn/using-experimental-modules)