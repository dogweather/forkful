---
title:                "Creazione di un file temporaneo."
html_title:           "Javascript: Creazione di un file temporaneo."
simple_title:         "Creazione di un file temporaneo."
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?
Creare un file temporaneo è un'operazione comune per i programmatori. In poche parole, un file temporaneo è un file che viene creato per svolgere una specifica funzione e poi viene eliminato dal sistema. I programmatori lo fanno per salvare temporaneamente dei dati, per testare il funzionamento di un programma o per archiviare temporaneamente informazioni.

## Come fare:
Un esempio di codice in Javascript per creare un file temporaneo è il seguente:
```Javascript
const temp = require('temp');
temp.open('file', function (err, info) {
  fs.write(info.fd, 'Questo è un esempio di file temporaneo.', function (err) {
    fs.close(info.fd, function (err) {
      // Elimina il file temporaneo
      temp.cleanup();
    });
  });
});
```

Output:
`Il file temporaneo è stato creato con successo.`

## Approfondimento:
Creare file temporanei è una pratica molto comune nella programmazione. È spesso utilizzato per testare il funzionamento di un programma o per svolgere operazioni temporanee. Prima dell'avvento dei file temporanei, i programmatori dovevano creare file permanenti e poi eliminarli manualmente. Oggi, con l'avanzamento della tecnologia, esistono anche alternative come la creazione di file temporanei in memoria invece che sul disco rigido.

## Vedi anche:
- [Documentazione su come creare file temporanei in Node.js](https://www.npmjs.com/package/temp)
- [Una guida dettagliata su come utilizzare i file temporanei in Javascript](https://www.sitepoint.com/javascript-temporary-files/)