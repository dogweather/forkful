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

## Che cos'è e perché?:
Verificare se una directory esiste è un'operazione comune per i programmatori che consente loro di gestire la struttura dei file nel loro codice. In pratica, si tratta semplicemente di verificare se una determinata directory è presente nel sistema.

## Come fare:
Utilizzando il linguaggio di programmazione Javascript, è possibile verificare se una directory esiste utilizzando la funzione integrata "fs.existsSync()". Di seguito è riportato un esempio di codice che mostra come utilizzarla:

```Javascript
const fs = require('fs');

// Verifica se la directory "documents" esiste
if (fs.existsSync('./documents')) {
  console.log('La directory esiste!');
} else {
  console.log('La directory non esiste.');
}
```
L'output di questo esempio dipende dal fatto che la directory "documents" sia presente o meno nel sistema. Se la directory esiste, sarà visualizzato il messaggio "La directory esiste!", altrimenti verrà mostrato il messaggio "La directory non esiste.".

## Approfondimento:
La necessità di verificare l'esistenza di una directory deriva dal fatto che molte operazioni sui file richiedono l'utilizzo di percorsi (path) per accedere ai file. Senza una directory esistente, queste operazioni potrebbero causare errori nel codice.

Un'alternativa all'utilizzo di "fs.existsSync()" è l'utilizzo della funzione "fs.statSync()", che fornisce informazioni dettagliate sul file o sulla directory specificata. Tuttavia, questa è una soluzione più complessa e meno comune rispetto alla semplice verifica dell'esistenza di una directory.

## Vedi anche:
- [Documentazione ufficiale di Node.js su fs.existsSync](https://nodejs.org/api/fs.html#fs_fs_existssync_path)
- [Documentazione ufficiale di Node.js su fs.statSync](https://nodejs.org/api/fs.html#fs_fs_statsync_path_options)