---
title:                "Lettura di un file di testo"
html_title:           "C: Lettura di un file di testo"
simple_title:         "Lettura di un file di testo"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Che cosa & Perché?

Leggere un file di testo significa accedere e interpretare i dati salvati in un file come stringhe di testo. I programmatori lo fanno per manipolare o analizzare i dati, riutilizzarli nell'applicazione e condividere le informazioni tra diverse sessioni o utenti.

## Come si fa:

Ecco un esempio di come leggere un file di testo in TypeScript, utilizzando il modulo `fs` di Node.js:

```TypeScript
import fs from 'fs';

fs.readFile('testo.txt', 'utf8' , (err, data) => {
  if (err) {
    console.error(err);
    return;
  }
  console.log(data);
});
```

Se il file 'testo.txt' contiene "Ciao Mondo", l'output sarà:

```TypeScript
Ciao Mondo
```

## Un tuffo più profondo:

Historicamente, la lettura dei file di testo è uno dei primi metodi utilizzati per la persistenza dei dati nelle applicazioni. Prima degli avanzati sistemi di database, abbiamo iniziato con file di testo.

Alternativamente, potresti utilizzare il modulo `readline` di Node.js o librerie di terze parti come `csv-parser` per leggere file di testo più complessi come i file CSV. 

L'aspetto di implementazione più importante durante la lettura dei file di testo è la gestione degli errori. Il modulo `fs.readFile` ha un callback che contiene un errore, nel caso in cui il file non possa essere letto. È fondamentale gestire queste eccezioni per prevenire crash inaspettati del programma. 

## Si veda anche:

- [Documentazione Node.js fs](https://nodejs.org/api/fs.html)
- [Modulo Node.js Readline](https://nodejs.org/api/readline.html)
- [Libreria csv-parser](https://www.npmjs.com/package/csv-parser)

Questi collegamenti ti aiuteranno a esplorare ulteriormente le opzioni disponibili per leggere i file di testo in TypeScript.