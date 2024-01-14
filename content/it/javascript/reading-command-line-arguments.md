---
title:                "Javascript: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché
Il leggere gli argomenti della riga di comando è fondamentale per un programmatore Javascript, in quanto permette di passare informazioni direttamente al programma durante l'esecuzione. Questo può risultare estremamente utile in una varietà di situazioni, ad esempio per personalizzare l'esecuzione del programma o per passare dati dinamici.

## Come Fare
Per leggere gli argomenti della riga di comando in Javascript, si può utilizzare l'oggetto `process.arggv`. Questo oggetto contiene un array con tutti gli argomenti passati al programma durante l'esecuzione. Vediamo un esempio:

```Javascript
let args = process.argv;
console.log(args);
```

In questo esempio, il programma stamperà su console l'array `args` che conterrà tutti gli argomenti passati. Ad esempio, se il programma viene eseguito con `node index.js arg1 arg2`, l'array `args` conterrà `[ "node", "index.js", "arg1", "arg2" ]`.

Si può anche ottenere solo gli argomenti dopo il primo tramite `args.slice(2)`, poiché i primi due argomenti (`node` e `index.js`) sono sempre presenti.

```Javascript
let args = process.argv.slice(2);
console.log(args);
```

L'utilizzo degli argomenti della riga di comando è spesso utile per personalizzare il funzionamento del programma in base alle esigenze dell'utente. Ad esempio, si può chiedere all'utente di inserire il nome del file su cui lavorare o di specificare delle opzioni aggiuntive.

## Approfondimento
Esistono molti modi diversi per leggere e gestire gli argomenti della riga di comando in Javascript. Ad esempio, si può utilizzare una libreria esterna come `yargs` che semplifica notevolmente la gestione degli argomenti e permette di definire opzioni e comandi personalizzati.

Inoltre, si possono utilizzare degli statement di condizione per verificare la presenza o il valore di determinati argomenti e agire di conseguenza. In questo modo, si può rendere il programma più flessibile e adatto a molteplici situazioni.

## Vedi Anche
- [Documentazione di Node.js su process.argv](https://nodejs.org/dist/latest-v12.x/docs/api/process.html#process_process_argv)
- [Libreria yargs](https://www.npmjs.com/package/yargs)
- [Tutorial su come gestire gli argomenti della riga di comando in Javascript](https://www.digitalocean.com/community/tutorials/nodejs-command-line-arguments-node-scripts)