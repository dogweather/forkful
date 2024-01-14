---
title:                "TypeScript: Lettura degli argomenti della linea di comando"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Spesso, quando scriviamo un programma, ci troviamo nella situazione in cui vogliamo passare dei parametri direttamente dalla riga di comando. Questi parametri sono chiamati "command line arguments" e consentono di fornire al programma delle informazioni specifiche che verranno utilizzate durante l'esecuzione. In questo post, esploreremo come leggere e gestire i command line arguments utilizzando TypeScript.

## Come fare

Per leggere i command line arguments, dobbiamo prima di tutto importare il modulo `process` di Node.js. Questo ci permette di accedere all'array di stringhe contenente i parametri passati dalla riga di comando. Vediamo un esempio pratico:

```TypeScript
import * as process from 'process';

const args: string[] = process.argv;

console.log(args);
```
L'esempio sopra stamperà su console l'array `args` contenente tutti i parametri passati dalla riga di comando. Nota che il primo elemento dell'array è sempre il percorso del file TypeScript in cui stiamo lavorando.

Possiamo anche passare dei parametri specifici nella riga di comando, come ad esempio `--nome=Marco`:

```TypeScript
import * as process from 'process';

// Leggiamo il secondo parametro
const nome: string = process.argv[2];

console.log(`Ciao ${nome}!`);
```

L'esempio sopra stamperà `Ciao Marco!` se eseguito dalla riga di comando `ts-node app.ts --nome=Marco`.

## Un po' più in profondità

Oltre ad accedere ai singoli parametri, esistono anche alcune funzionalità interessanti nel modulo `process`. Ad esempio, possiamo verificare il numero totale di parametri passati con la proprietà `process.argc`. Possiamo anche utilizzare la funzione `process.execArgv` per ottenere un array dei parametri passati all'avvio di Node.js.

Inoltre, è possibile utilizzare il modulo `yargs` per gestire in maniera più strutturata i command line arguments. Questo modulo ci permette di definire dei parametri richiesti, opzionali e anche dei comandi specifici per il nostro programma.

## Vedi anche

- [Documentazione ufficiale di Node.js su `process`](https://nodejs.org/dist/latest-v14.x/docs/api/process.html)
- [Documentazione ufficiale di `yargs`](https://github.com/yargs/yargs)