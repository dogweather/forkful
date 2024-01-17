---
title:                "Leggere gli argomenti dalla riga di comando"
html_title:           "Javascript: Leggere gli argomenti dalla riga di comando"
simple_title:         "Leggere gli argomenti dalla riga di comando"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Che cos'è e perché si fa?

Leggere gli argomenti della riga di comando è una pratica comune tra i programmatori per ottenere informazioni dall'utente durante l'esecuzione di un programma. Questi argomenti possono includere opzioni, valori e altro ancora, e possono essere utilizzati per personalizzare l'esecuzione del programma o per passare informazioni importanti.

## Come si fa:

```Javascript
let args = process.argv.slice(2);
console.log(args);
```

Ecco un semplice esempio di codice Javascript che legge gli argomenti dalla riga di comando e li stampa nella console. Chiamando il nostro file "myScript.js" e passando "Hello World" come argomento, l'output sarebbe: ```[ 'Hello World' ]```. Nota che il primo elemento dell'array è sempre il percorso del file in esecuzione, quindi in questo caso non viene stampato.

## Un'analisi approfondita:

La lettura degli argomenti della riga di comando ha origini molto antiche, risalenti ai primi sistemi operativi UNIX. Inoltre, non è limitata solo al linguaggio Javascript, ma viene utilizzata anche in molti altri linguaggi di programmazione come C, Java e Python.

Un'alternativa alla lettura degli argomenti della riga di comando è l'utilizzo di variabili di ambiente, che possono essere settate dall'utente e lette dal programma. Tuttavia, questa opzione non è così flessibile e può portare a problemi di sicurezza se non gestita correttamente.

Per quanto riguarda l'implementazione, la lettura degli argomenti della riga di comando può essere effettuata utilizzando la libreria standard "process" di Node.js o attraverso librerie di terze parti come "yargs" o "commander".

## Vedi anche:

- [Documentazione ufficiale di Node.js](https://nodejs.org/api/process.html#process_process_argv)
- [Libreria "yargs"](https://www.npmjs.com/package/yargs)
- [Libreria "commander"](https://www.npmjs.com/package/commander)