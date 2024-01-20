---
title:                "Lettura degli argomenti della riga di comando"
html_title:           "Java: Lettura degli argomenti della riga di comando"
simple_title:         "Lettura degli argomenti della riga di comando"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Cos'è & Perché?

La lettura degli argomenti da linea di comando è un modo per ottenere i dati di input per una applicazione da un terminale. Gli sviluppatori la utilizzano per dare agli utenti la capacità di personalizzare il comportamento delle applicazioni al momento dell'esecuzione.

## Come si fa:

Ecco un esempio di come leggere gli argomenti da riga di comando in TypeScript, utilizzando il vettore di processo `process.argv`:

```TypeScript
// Esempio TypeScript per leggere gli argomenti dalla linea di comando
process.argv.forEach((val, index) => {
  console.log(`${index}: ${val}`);
});
```
Quando eseguiamo lo script con alcuni argomenti si avrà un output simile a questo:
```
0: node
1: /path/to/your/script.js
2: argomento1
3: argomento2
```

## Approfondimenti

Storicamente, la lettura degli argomenti da linea di comando è un concetto ereditato dai primi linguaggi di programmazione come C e Perl. In TypeScript, l'oggetto `process.argv` è un array contenente gli argomenti passati a un programma Node.js. 

Se si richiede una maggiore flessibilità nello smistamento e nell'analisi dei parametri della linea di comando, si possono utilizzare librerie come `commander` o `yargs`. 

Vale la pena notare che `process.argv` include due argomenti predefiniti: il percorso all'eseguibile node (`process.argv[0]`) e il percorso allo script corrente (`process.argv[1]`). Gli argomenti reali iniziano dall'indice 2.

## Per approfondire

- Documentazione delle librerie [Commander.js](https://www.npmjs.com/package/commander) e [yargs.js](https://www.npmjs.com/package/yargs).