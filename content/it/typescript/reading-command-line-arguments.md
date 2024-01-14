---
title:    "TypeScript: Leggere gli argomenti della riga di comando"
keywords: ["TypeScript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Molte volte, come programmatori, abbiamo bisogno di accedere direttamente ai dati del nostro programma senza dover modificare il codice sorgente. Questo può essere utile per eseguire operazioni diverse o per fare test rapidi senza dover riscrivere il codice ogni volta. Una delle migliori e più semplici soluzioni per questo è utilizzare gli argomenti della linea di comando.

## Come Usare

Per leggere gli argomenti della linea di comando in TypeScript, possiamo utilizzare la classe "process" che ci consente di accedere a variabili di ambiente, argomenti e altro ancora. Vediamo un esempio semplice:

```TypeScript
const args = process.argv.slice(2);
console.log(args);
```
Questo codice ci permette di ottenere tutti gli argomenti della linea di comando, escludendo i primi due (che corrispondono al percorso del nodo e al percorso dello script). Ad esempio, se eseguiamo il nostro programma con `node index.ts hello world`, l'output sarà `[ 'hello', 'world' ]`.

Inoltre, per accedere a argomenti specifici, possiamo utilizzare gli indici dell'array di "args". Ad esempio, se vogliamo accedere solo al primo argomento (in questo caso, "hello"), possiamo farlo con `args[0]`.

## Deep Dive

La classe "process" offre molte altre funzionalità utili per leggere e gestire gli argomenti della linea di comando. Ad esempio, possiamo utilizzare il metodo "process.exit()" per uscire dal nostro programma con un determinato codice di uscita. Inoltre, possiamo utilizzare la proprietà "process.env" per accedere alle variabili di ambiente e utilizzarle come argomenti.

Inoltre, TypeScript ci consente di definire tipi per i nostri argomenti, rendendo il nostro codice più sicuro e leggibile. Ad esempio, se vogliamo assicurarci che il nostro primo argomento sia una stringa, possiamo specificarlo come `args[0]: string`.

Infine, possiamo utilizzare librerie esterne per rendere la lettura e la gestione degli argomenti ancora più semplice. Alcuni esempi di queste librerie sono "yargs" e "commander".

## Vedi Anche

- [Documentazione di TypeScript su "process" (in inglese)](https://nodejs.org/api/process.html)
- [Libreria "yargs" per la gestione degli argomenti (in inglese)](https://www.npmjs.com/package/yargs)
- [Libreria "commander" per la gestione degli argomenti (in inglese)](https://www.npmjs.com/package/commander)