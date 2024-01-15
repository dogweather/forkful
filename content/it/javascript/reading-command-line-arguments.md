---
title:                "Leggere gli argomenti della riga di comando"
html_title:           "Javascript: Leggere gli argomenti della riga di comando"
simple_title:         "Leggere gli argomenti della riga di comando"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

I comandi della riga di comando sono una parte fondamentale dell'esperienza di programmazione in Javascript. Saper leggere gli argomenti della riga di comando può sembrare un piccolo dettaglio, ma può facilitare enormemente la gestione dei tuoi programmi e renderli più versatili ed efficienti.

## Come Fare

La lettura degli argomenti della riga di comando in Javascript è molto semplice e può essere eseguita utilizzando l'oggetto `process.argv`. Questo oggetto contiene un array di stringhe che rappresentano gli argomenti passati all'esecuzione del tuo programma. Vediamo un esempio pratico:

```Javascript
// esempio.js

const num1 = Number(process.argv[2]);
const num2 = Number(process.argv[3]);

console.log(`Il risultato della somma è ${num1 + num2}`);
```

Se il nostro programma viene eseguito dalla riga di comando utilizzando il comando `node esempio.js 5 7`, il risultato mostrato sarebbe "Il risultato della somma è 12". Come puoi vedere, il nostro programma legge gli argomenti passati e li utilizza per eseguire l'operazione desiderata.

## Approfondimento

Oltre a leggere gli argomenti passati, è possibile anche gestire gli argomenti opzionali utilizzando il pacchetto `yargs`. Questo pacchetto semplifica la lettura e la gestione dei comandi della riga di comando fornendo una sintassi più intuitiva e facile da utilizzare. Puoi installarlo utilizzando il comando `npm install yargs` e successivamente utilizzarlo nel tuo codice:

```Javascript
// esempio.js

const argv = require('yargs').argv;

console.log(`Il nome inserito è ${argv.nome}`);
```

E se il nostro programma viene eseguito utilizzando il comando `node esempio.js --nome Mario`, il risultato mostrato sarebbe "Il nome inserito è Mario".

## Vedi Anche

- [Documentazione di Process](https://nodejs.org/api/process.html#process_process_argv)
- [Documentazione di Yargs](https://www.npmjs.com/package/yargs)