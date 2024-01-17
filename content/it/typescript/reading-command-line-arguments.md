---
title:                "Leggere gli argomenti della linea di comando"
html_title:           "TypeScript: Leggere gli argomenti della linea di comando"
simple_title:         "Leggere gli argomenti della linea di comando"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?: La lettura degli argomenti della riga di comando è un'attività comune tra i programmatori, che consente di accedere ai dati inseriti dall'utente al momento del lancio di un programma. Ciò è particolarmente utile per personalizzare il comportamento del programma in base alle esigenze dell'utente.

## Come fare: Di seguito sono riportati alcuni esempi di codice in TypeScript per mostrare come leggere gli argomenti della riga di comando e come utilizzarli nel programma.

```TypeScript
import * as process from 'process';

// read single argument
const argument1 = process.argv[2];
console.log(argument1);

// read multiple arguments
const arguments = process.argv.slice(2);
console.log(arguments);

// handle optional arguments
const argument2 = process.argv[2];
const argument3 = process.argv[3];
if (argument3 === "-optional") {
    console.log(argument2);
}
```

Output per il comando `node example.ts uno due -optional`:

```
uno            // argument1
[ "uno", "due" ]  // array of arguments
uno              // argument2 (if third argument is "-optional")
```

## Approfondimento: Sebbene sia una pratica comune nei programmi moderni, la lettura degli argomenti della riga di comando è stata introdotta per la prima volta nei primi sistemi operativi Unix. Prima di questo, i programmi erano progettati per accettare input solo da un file di testo o da interazioni con l'utente durante l'esecuzione.

Esistono anche altre alternative per leggere gli argomenti della riga di comando, come l'utilizzo delle variabili di ambiente o la lettura direttamente da un file di configurazione. Tuttavia, per molti casi d'uso, la lettura degli argomenti della riga di comando rimane il metodo più semplice e diretto.

Per quanto riguarda l'implementazione, TypeScript fornisce la classe `process` che consente di accedere alla riga di comando grazie al module system di Node.js. È importante sottolineare che l'ordine degli argomenti può variare a seconda del sistema operativo e del shell utilizzato.

## Vedi anche: Per ulteriori informazioni sulla lettura degli argomenti della riga di comando in TypeScript, puoi consultare la [documentazione ufficiale di Node.js](https://nodejs.org/api/process.html#process_process_argv). Se sei interessato a conoscere approfonditamente il funzionamento dei sistemi operativi Unix e della loro evoluzione, puoi leggere il libro "The Design of the UNIX Operating System" di Maurice J. Bach.