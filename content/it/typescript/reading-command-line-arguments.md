---
title:                "TypeScript: Leggere gli argomenti della riga di comando"
simple_title:         "Leggere gli argomenti della riga di comando"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché

Molti programmatori sono familiari con il concetto di linee di comando, ma pochi comprendono l'importanza dei loro argomenti. In questo articolo, esploreremo come leggere gli argomenti della riga di comando in TypeScript e come possono essere utili per i nostri progetti.

## Come Fare

```TypeScript
// Esempio di codice TypeScript per leggere gli argomenti della riga di comando
const args = process.argv.slice(2); //elimina i primi due argomenti che rappresentano il percorso del file e il comando node

// Stampa tutti gli argomenti passati alla riga di comando
console.log(args);
```

L'esempio sopra mostra come utilizzare il modulo globale `process` per accedere agli argomenti della riga di comando in TypeScript. Il metodo `slice` viene utilizzato per rimuovere i primi due argomenti, che non sono pertinenti per il nostro scopo. Quindi, i restanti argomenti vengono stampati a console. Ecco un esempio di output:

```
Input: node index.ts arg1 arg2 arg3
Output: ['arg1', 'arg2', 'arg3']
```

Possiamo anche accedere agli argomenti specifici utilizzando l'indice appropriato. Ad esempio, per ottenere il terzo argomento (in questo caso `arg3`), dobbiamo utilizzare `args[2]`. Inoltre, possiamo controllare la lunghezza dell'array `args` per gestire dinamicamente la quantità di argomenti passati.

```TypeScript
// Esempio di codice TypeScript per gestire un numero variabile di argomenti
const args = process.argv.slice(2);

// Verifica se almeno un argomento è stato passato
if (args.length > 0) {
  console.log(`Il primo argomento passato è ${args[0]}`);
}
```

## Deep Dive

Oltre alla lettura dei singoli argomenti, possiamo anche utilizzare la libreria `yargs` per gestire facilmente tutti gli argomenti della riga di comando. Questa libreria semplifica la gestione dei flag, delle opzioni e dei parametri dei nostri argomenti. Vediamo un esempio:

```TypeScript
// Esempio di codice TypeScript per utilizzare la libreria yargs
import * as yargs from 'yargs';

// Aggiunge un flag alla riga di comando
yargs.boolean('big'); // accetta un flag --big

// Aggiunge una opzione con uno short name e un valore di default
yargs.option('name', {
  alias: 'n', // utilizza `-n` come short name
  default: 'world', // utilizza `world` come valore di default
});

// Aggiunge un parametro obbligatorio
yargs.demandCommand(1); // richiede almeno un argomento senza flag o opzione

// Esegui il parsing degli argomenti della riga di comando
const argv = yargs.parse()

// Stampa il valore dell'opzione `name` se passato, altrimenti stampa `world`
console.log(`Hello ${argv.name}!`);

// Stampiamo in maiuscolo se è stato passato il flag `--big`
if (argv.big) {
  console.log(`HELLO ${argv.name.toUpperCase()}!`);
}
```

Puoi anche aggiungere una descrizione ai tuoi argomenti utilizzando `yargs` per una migliore comprensione e documentazione del tuo codice.

## Vedi Anche

- Documentazione ufficiale di [yargs](https://github.com/yargs/yargs)
- Tutorial su come scrivere linee di comando in TypeScript con [commander](https://github.com/tj/commander.js)
- Articolo su [Node.js guide for CLI apps](https://www.twilio.com/blog/node-js-command-line-apps)