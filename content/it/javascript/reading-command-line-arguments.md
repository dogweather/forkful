---
title:                "Javascript: Leggere gli argomenti della linea di comando"
programming_language: "Javascript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Perché:
Questa è una domanda comune tra i programmatori: perché dovrei perdere tempo a leggere gli argomenti dalla riga di comando? La risposta è semplice: leggere gli argomenti dalla riga di comando è essenziale per creare programmi flessibili e interattivi.

## Come fare:
Per leggere gli argomenti dalla riga di comando in Javascript, è necessario utilizzare il metodo `process.argv`. Questo metodo restituirà un array contenente tutti gli argomenti passati al programma dalla riga di comando. Ecco un esempio di codice:

```Javascript
var args = process.argv;
console.log(args);
```

Se eseguiamo questo codice da linea di comando `node args.js arg1 arg2`, otterremo l'output: `[ 'node', 'args.js', 'arg1', 'arg2' ]`. Possiamo anche utilizzare il metodo `slice` per selezionare solo gli argomenti senza il primo elemento (in questo caso "node" e il nome del nostro file). Ecco un esempio di codice più avanzato:

```Javascript
var args = process.argv.slice(2);
console.log(args);
```

Se eseguiamo questo codice con gli stessi argomenti, otterremo l'output: `[ 'arg1', 'arg2' ]`. Ora possiamo utilizzare questi argomenti per creare programmi dinamici che possono accettare input dalla riga di comando.

## Approfondimento:
Mentre il metodo `process.argv` è sufficiente per leggere gli argomenti dalla riga di comando, ci sono alcune librerie che possono semplificare il processo. Ad esempio, la libreria "yargs" offre funzionalità aggiuntive come la gestione di flag e opzioni per gli argomenti. Inoltre, ci sono numerose librerie che consentono di convertire automaticamente gli argomenti in tipi di dati specifici (ad esempio numeri o booleani).

## Vedi anche:
- Documentazione ufficiale di Node.js su `process.argv`: https://nodejs.org/api/process.html#process_process_argv
- Libreria "yargs": https://www.npmjs.com/package/yargs
- Libreria "commander": https://www.npmjs.com/package/commander