---
aliases:
- /it/typescript/reading-command-line-arguments/
date: 2024-01-20 17:57:09.101131-07:00
description: "Leggere gli argomenti della riga di comando significa estrarre le informazioni\
  \ inserite dagli utenti quando lanciano un programma da terminale. I\u2026"
lastmod: 2024-02-18 23:08:55.655024
model: gpt-4-1106-preview
summary: "Leggere gli argomenti della riga di comando significa estrarre le informazioni\
  \ inserite dagli utenti quando lanciano un programma da terminale. I\u2026"
title: Lettura degli argomenti della riga di comando
---

{{< edit_this_page >}}

## What & Why?
Leggere gli argomenti della riga di comando significa estrarre le informazioni inserite dagli utenti quando lanciano un programma da terminale. I programmatori usano questa funzionalità per personalizzare l'esecuzione dell'applicazione basandosi sui dati forniti.

## How to:
Per leggere gli argomenti della riga di comando in TypeScript, usiamo `process.argv`. Ecco un esempio:

```typescript
// salva come getArguments.ts

function getArguments() {
  // process.argv contiene i tuoi argomenti
  // [0] è il percorso di node, [1] è il percorso del tuo script
  // gli argomenti iniziano dal index [2]
  return process.argv.slice(2);
}

const args = getArguments();

console.log(args);
```

Compila con `tsc getArguments.ts` e poi esegui con `node getArguments uno due tre`. Vedrai:

```shell
[ 'uno', 'due', 'tre' ]
```

## Deep Dive
L'uso della riga di comando risale agli albori dell'informatica, quando era l'unico modo per interagire con i computer. In TypeScript, `process` è un oggetto globale che fornisce informazioni su, e controllo sul, il processo Node.js corrente. Alternative moderne includono librerie come `yargs` o `commander`, che rendono più semplice la gestione degli argomenti, specialmente quando sono complessi.

Dettagli sull'implementazione: `process.argv` è un array e perciò può essere manipolato con metodi JavaScript standard. Fai attenzione a validare e sanificare gli argomenti per evitare vulnerabilità di sicurezza.

## See Also
- [Node.js documentation for process.argv](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- [yargs, npm package](https://www.npmjs.com/package/yargs)
- [commander, npm package](https://www.npmjs.com/package/commander)
