---
title:                "Utilizzo di un interprete interattivo (REPL)"
aliases:
- it/javascript/using-an-interactive-shell-repl.md
date:                  2024-01-26T04:15:38.798530-07:00
model:                 gpt-4-0125-preview
simple_title:         "Utilizzo di un interprete interattivo (REPL)"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/using-an-interactive-shell-repl.md"
---

{{< edit_this_page >}}

## Cos'è e Perché?
Le shell interattive, o REPL (Read-Eval-Print Loops, Cicli di Lettura-Valutazione-Stampa), ti permettono di eseguire codice al volo, testare funzioni, algoritmi o sperimentare con idee. Sono gli appunti veloci della programmazione, rapidi e senza fronzoli, senza dover configurare un intero ambiente di sviluppo.

## Come fare:
Node.js include una REPL accessibile tramite il terminale. Aprila, e sei pronto per iniziare. Ecco un assaggio:

```javascript
$ node
> let sum = (a, b) => a + b;
undefined
> sum(5, 10);
15
> .exit
```

Semplice, vero? Definisci variabili, funzioni, o esegui cicli. Quando hai finito, `.exit` ti riporta al mondo reale.

## Approfondimento
Le REPL esistono dagli anni '60 – LISP ha introdotto il concetto. L'idea: fornire un feedback immediato al programmatore. Alternative? Oltre alla REPL di Node.js, ci sono console basate su browser come gli strumenti per sviluppatori di Chrome, sandbox online come JSFiddle o IDE completi come VSCode con aree di gioco interattive.

Sotto il cofano, i flussi di lavoro REPL tipicamente:
1. Leggono l'input
2. Compilano ed eseguono il codice
3. Stampa l'output
4. Tornano al punto di partenza

È un ciclo semplice ma efficace che ha influenzato enormemente la programmazione interattiva.

## Vedi Anche
- [Documentazione REPL di Node.js](https://nodejs.org/api/repl.html)
- [Introduzione ai moduli JavaScript su REPL di Mozilla](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Modules)
- [JSFiddle](https://jsfiddle.net/)
