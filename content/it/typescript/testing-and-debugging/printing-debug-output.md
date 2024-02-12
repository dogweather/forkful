---
title:                "Stampa dell'output di debug"
aliases: - /it/typescript/printing-debug-output.md
date:                  2024-01-20T17:53:26.024011-07:00
model:                 gpt-4-1106-preview
simple_title:         "Stampa dell'output di debug"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## What & Why?
Stampare l'output di debug è mostrare valori e messaggi nel terminale per tracciare cosa succede nel tuo codice. I programmatori lo fanno per capire meglio i flussi logici e individuare errori (bugs).

## How to:
```TypeScript
function somma(a: number, b: number): number {
    console.log(`Sommando ${a} + ${b}`);
    return a + b;
}

const risultato = somma(5, 7);
console.log(`Risultato: ${risultato}`);
```
Output:
```
Sommando 5 + 7
Risultato: 12
```

## Deep Dive:
L'output di debug in TypeScript si fa spesso con `console.log()`, ereditato da JavaScript. Prima dei moderni IDE e debugger, il debug era principalmente manuale; i vecchi rituali inclusivano anche il controllo dei log dei server o l'inserimento di codice per scrivere su file. Alternative moderne includono l'uso di debugger integrati o strumenti di profiling per analizzare il flusso del programma. Ricordati che lasciare troppo codice di debug può rendere il codice meno leggibile e rallentare l'esecuzione. Quindi, pulisci quando hai finito!

## See Also:
- [Node.js Debugging](https://nodejs.org/api/debugger.html)
- [TypeScript Handbook](https://www.typescriptlang.org/docs/home.html)
- [MDN Web Docs: Console](https://developer.mozilla.org/en-US/docs/Web/API/Console)
