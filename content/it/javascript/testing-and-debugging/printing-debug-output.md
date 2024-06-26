---
date: 2024-01-20 17:52:47.622809-07:00
description: "How to: (Come fare:) Historically, debugging was done by meticulously\
  \ sifting through code and using cumbersome methods like print statements. With\u2026"
lastmod: '2024-04-05T22:50:57.601333-06:00'
model: gpt-4-1106-preview
summary: (Come fare:) Historically, debugging was done by meticulously sifting through
  code and using cumbersome methods like print statements.
title: Stampa dell'output di debug
weight: 33
---

## How to: (Come fare:)
```javascript
// Esempio semplice di console.log
console.log("Ciao, sto debuggando!");

// Stampare variabili
let fruttoPreferito = "mela";
console.log("Il mio frutto preferito è:", fruttoPreferito);

// Debugging con template literals
let numeroMagico = 42;
console.log(`Il numero magico è ${numeroMagico}`);

// Raggruppare output per una migliore leggibilità
console.group('Dettagli Utente');
console.log('Nome: Mario');
console.log('Cognome: Rossi');
console.log('Età: 30');
console.groupEnd();
```
Output:
```
Ciao, sto debuggando!
Il mio frutto preferito è: mela
Il numero magico è 42
Dettagli Utente
    Nome: Mario
    Cognome: Rossi
    Età: 30
```

## Deep Dive (Approfondimento)
Historically, debugging was done by meticulously sifting through code and using cumbersome methods like print statements. With JavaScript, `console.log()` became the go-to for quick-and-dirty debugs. JavaScript’s console API provides other methods too:

- `console.error()`: Mostra messaggi d'errore in rosso, utili per distinguere errori.
- `console.warn()`: Emite avvertimenti in giallo, meno gravi degli errori ma comunque da notare.
- `console.info()`: Per informazioni generali, spesso visualizzate con un'icona informativa.

Strumenti di sviluppo moderni offrono anche breakpoint e ispezionamento del DOM, ma `console.log()` resta uno strumento veloce e potente. Attenzione però a non lasciare stampe di debug nel codice di produzione; possono rallentare l’esecuzione e divulgare informazioni sensibili.

## See Also (Vedi Anche)
- Console API documentation on MDN Web Docs: [MDN - Console](https://developer.mozilla.org/en-US/docs/Web/API/Console)
- Guide to JavaScript template literals: [MDN - Template Literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
