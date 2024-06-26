---
date: 2024-01-20 17:51:53.852270-07:00
description: "How to: Con TypeScript, interpolare \xE8 facile come usare il backtick\
  \ (\\`) e le espressioni `${}`. Ecco un esempio."
lastmod: '2024-03-13T22:44:43.162616-06:00'
model: gpt-4-1106-preview
summary: "Con TypeScript, interpolare \xE8 facile come usare il backtick (\\`) e le\
  \ espressioni `${}`."
title: Interpolazione di una stringa
weight: 8
---

## How to:
Con TypeScript, interpolare è facile come usare il backtick (\`) e le espressioni `${}`. Ecco un esempio:

```typescript
let nome = "Mario";
let saluto = `Ciao ${nome}, come stai?`;

console.log(saluto); // Ciao Mario, come stai?
```

Cosa succede se combini numeri? Nessun problema, TypeScript gestisce anche quello:

```typescript
let ore = 8;
let messaggio = `Ci sono ${24 - ore} ore rimanenti nella giornata.`;

console.log(messaggio); // Ci sono 16 ore rimanenti nella giornata.
```

## Deep Dive
L'interpolazione di stringhe non è un concetto nuovo; esisteva in linguaggi come Perl e Python prima dell'avvento di JavaScript ES6, dove è stato introdotto come Template Literals. Prima di questo, i programmatori dovevano concatenare le stringhe e le variabili, spesso in modo goffo e meno leggibile. Ad esempio, `"Ciao " + nome + ", come stai?"`.

Le alternative? Oltre alla concatenazione, esistono librerie come Lodash che forniscono funzioni di templating, ma con TypeScript, l'interpolazione nativa è così elegante e potente che raramente è necessario cercare altrove.

### Dettagli implementativi
Dietro le quinte, TypeScript trasforma i template literals in concatenazioni di stringhe equivalenti in JavaScript ES5. Questo significa che anche se scrivi codice in TypeScript usando template literals, funzionerà su browser e ambienti che supportano solo versioni precedenti di JavaScript.

## See Also
- MDN Web Docs sui Template Literals: [MDN Template Literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
- TypeScript Handbook: [TypeScript Template Strings](https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html)
- Documentazione Lodash su `_.template`: [Lodash _.template](https://lodash.com/docs/4.17.15#template)
