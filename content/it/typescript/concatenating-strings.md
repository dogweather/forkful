---
title:                "Concatenazione di stringhe"
html_title:           "Bash: Concatenazione di stringhe"
simple_title:         "Concatenazione di stringhe"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/concatenating-strings.md"
---

{{< edit_this_page >}}

## Che Cosa è e Perché?

La concatenazione di stringhe è il processo di unire due o più stringhe in una sola. Questo è utile per i programmatori per costruire messaggi dinamici o formati di testo personalizzati.

## Come Fare:

In TypeScript, possiamo usare l'operatore `+` o i template di stringhe. Per esempio:

```TypeScript
// Usando l'operatore '+'
let saluto = "Ciao";
let nome = "Marco";
console.log(saluto + " " + nome);  // Output: "Ciao Marco"

// Usando i template di stringhe
console.log(`${saluto} ${nome}`);  // Output: "Ciao Marco"
```

## Approfondimento

La concatenazione delle stringhe è una pratica antica quanto la programmazione stessa. In TypeScript, l'uso dei template di stringhe (introdotti in ES6) è spesso preferito per la leggibilità e la facilità di formattazione. 

Un'alternativa è l'uso del metodo `concat()`, che unisce le stringhe passate come argomenti. Ecco un esempio:

```TypeScript
console.log(saluto.concat(" ", nome));  // Output: "Ciao Marco"
```

Tuttavia, per stringhe più lunghe o complesse, l'uso di `+` o dei template di stringhe è spesso più efficiente sia in termini di pulizia del codice sia di performance.

## Vedi Anche

Per ulteriori dettagli sulla concatenazione delle stringhe in TypeScript, consultare:

1. [Stringhe - TypeScript Deep Dive](https://basarat.gitbook.io/typescript/type-system/string)
2. [Metodo `concat()` - MDN Web Docs](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/concat)