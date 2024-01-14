---
title:    "TypeScript: Convertire una data in una stringa"
keywords: ["TypeScript"]
---

{{< edit_this_page >}}

## Perché

In questo post, parlerò di come convertire una data in una stringa utilizzando TypeScript. La conversione di una data in una stringa è un'operazione comune nell'esecuzione di programmi che manipolano le date, quindi imparare come farlo sarà molto utile per i programmatori TypeScript.

## Come fare

Per convertire una data in una stringa utilizzando TypeScript, dobbiamo prima creare un'istanza dell'oggetto `Date` e quindi utilizzare il metodo `toLocaleString()` per ottenere la data formattata come stringa. Ecco un esempio di codice:

```TypeScript
let date = new Date();
let stringaData = date.toLocaleString();
console.log(stringaData);
```

Il codice sopra creerà un oggetto `Date` con la data corrente e quindi lo formatterà come una stringa utilizzando il metodo `toLocaleString()`. L'output del codice sarà qualcosa del genere: "20/03/2020, 10:30:00".

Inoltre, è possibile specificare le opzioni di formattazione per la stringa utilizzando il metodo `toLocaleString()` come segue:

```TypeScript
let date = new Date();
let opzioni = {weekday: 'long', year: 'numeric', month: 'long', day: 'numeric'};
let stringaData = date.toLocaleString('it-IT', opzioni);
console.log(stringaData);
```

Questo codice produrrà un output del genere: "giovedì, 20 marzo 2020".

## Approfondimento

Un aspetto importante da considerare durante la conversione di una data in una stringa è la gestione dei fusi orari. TypeScript gestisce i fusi orari in modo diverso a seconda che si stia lavorando con date in formato locale o in formato UTC. È possibile utilizzare il metodo `toLocaleString()` con l'opzione `timeZone` per specificare il fuso orario desiderato e ottenere la data convertita correttamente.

Ad esempio, se si desidera ottenere la data in formato UTC, è possibile utilizzare il seguente codice:

```TypeScript
let date = new Date();
let opzioni = {timeZone: 'UTC'};
let stringaData = date.toLocaleString('it-IT', opzioni);
console.log(stringaData);
```

## Vedi anche

- TypeScript: Date - https://www.typescriptlang.org/docs/handbook/standard-library.html#date
- Formattare una data in JavaScript - https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/Date/toLocaleString