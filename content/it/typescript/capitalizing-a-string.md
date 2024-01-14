---
title:                "TypeScript: Capitalizzazione di una stringa"
programming_language: "TypeScript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché
Capitalize una stringa è un'operazione utile quando si desidera rendere più leggibile un testo o quando si deve rispettare alcune regole di formattazione specifiche.

## Come Farlo
Per capitalizzare una stringa in TypeScript, si possono seguire due approcci diversi. Il primo è utilizzare il metodo `toUpperCase()` che converte tutti i caratteri della stringa in maiuscolo. Ad esempio:

```TypeScript
let stringa = "ciao a tutti!";
console.log(stringa.toUpperCase()); // Output: CIAO A TUTTI!
```

Il secondo approccio è quello di utilizzare la funzione `charAt()` per selezionare il primo carattere della stringa e poi utilizzare il metodo `toUpperCase()` per convertirlo in maiuscolo. Infine, si concatena il resto della stringa utilizzando il metodo `slice()`. Ad esempio:

```TypeScript
let stringa = "ciao a tutti!";
let primaLettera = stringa.charAt(0).toUpperCase();
let restoStringa = stringa.slice(1);
console.log(primaLettera + restoStringa); // Output: Ciao a tutti!
```

## Deep Dive
È importante notare che il metodo `toUpperCase()` converte tutti i caratteri della stringa in maiuscolo, mentre il secondo approccio consente di mantenere il resto della stringa in minuscolo. Inoltre, è possibile utilizzare queste tecniche anche per capitalizzare una singola parola o solo le prime lettere di ogni parola in una stringa più lunga.

## Vedi Anche
- [Metodo toUpperCase() in TypeScript](https://www.typescriptlang.org/docs/handbook/strings.html#uppercase)
- [Funzione charAt() in TypeScript](https://www.typescriptlang.org/docs/handbook/strings.html#accessing-characters)