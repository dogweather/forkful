---
title:    "Javascript: Capitalizzare una stringa"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/it/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

##Perché
In JavaScript, capitalizzare una stringa è importante per assicurarsi che il testo sia corretto e leggibile. Inoltre, alcune funzioni e librerie potrebbero richiedere che le stringhe siano in maiuscolo o in minuscolo per funzionare correttamente.

##Come Fare
Per capitalizzare una stringa in JavaScript, è possibile utilizzare il metodo "toUpperCase()" per convertire tutti i caratteri in maiuscolo:

```javascript
let stringa = "ciao mondo";
console.log(stringa.toUpperCase()); // Output: CIAO MONDO
```

Invece, per convertire tutti i caratteri in minuscolo, si può utilizzare il metodo "toLowerCase()" :

```javascript
let stringa = "CIAO MONDO";
console.log(stringa.toLowerCase()); // Output: ciao mondo
```

È anche possibile capitalizzare solo la prima lettera di una stringa utilizzando il metodo "charAt()" insieme ad altri metodi di stringa:

```javascript
let stringa = "ciao mondo";
console.log(stringa.charAt(0).toUpperCase() + stringa.slice(1).toLowerCase()); // Output: Ciao mondo
```

##Approfondimento
È importante notare che i metodi "toUpperCase()" e "toLowerCase()" non modificano la stringa originale, ma ne restituiscono una nuova. Inoltre, questi metodi non solo lavorano con le lettere dell'alfabeto ma anche con caratteri speciali, numeri e spazi.

Inoltre, è possibile capitalizzare una stringa utilizzando espressioni regolari o creando una funzione personalizzata. Ci sono diverse librerie JavaScript disponibili che offrono funzioni avanzate per la manipolazione delle stringhe, come "lodash" e "string.js".

##Vedi Anche
- https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase
- https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase
- https://lodash.com/
- https://stringjs.com/