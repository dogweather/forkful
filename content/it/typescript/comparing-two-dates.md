---
title:                "Confronto di due date"
html_title:           "TypeScript: Confronto di due date"
simple_title:         "Confronto di due date"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

Cosa & Perché?

Comparare due date è un'operazione comune nella programmazione per verificare se una data è successiva, precedente o uguale ad un'altra. Questo può essere utile per eseguire controlli sulle date di scadenza o per mostrare le informazioni più aggiornate.

Come fare:

Nella seguente sezione, vedremo come effettuare il confronto tra due date utilizzando TypeScript. Utilizzeremo i metodi nativi della classe Date per ottenere le informazioni necessarie e confrontarle.

```TypeScript
// prima data
const data1 = new Date(2021, 5, 10); // 10 Giugno 2021

// seconda data
const data2 = new Date(2021, 4, 25); // 25 Maggio 2021

// confronta le due date
if (data1 > data2) {
  console.log("La prima data è successiva alla seconda data.");
} else if (data1 < data2) {
  console.log("La prima data è precedente alla seconda data.");
} else {
  console.log("Le due date sono uguali.");
}
```

Output:

```
La prima data è successiva alla seconda data.
```

Deep Dive:

Normalmente, le date vengono rappresentate da un numero intero che indica i millisecondi trascorsi dal 1 Gennaio 1970, noto come "epoch time". Questo è il valore utilizzato dalla classe Date di TypeScript per rappresentare le date. Oltre al confronto con gli operatori di confronto (>, <, ==), è possibile utilizzare il metodo `.getTime()` per ottenere il valore in millisecondi di una data e confrontarlo con un'altra.

Alternative:

Invece di utilizzare gli operatori di confronto, è possibile utilizzare i metodi della classe Date come `.getTime()`, `.getFullYear()` o `.getMonth()` per ottenere argomenti numerici e confrontare questi valori per determinare quale data è successiva, precedente o uguale. Inoltre, ci sono molte librerie esterne disponibili, come Moment.js, che semplificano il confronto tra le date in diversi formati.

See Also:

Per ulteriori informazioni su come lavorare con date in TypeScript, potete consultare la documentazione ufficiale di TypeScript sulle classi Date: https://www.typescriptlang.org/docs/handbook/2/classes.html#date.