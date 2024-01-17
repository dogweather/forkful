---
title:                "Convertire una stringa in maiuscolo"
html_title:           "TypeScript: Convertire una stringa in maiuscolo"
simple_title:         "Convertire una stringa in maiuscolo"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Cosa & Perché?

Capitalizzare una stringa significa trasformare la prima lettera in maiuscolo e lasciare tutte le altre lettere in minuscolo. I programmatori spesso lo fanno per rendere più leggibile e coerente il testo visualizzato sui loro programmi.

## Come fare:

Una delle soluzioni più semplici per capitalizzare una stringa in TypeScript è utilizzare il metodo toUpperCase() insieme al metodo slice() per ottenere la prima lettera e trasformarla in maiuscolo.

```TypeScript
let testo = 'ciao a tutti!
let primoCarattere = testo.slice(0, 1).toUpperCase();
let restoCaratteri = testo.slice(1);
testo = primoCarattere + restoCaratteri;
console.log(testo) // Ciao a tutti!
```

Un'altra soluzione è utilizzare il metodo replace() con le espressioni regolari per individuare la prima lettera e sostituirla con la versione maiuscola.

```TypeScript
let testo = 'ciao a tutti!';
testo = testo.replace(/^\w/, (primaLettera) => primaLettera.toUpperCase());
console.log(testo); // Ciao a tutti!
```

## Approfondimento:

La pratica di capitalizzare le stringhe deriva dalla scrittura manuale, dove le parole iniziavano sempre con una lettera maiuscola per facilitare la lettura del testo. Negli anni, questa convenzione è stata adottata anche nei linguaggi di programmazione per rendere il codice più leggibile e coerente.

Oltre alle soluzioni sopra descritte, esistono molte librerie e pacchetti di npm che offrono funzioni specifiche per capitalizzare una stringa. Inoltre, in alcuni casi, è possibile utilizzare il CSS per ottenere lo stesso effetto. Tuttavia, è importante tenere conto delle prestazioni e della compatibilità con i vari browser.

## Vedere anche:

Per ulteriori informazioni su come manipolare le stringhe in TypeScript, è possibile consultare la documentazione ufficiale: https://www.typescriptlang.org/docs/handbook/strings.html 

Inoltre, la seguente risorsa offre un'ottima panoramica sull'utilizzo delle espressioni regolari in TypeScript: https://fireship.io/lessons/ts-regex-basics-beginners-guide/