---
title:                "Convertire una stringa in minuscolo"
html_title:           "Javascript: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Cosa e perché?
In breve, convertire una stringa in minuscolo è un processo mediante il quale si riducono tutte le lettere di una stringa in caratteri minuscoli. I programamatori spesso lo fanno per uniformare le stringhe, eliminando possibili errori di formattazione e rendendole più facili da confrontare e manipolare.

## Come:
Con Javascript, è possibile utilizzare il metodo "toLowerCase()" per convertire una stringa in minuscolo. Di seguito un esempio di codice e l'output ottenuto:

```Javascript
const stringa = "CiaO a tuTTi";
console.log(stringa.toLowerCase());
// output: ciao a tutti
```

## Deep Dive:
Il processo di convertire una stringa in minuscolo risale ai primi linguaggi di programmazione che utilizzavano solo caratteri maiuscoli. Oggi, con l'avvento dei linguaggi case-sensitive, questa operazione può sembrare di poco utilizzo, ma rimane un'utile funzione per uniformare il testo prima di confrontarlo o manipolarlo. Esistono anche altre funzioni simili, come ad esempio "toUpperCase()", che converte una stringa in maiuscolo.

## See Also:
Per ulteriori informazioni sulla manipolazione delle stringhe in Javascript, si possono consultare i seguenti link:

- [MDN Web Docs su String.prototype.toLowerCase()](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)
- [W3Schools su String.toLowerCase()](https://www.w3schools.com/jsref/jsref_tolowercase.asp)