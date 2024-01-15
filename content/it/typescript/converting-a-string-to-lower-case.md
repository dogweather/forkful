---
title:                "Convertire una stringa in minuscolo"
html_title:           "TypeScript: Convertire una stringa in minuscolo"
simple_title:         "Convertire una stringa in minuscolo"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## Perché
Ci sono diverse ragioni per cui potresti voler convertire una stringa in lettere minuscole in TypeScript. Una delle ragioni più comuni è che è importante mantenere la coerenza nella formattazione del testo all'interno del tuo codice. Inoltre, convertire una stringa in lettere minuscole potrebbe essere utile quando si compara o si cerca all'interno di una stringa.

## Come fare
Per convertire una stringa in lettere minuscole in TypeScript, puoi utilizzare il metodo built-in `toLowerCase()`. Questo metodo può essere utilizzato su una variabile di tipo stringa e restituirà una nuova stringa contenente solamente lettere minuscole.

```TypeScript
let testo = "CIAO AMICI";
console.log(testo.toLowerCase()); // output: ciao amici
```

In questo esempio, abbiamo definito una variabile `testo` che contiene la stringa "CIAO AMICI". Utilizzando il metodo `toLowerCase()`, abbiamo convertito il testo in lettere minuscole e poi stampato il risultato in console.

## Approfondimento
È importante notare che il metodo `toLowerCase()` non altera la stringa originale, ma ne crea una nuova con le lettere in minuscolo. Se si desidera conservare la stringa originale, è necessario assegnare il risultato del metodo a una nuova variabile.

Inoltre, è possibile che il metodo `toLowerCase()` non funzioni correttamente su certe lettere con accento o caratteri speciali, poiché il comportamento può variare a seconda del sistema operativo e della codifica dei caratteri.

## Vedi anche
- Il metodo `toLowerCase()` nella documentazione ufficiale di TypeScript: https://www.typescriptlang.org/docs/handbook/release-notes/typescript-1-8.html#lowercaseAndUppercase
- Un esempio di utilizzo di `toLowerCase()` in un progetto TypeScript: https://medium.com/@jorgeucano/manipulando-strings-que-es-to-lower-case-to-upper-case-normalize-y-more-d059c12c8be6
- Una spiegazione dettagliata su come funzionano le codifiche dei caratteri e perché possono influire su `toLowerCase()`: https://www.toptal.com/developers/string-encoding-tutorial