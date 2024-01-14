---
title:                "Javascript: Capitalizzare una stringa"
simple_title:         "Capitalizzare una stringa"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Ci sono molte ragioni per voler cambiare la capitalizzazione di una stringa in un programma Javascript. Ad esempio, puoi voler formattare una stringa in modo che abbia un aspetto uniforme e più leggibile per gli utenti. Inoltre, alcune funzioni e metodi richiedono una determinata capitalizzazione per funzionare correttamente. Fortunatamente, capitalizzare una stringa in Javascript è un'operazione semplice e veloce.

## Come

Per capitalizzare una stringa in Javascript, puoi utilizzare il metodo `toUpperCase` combinato con il metodo `charAt`. Ecco un esempio di codice:

```Javascript
let stringa = "ciao mondo";
let nuovaStringa = stringa.charAt(0).toUpperCase() + stringa.slice(1);

console.log(nuovaStringa);

// Output: "Ciao mondo"
```

In questo esempio, abbiamo utilizzato il metodo `charAt` per selezionare il primo carattere della stringa e poi lo abbiamo modificato in maiuscolo con il metodo `toUpperCase`. Infine, abbiamo aggiunto la parte rimanente della stringa utilizzando il metodo `slice`. Questo ci ha permesso di ottenere una nuova stringa con il primo carattere capitalizzato.

## Approfondimento

Se vuoi approfondire ulteriormente il processo di capitalizzazione di una stringa in Javascript, puoi utilizzare il metodo `replace` con le espressioni regolari. Ad esempio, se vuoi capitalizzare tutte le parole in una stringa, puoi utilizzare il seguente codice:

```Javascript
let stringa = "ciao mondo";
let nuovaStringa = stringa.replace(/\b\w/g, (l) => l.toUpperCase());

console.log(nuovaStringa);

// Output: "Ciao Mondo"
```

In questo esempio, abbiamo utilizzato l'espressione regolare /\b\w/g per selezionare ogni parola nella stringa e poi abbiamo utilizzato la funzione di callback per capitalizzare ogni parola selezionata.

## Vedi anche

- [MDN Web Docs: toUpperCase](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [MDN Web Docs: charAt](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)
- [MDN Web Docs: slice](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [MDN Web Docs: replace](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/replace)
- [MDN Web Docs: espressioni regolari](https://developer.mozilla.org/it/docs/Web/JavaScript/Guida/Espressioni_regolari)