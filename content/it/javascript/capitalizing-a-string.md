---
title:                "Capitalizzare una stringa"
html_title:           "Javascript: Capitalizzare una stringa"
simple_title:         "Capitalizzare una stringa"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Che Cosa & Perché?

Capitalizzare una stringa significa trasformare la prima lettera di ogni parola in una lettera maiuscola. I programmatori lo fanno per migliorare la leggibilità e l'estetica del testo.

## Come Fare:

Ecco un esempio di come capitalizzare una stringa in Javascript:
```Javascript
function capitalizzaStringa(stringa) {
    return stringa.charAt(0).toUpperCase() + stringa.slice(1).toLowerCase();
}

console.log(capitalizzaStringa("ciao mondo")); // Output: "Ciao mondo"
```

## Approfondimento:

- **Contesto storico**: Il bisogno di capitalizzare le stringhe risale agli albori della programmazione, da quando i calcolatori sono stati usati per manipolare il testo.
- **Alternative**: Oltre al metodo mostrato sopra, possiamo usare `split()` per dividere la stringa in parole, capitalizzare ogni parola e poi unirle con `join()`.
```Javascript
function capitalizzaStringa(stringa) {
    return stringa.split(' ').map(parola => parola.charAt(0).toUpperCase() + parola.slice(1).toLowerCase()).join(' ');
}

console.log(capitalizzaStringa("ciao mondo")); // Output: "Ciao Mondo"
```
- **Dettagli implementativi**: `charAt(0)` restituisce la prima lettera della stringa. `toUpperCase()` la trasforma in maiuscolo. `slice(1)` restituisce il resto della stringa, e `toLowerCase()` lo trasforma in minuscolo.

## Guarda Anche:

- [MDN String.prototype.charAt()](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/charAt)
- [MDN String.prototype.toUpperCase()](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- [MDN String.prototype.slice()](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/slice)
- [MDN String.prototype.toLowerCase()](https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/toLowerCase)