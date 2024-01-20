---
title:                "Capitalizzare una stringa"
html_title:           "TypeScript: Capitalizzare una stringa"
simple_title:         "Capitalizzare una stringa"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Che cos'è e perché?

La capitalizzazione di una stringa significa trasformare la prima lettera di ogni parola in maiuscolo. Questo viene fatto per migliorare la leggibilità del testo, per esempio, in titoli, nomi propri o frasi di benvenuto.

## Come fare:
Ecco un esempio di come si può capitalizzare una stringa in TypeScript:

```TypeScript
function capitalizeStr(input: string): string {
    return input.replace(/\b\w/g, function (letter) {
        return letter.toUpperCase();
    });
}

console.log(capitalizeStr('ciao mondo'));
```
Output:
```TypeScript
'Ciao Mondo'
```

In questa funzione, utilizziamo l'espressione regolare `\b\w` per trovare la prima lettera di ogni parola e la funzione `toUpperCase()` per convertirla in maiuscolo.

## Approfondimento

La capitalizzazione di stringhe è un concetto comune in programmazione, presente fin dai primi linguaggi di programmazione. In TypeScript, si può utilizzare il metodo `replace` con un'espressione regolare, come nel nostro esempio.

Un'alternativa può essere l'utilizzo del metodo `split` per dividere la stringa in parole, trasformare la prima lettera di ogni parola in maiuscolo con `toUpperCase()`, e poi rigenerare la stringa con `join`.

Dettagli di implementazione: ricorda che in JavaScript (e quindi TypeScript), le stringhe sono immutabili. Quindi ogni volta che "modifichi" una stringa, in realtà stai creando una nuova stringa.

```TypeScript
function capitalizeStrAlternative(input: string): string {
    return input.split(' ')
        .map(word => word[0].toUpperCase() + word.substr(1))
        .join(' ');
}

console.log(capitalizeStrAlternative('ciao mondo'));
```
Output:

```TypeScript
'Ciao Mondo'
```

## Per saperne di più

Per approfondire l'argomento, ecco alcuni link utili:

1. La documentazione MDN sulla funzione `replace()`: https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/replace
2. Dettagli sulla funzione `toUpperCase()`: https://developer.mozilla.org/it/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase
3. Vue Mastery: https://www.vuemastery.com/courses/advanced-components/build-a-reactivity-system/ (Inglese)