---
title:                "TypeScript: Maiuscolare una stringa"
simple_title:         "Maiuscolare una stringa"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/it/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Perché

Capitalizzare una stringa è un'operazione comune nel mondo della programmazione. Spesso, abbiamo bisogno di manipolare le stringhe per ottenere un output specifico o per renderle più leggibili. In questo articolo, vedremo come capitalizzare una stringa utilizzando TypeScript.

## Come fare

Per capitalizzare una stringa in TypeScript, possiamo utilizzare la funzione integrata `toUpperCase()`. Vediamo un esempio di come utilizzarla:

```TypeScript
let stringa = 'ciao a tutti!';
console.log(stringa.toUpperCase()); // Output: CIAO A TUTTI!
```

In questo esempio, abbiamo creato una variabile `stringa` contenente la stringa "ciao a tutti!". Successivamente, abbiamo utilizzato il metodo `toUpperCase()` per convertire la stringa in maiuscolo. Infine, abbiamo usato il comando `console.log()` per stampare il nuovo valore della stringa in maiuscolo.

Possiamo anche utilizzare la funzione `slice()` per dividere la stringa iniziale e convertire solo la prima lettera in maiuscolo. Vediamo un esempio:

```TypeScript
let stringa = 'programmazione';
let nuovaStringa = stringa.slice(0,1).toUpperCase() + stringa.slice(1);
console.log(nuovaStringa); // Output: Programmazione
```

In questo esempio, abbiamo utilizzato il metodo `slice()` per dividere la stringa iniziale in due parti: la prima lettera e il resto della stringa. Successivamente, abbiamo utilizzato il metodo `toUpperCase()` sulla prima lettera e poi unito le due parti per ottenere una nuova stringa con la prima lettera in maiuscolo.

## Approfondimento

Abbiamo visto due metodi diversi per capitalizzare una stringa, ma ci sono anche altre opzioni disponibili. Ad esempio, è possibile utilizzare la libreria `lodash` e il suo metodo `capitalize()` per capitalizzare una stringa in modo più efficiente.

Inoltre, dobbiamo fare attenzione alle impostazioni regionali quando si manipolano le stringhe in TypeScript. Ad esempio, il metodo `toUpperCase()` non funzionerà correttamente per le stringhe con caratteri speciali o per lingue diverse dall'inglese. In questi casi, è necessario utilizzare algoritmi più complessi per ottenere una corretta capitalizzazione.

## Vedi anche

- [Documentazione ufficiale di TypeScript](https://www.typescriptlang.org/docs/)
- [Guida di TypeScript nell'IU di Visual Studio Code](https://code.visualstudio.com/docs/typescript/typescript-tutorial)
- [Libreria Lodash](https://lodash.com/)